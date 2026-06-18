# ── Scheduled run ─────────────────────────────────────────────────────────────
# Runs daily (or on demand). Does two things:
#   1. Scrapes all PSP sessions from the current year, matches bills to VeKLEP,
#      processes new ones and refreshes metadata on existing ones.
#   2. Scrapes Komise RIA verdicts and writes them to the database.
#
# Monitoring:
#   - Errors in any step send an email via Resend to ADMIN_EMAIL
#   - A heartbeat timestamp is written to Supabase after every successful run
#   - Optional: pings healthchecks.io if HEALTHCHECKS_URL is set in .Renviron

script_dir <- if (Sys.getenv("SCHEDULED_ROOT") != "") {
  Sys.getenv("SCHEDULED_ROOT")
} else if (requireNamespace("rstudioapi", quietly = TRUE) &&
           rstudioapi::isAvailable()) {
  dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  getwd()
}
setwd(script_dir)
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Ensure data/ exists before any writes (needed on fresh CI runners)
dir.create(file.path(script_dir, "data"), showWarnings = FALSE, recursive = TRUE)

cat(format(Sys.time()), "Scheduled run started\n",
    file = file.path(script_dir, "data", "heartbeat.log"), append = TRUE)

library(httr2); library(rvest); library(xml2); library(pdftools)
library(stringr); library(dplyr); library(DBI); library(RPostgres)
library(jsonlite); library(officer); library(uuid)

source("R/01_fetch_agenda.R")
source("R/02_crawl_documents.R")
source("R/03_extract_llm.R")
source("R/04_database.R")
source("R/06_run_pipeline.R")   # defines process_bill() only — no auto-run
source("R/08_komise_ria.R")
source("R/10_alerts.R")

# ── Logging ───────────────────────────────────────────────────────────────────

log_file <- paste0("data/scheduled_", format(Sys.Date(), "%Y%m%d"), ".log")

log_msg <- function(msg, level = "INFO") {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [", level, "] ", msg)
  message(line)
  cat(line, "\n", file = log_file, append = TRUE)
}

# ── Error email via Resend ────────────────────────────────────────────────────
# Requires ADMIN_EMAIL in .Renviron — the address that receives error alerts.
# Uses the same RESEND_API_KEY and RESEND_FROM already set up for user alerts.

send_error_alert <- function(step, error_msg) {
  api_key     <- Sys.getenv("RESEND_API_KEY")
  admin_email <- Sys.getenv("ADMIN_EMAIL")
  from_addr   <- Sys.getenv("RESEND_FROM", "Sněmovna dnes <alerts@snemovnadnes.cz>")
  
  if (api_key == "" || admin_email == "") {
    log_msg("WARN: RESEND_API_KEY or ADMIN_EMAIL not set — skipping error email")
    return(invisible(FALSE))
  }
  
  html <- paste0(
    '<div style="font-family:monospace;max-width:600px;">',
    '<h2 style="color:#c0392b;">&#9888; Pipeline error: ', step, '</h2>',
    '<p><strong>Time:</strong> ', format(Sys.time()), '</p>',
    '<p><strong>Error:</strong></p>',
    '<pre style="background:#f5f5f5;padding:12px;border-left:3px solid #c0392b;">',
    gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", as.character(error_msg)))),
    '</pre>',
    '<p style="color:#7f8c8d;font-size:0.85rem;">',
    'Check log: data/scheduled_', format(Sys.Date(), "%Y%m%d"), '.log',
    '</p></div>'
  )
  
  tryCatch({
    request("https://api.resend.com/emails") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      req_body_json(list(
        from    = from_addr,
        to      = list(admin_email),
        subject = paste0("[Sněmovna dnes] Pipeline error — ", step,
                         " (", format(Sys.Date()), ")"),
        html    = html
      )) |>
      req_timeout(15) |>
      req_perform()
    log_msg(paste("Error alert sent to", admin_email))
    invisible(TRUE)
  }, error = function(e) {
    log_msg(paste("Failed to send error alert:", e$message), "WARN")
    invisible(FALSE)
  })
}

# ── Heartbeat: write last_run to Supabase ─────────────────────────────────────
# Creates a single-row 'heartbeat' table and updates it after every successful
# run. The Shiny app can display this as "last updated X minutes ago".
# Also queryable via the public API:
#   GET /rest/v1/heartbeat?select=last_run

update_heartbeat <- function(db, counts) {
  tryCatch({
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS heartbeat (
        id          INTEGER PRIMARY KEY DEFAULT 1,
        last_run    TIMESTAMPTZ NOT NULL,
        bills_success INTEGER,
        bills_skipped INTEGER,
        bills_error   INTEGER
      )
    ")
    dbExecute(db, "
      INSERT INTO heartbeat (id, last_run, bills_success, bills_skipped, bills_error)
      VALUES (1, now(), $1, $2, $3)
      ON CONFLICT (id) DO UPDATE
        SET last_run      = now(),
            bills_success = EXCLUDED.bills_success,
            bills_skipped = EXCLUDED.bills_skipped,
            bills_error   = EXCLUDED.bills_error
    ", params = list(
      as.integer(counts$success %||% 0L),
      as.integer(counts$skipped %||% 0L),
      as.integer(counts$error   %||% 0L)
    ))
    log_msg("Heartbeat updated in Supabase")
  }, error = function(e) {
    log_msg(paste("Heartbeat update failed:", e$message), "WARN")
  })
}

# ── Healthchecks.io ping ──────────────────────────────────────────────────────
# Optional. Sign up free at healthchecks.io, create a check with a 25h grace
# period, and add the ping URL to .Renviron as HEALTHCHECKS_URL.
# If no ping arrives within 25h, healthchecks.io emails you automatically.
# This covers the "machine didn't run at all" case that Supabase cannot detect.

ping_healthchecks <- function(success = TRUE) {
  url <- Sys.getenv("HEALTHCHECKS_URL")
  if (url == "") return(invisible(NULL))
  
  ping_url <- if (success) url else paste0(url, "/fail")
  
  tryCatch({
    request(ping_url) |>
      req_timeout(10) |>
      req_perform()
    log_msg(paste("Healthchecks.io pinged:", if (success) "success" else "FAIL"))
  }, error = function(e) {
    log_msg(paste("Healthchecks.io ping failed:", e$message), "WARN")
  })
}

# ── 1. Determine which sessions to scan ───────────────────────────────────────

get_sessions_to_scan <- function() {
  
  active <- tryCatch(
    get_current_session(),
    error = function(e) {
      log_msg(paste("Could not fetch active sessions:", e$message), "WARN")
      integer(0)
    }
  )
  
  if (length(active) == 0) {
    log_msg("Falling back to scanning sessions 1–30", "WARN")
    return(1:30)
  }
  
  max_s    <- max(active)
  window   <- seq(max(1, max_s - 10), max_s + 5)
  sessions <- unique(sort(c(active, window)))
  
  log_msg(paste("Sessions to scan:", paste(sessions, collapse = ", ")))
  sessions
}

# ── 2. Document quality rank ──────────────────────────────────────────────────

doc_rank <- function(type) {
  switch(type,
         ria_keyword    = 3L,
         ria_type       = 3L,
         RIA            = 3L,
         mp_zip         = 2L,
         prehled_dopadu = 2L,
         zd_fallback    = 1L,
         ma_fallback    = 1L,
         zadne          = 0L,
         0L
  )
}

# ── 3. Refresh a single bill ──────────────────────────────────────────────────

refresh_bill <- function(db, material, backend = "gemini") {
  
  pid <- material$pid
  
  stored <- dbGetQuery(db,
                       "SELECT veklep_modified FROM bills WHERE pid = $1",
                       params = list(pid)
  )
  stored_modified <- if (nrow(stored) > 0 && !is.na(stored$veklep_modified[1]))
    stored$veklep_modified[1]
  else
    ""
  
  meta <- tryCatch(get_material_metadata(pid), error = function(e) NULL)
  if (!is.null(meta)) material <- c(material, meta)
  tryCatch(
    save_bill(db, material),
    error = function(e) log_msg(paste("  save_bill failed:", e$message), "WARN")
  )
  
  current_modified <- material$veklep_modified %||% ""
  
  existing <- dbGetQuery(db,
                         "SELECT typ_dokumentu FROM impacts WHERE pid = $1 ORDER BY id DESC LIMIT 1",
                         params = list(pid)
  )
  
  if (nrow(existing) == 0) {
    log_msg(paste("  New bill — processing:", pid))
    return(process_bill(db, material, backend = backend))
  }
  
  prev_type <- existing$typ_dokumentu[1]
  prev_type <- if (is.na(prev_type)) "zadne" else prev_type
  
  if (doc_rank(prev_type) >= 3L) {
    log_msg(paste("  Already has RIA — metadata refreshed:", pid))
    return("skipped")
  }
  
  veklep_changed <- current_modified == "" ||
    stored_modified  == "" ||
    current_modified != stored_modified
  
  if (!veklep_changed) {
    log_msg(paste("  VeKLEP unchanged — skipping doc check:", pid))
    return("skipped")
  }
  
  log_msg(paste0("  VeKLEP modified (", stored_modified, " → ", current_modified,
                 ") — checking documents: ", pid))
  
  docs    <- tryCatch(get_material_documents(pid), error = function(e) NULL)
  ria_doc <- if (!is.null(docs)) find_ria_document(docs) else NULL
  
  if (is.null(ria_doc)) {
    log_msg(paste("  No document found after update — metadata refreshed:", pid))
    return("skipped")
  }
  
  if (doc_rank(ria_doc$type) > doc_rank(prev_type)) {
    log_msg(paste0("  Upgrade: ", prev_type, " → ", ria_doc$type,
                   " — re-processing: ", pid))
    dbExecute(db, "DELETE FROM impacts  WHERE pid = $1", params = list(pid))
    dbExecute(db, "DELETE FROM costs    WHERE pid = $1", params = list(pid))
    dbExecute(db, "DELETE FROM benefits WHERE pid = $1", params = list(pid))
    return(process_bill(db, material, backend = backend))
  }
  
  log_msg(paste0("  VeKLEP changed but no document upgrade (still ",
                 ria_doc$type, ") — metadata refreshed: ", pid))
  return("skipped")
}

# ── 4. Main pipeline ──────────────────────────────────────────────────────────

run_pipeline <- function(backend = "gemini") {
  
  log_msg("=== Pipeline started ===")
  
  sessions <- get_sessions_to_scan()
  
  bill_list <- lapply(sessions, function(s) {
    tryCatch(
      scrape_session_agenda(s),
      error = function(e) {
        log_msg(paste("Session", s, "failed:", e$message), "WARN")
        NULL
      }
    )
  })
  all_bills <- do.call(rbind, Filter(Negate(is.null), bill_list))
  
  if (is.null(all_bills) || nrow(all_bills) == 0) {
    log_msg("No bills found across scanned sessions", "WARN")
    return(invisible(list(success = 0L, skipped = 0L, error = 0L, no_document = 0L)))
  }
  log_msg(paste("Bills found:", nrow(all_bills), "across", length(sessions), "sessions"))
  
  matched <- tryCatch(
    match_tisk_to_veklep(all_bills),
    error = function(e) {
      log_msg(paste("VeKLEP matching failed:", e$message), "ERROR")
      NULL
    }
  )
  
  if (is.null(matched) || nrow(matched) == 0) {
    log_msg("No bills could be matched to VeKLEP PIDs", "WARN")
    return(invisible(list(success = 0L, skipped = 0L, error = 0L, no_document = 0L)))
  }
  log_msg(paste("Matched", nrow(matched), "unique bills to VeKLEP"))
  
  db <- get_db()
  on.exit(dbDisconnect(db), add = TRUE)
  init_db(db)
  
  counts <- list(success = 0L, skipped = 0L, error = 0L, no_document = 0L)
  
  for (i in seq_len(nrow(matched))) {
    
    material <- list(
      pid          = matched$veklep_pid[i],
      title        = matched$title[i],
      predkladatel = "",
      published    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      id_tisk      = matched$id_tisk[i],
      session      = matched$session[i]
    )
    
    log_msg(paste0("[", i, "/", nrow(matched), "] ",
                   "Tisk ", matched$id_tisk[i], " → ", matched$veklep_pid[i]))
    
    status <- tryCatch(
      refresh_bill(db, material, backend = backend),
      error = function(e) {
        log_msg(paste("  Unexpected error:", e$message), "ERROR")
        "error"
      }
    )
    
    counts[[status]] <- counts[[status]] + 1L
    if (status != "skipped") Sys.sleep(5)
  }
  
  log_msg(paste(
    "Results — Success:", counts$success,
    "| Skipped:", counts$skipped,
    "| No document:", counts$no_document,
    "| Errors:", counts$error
  ))
  
  invisible(counts)
}

# ── 5. Refresh metadata for all bills already in DB ──────────────────────────

refresh_all_metadata <- function(db) {
  
  all_pids <- dbGetQuery(db, "SELECT pid FROM bills ORDER BY pid")$pid
  log_msg(paste("Refreshing metadata for", length(all_pids), "bills in DB"))
  
  for (pid in all_pids) {
    meta <- tryCatch(
      get_material_metadata(pid),
      error = function(e) {
        log_msg(paste("  Metadata fetch error for", pid, ":", e$message), "WARN")
        NULL
      }
    )
    
    if (is.null(meta)) next
    
    tryCatch(
      dbExecute(db, "
        UPDATE bills
        SET status_id       = CASE WHEN $1 <> '' THEN $1 ELSE status_id END,
            status_name     = CASE WHEN $2 <> '' THEN $2 ELSE status_name END,
            description     = CASE WHEN $3 <> '' THEN $3 ELSE description END,
            government_date = CASE WHEN $4 <> '' THEN $4 ELSE government_date END
        WHERE pid = $5
      ", params = list(
        meta$status_id       %||% "",
        meta$status_name     %||% "",
        meta$description     %||% "",
        meta$government_date %||% "",
        pid
      )),
      error = function(e) log_msg(paste("  Update failed for", pid, ":", e$message), "WARN")
    )
    
    Sys.sleep(0.3)
  }
  
  log_msg("Metadata refresh complete")
}

# ── Run ───────────────────────────────────────────────────────────────────────

# Initialize so the heartbeat section always has a value, even if the
# pipeline block is skipped (e.g. interactive() == TRUE locally).
pipeline_counts <- list(success = 0L, skipped = 0L, error = 0L, no_document = 0L)

if (!interactive() || Sys.getenv("RUN_PIPELINE") == "1") {

  pipeline_counts <- tryCatch({
    run_pipeline(backend = "gemini")
  }, error = function(e) {
    log_msg(paste("FATAL: run_pipeline crashed:", e$message), "ERROR")
    send_error_alert("run_pipeline", e$message)
    list(success = 0L, skipped = 0L, error = 1L, no_document = 0L)
  })

  tryCatch({
    db_komise <- get_db()
    run_komise_scraper(
      years = c(as.integer(format(Sys.Date(), "%Y")),
                as.integer(format(Sys.Date(), "%Y")) - 1),
      db    = db_komise
    )
    dbDisconnect(db_komise)
  }, error = function(e) {
    log_msg(paste("Komise RIA scraper error:", e$message), "ERROR")
    send_error_alert("run_komise_scraper", e$message)
  })

  tryCatch({
    db_alerts <- get_db()
    check_and_fire_alerts(
      db      = db_alerts,
      app_url = Sys.getenv("APP_URL", "https://snemovnadnes.cz")
    )
    dbDisconnect(db_alerts)
  }, error = function(e) {
    log_msg(paste("Alerts error:", e$message), "ERROR")
    send_error_alert("check_and_fire_alerts", e$message)
  })

  # ── Heartbeat ───────────────────────────────────────────────────────────────
  db_hb <- tryCatch(get_db(), error = function(e) NULL)
  if (!is.null(db_hb)) {
    update_heartbeat(db_hb, pipeline_counts)
    dbDisconnect(db_hb)
  }

  ping_healthchecks(success = TRUE)
}

log_msg("=== Scheduled run complete ===")