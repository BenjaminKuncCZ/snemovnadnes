# ── Scheduled run ─────────────────────────────────────────────────────────────
# Runs daily (or on demand). Does two things:
#   1. Scrapes all PSP sessions from the current year, matches bills to VeKLEP,
#      processes new ones and refreshes metadata on existing ones.
#   2. Scrapes Komise RIA verdicts and writes them to the database.

script_dir <- if (Sys.getenv("SCHEDULED_ROOT") != "") {
  Sys.getenv("SCHEDULED_ROOT")
} else {
  dirname(rstudioapi::getActiveDocumentContext()$path)
}
setwd(script_dir)
if (file.exists(".Renviron")) readRenviron(".Renviron")

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

# ── 1. Determine which sessions to scan ───────────────────────────────────────
#
# Takes the highest active session number and scans a window of (max-10) to
# (max+5). This reliably covers all sessions from the current year, including
# ones that have concluded and no longer appear as "active" on the PSP page.

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
#
# Hierarchy (higher number = better document):
#   0  zadne          — nothing found
#   1  zd/ma fallback — last-resort document, not a real RIA
#   2  prehled_dopadu — summary / důvodová zpráva / příloha
#   3  RIA            — proper Závěrečná zpráva z hodnocení dopadů regulace

doc_rank <- function(type) {
  switch(type,
         ria_keyword    = 3L,
         ria_type       = 3L,
         RIA            = 3L,   # stored typ_dokumentu value
         mp_zip         = 2L,
         prehled_dopadu = 2L,   # stored typ_dokumentu value
         zd_fallback    = 1L,
         ma_fallback    = 1L,
         zadne          = 0L,
         0L
  )
}

# ── 3. Refresh a single bill ──────────────────────────────────────────────────
#
# Called for every bill found in scanned sessions. Logic:
#
#   a) Read the veklep_modified date currently stored in the DB (before update).
#   b) Fetch the latest VeKLEP metadata and write it to the bills table.
#      This always happens — status, description, government_date are kept fresh.
#   c) If the bill has never been processed → run the full extraction pipeline.
#   d) If the bill already has a proper RIA (rank 3) → nothing to upgrade; skip.
#   e) If veklep_modified has NOT changed since the last run → skip doc check.
#   f) If veklep_modified HAS changed (or was never stored) → fetch the current
#      document list and re-process if a better document is now available.

refresh_bill <- function(db, material, backend = "gemini") {
  
  pid <- material$pid
  
  # (a) Read the stored last-modified date BEFORE we overwrite it
  stored <- dbGetQuery(db,
                       "SELECT veklep_modified FROM bills WHERE pid = $1",
                       params = list(pid)
  )
  stored_modified <- if (nrow(stored) > 0 && !is.na(stored$veklep_modified[1]))
    stored$veklep_modified[1]
  else
    ""
  
  # (b) Fetch latest VeKLEP metadata and persist it (updates status, dates, etc.)
  meta <- tryCatch(get_material_metadata(pid), error = function(e) NULL)
  if (!is.null(meta)) material <- c(material, meta)
  tryCatch(
    save_bill(db, material),
    error = function(e) log_msg(paste("  save_bill failed:", e$message), "WARN")
  )
  
  current_modified <- material$veklep_modified %||% ""
  
  # (c) Never processed → run the full pipeline regardless of modification date
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
  
  # (d) Already has a proper RIA — top of the hierarchy, nothing to upgrade
  if (doc_rank(prev_type) >= 3L) {
    log_msg(paste("  Already has RIA — metadata refreshed:", pid))
    return("skipped")
  }
  
  # (e) VeKLEP modification date unchanged → skip expensive document check
  veklep_changed <- current_modified == "" ||        # field not available — be safe
    stored_modified   == "" ||        # never stored before
    current_modified  != stored_modified
  
  if (!veklep_changed) {
    log_msg(paste("  VeKLEP unchanged — skipping doc check:", pid))
    return("skipped")
  }
  
  # (f) VeKLEP has been modified — check whether a better document is now present
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
  
  # --- Collect bills from all relevant sessions ---
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
    return(invisible(NULL))
  }
  log_msg(paste("Bills found:", nrow(all_bills),
                "across", length(sessions), "sessions"))
  
  # --- Match each PSP tisk number to a VeKLEP PID ---
  matched <- tryCatch(
    match_tisk_to_veklep(all_bills),
    error = function(e) {
      log_msg(paste("VeKLEP matching failed:", e$message), "ERROR")
      NULL
    }
  )
  
  if (is.null(matched) || nrow(matched) == 0) {
    log_msg("No bills could be matched to VeKLEP PIDs", "WARN")
    return(invisible(NULL))
  }
  log_msg(paste("Matched", nrow(matched), "unique bills to VeKLEP"))
  
  # --- Refresh / process each matched bill ---
  db <- get_db()
  on.exit(dbDisconnect(db), add = TRUE)
  init_db(db)
  
  counts <- list(success = 0, skipped = 0, error = 0, no_document = 0)
  
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
    
    counts[[status]] <- counts[[status]] + 1
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
# Bills that drop off the active PSP agenda are never touched by the session
# scan above, so their status_name/description go stale. This loop fixes that.

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
    
    material <- c(list(pid = pid), meta)
    
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
    
    Sys.sleep(0.3)  # be polite to the API
  }
  
  log_msg("Metadata refresh complete")
}

# ── Run ───────────────────────────────────────────────────────────────────────

run_pipeline(backend = "gemini")

# Komise RIA — uses its own connection
db_komise <- get_db()
run_komise_scraper(
  years = c(as.integer(format(Sys.Date(), "%Y")),
            as.integer(format(Sys.Date(), "%Y")) - 1),
  db    = db_komise
)
dbDisconnect(db_komise)

# Alerts — check for new matches and fire notification emails
db_alerts <- get_db()
check_and_fire_alerts(
  db      = db_alerts,
  app_url = Sys.getenv("APP_URL", "https://snemovnadnes.cz")
)
dbDisconnect(db_alerts)