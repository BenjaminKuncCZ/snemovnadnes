# ── One-off, self-limiting re-extraction of old bills ─────────────────────────
#
# Re-runs the LLM extraction (recommended-variant logic, see 03_extract_llm.R)
# for
#   (a) every bill whose latest extraction is in the OLD format, i.e. its
#       impacts.poznamky does not start with "Vybraná varianta:", and
#   (b) every bill that has NO extraction at all (earlier errors; such bills
#       outside the daily pipeline's session window were never retried).
#
# Run by .github/workflows/reextract-old-bills.yml once a day. Each run handles
# at most REEXTRACT_BATCH bills (default 40) and stops starting new bills after
# REEXTRACT_MAX_MINUTES (default 75). When nothing is left to do it writes
# data/reextract_done.flag; the workflow then disables itself.
#
# Safety properties
#   * Independent of the daily pipeline's session window — works on any bill
#     that already has a row in `impacts`.
#   * Old data is replaced only AFTER a new extraction succeeded, in one
#     database transaction. A failure leaves the old extraction untouched.
#   * The ORIGINAL impacts.created_at is kept, so alert e-mails (which fire on
#     impacts.created_at) are not triggered for old bills.
#   * If the old extraction had items and the new one has none, the old data is
#     kept (status "kept_old") — protects against a bad or empty model answer.
#   * Progress lives in the table `reextract_log` (created automatically):
#       done      new extraction stored
#       kept_old  old extraction kept (no document / new result was empty)
#       failed    error; retried on later runs, at most 3 attempts in total
#     To retry failed bills later:
#       UPDATE reextract_log SET attempts = 0 WHERE status = 'failed';
#   * Bills with "Nebyl nalezen vhodný dokument." are left to the daily pipeline.
#   * A bill of kind (b) that has no suitable document gets the same
#     "Nebyl nalezen vhodný dokument." placeholder row the pipeline writes.
#   * Why a bill failed is stored in reextract_log.note, e.g.
#       SELECT pid, status, attempts, note FROM reextract_log WHERE status = 'failed';
#
# Environment: SUPABASE_*, GEMINI_API_KEY, LLM_BACKEND (default "gemini"),
#   REEXTRACT_BATCH, REEXTRACT_MAX_MINUTES,
#   REEXTRACT_DRY_RUN=true  -> only list what would be processed.

if (Sys.getenv("SCHEDULED_ROOT") != "") setwd(Sys.getenv("SCHEDULED_ROOT"))
if (file.exists(".Renviron")) readRenviron(".Renviron")
dir.create("data", showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(httr2); library(rvest); library(xml2); library(pdftools)
  library(stringr); library(dplyr); library(DBI); library(RPostgres)
  library(jsonlite); library(officer)
})

source("R/02_crawl_documents.R")
source("R/03_extract_llm.R")
source("R/04_database.R")

REEXTRACT_MAX_ATTEMPTS <- 3L

# When this file is sourced by the daily pipeline (07_scheduled_run.R), that
# script's own log_msg()/log_file are used.
if (!exists("log_msg", mode = "function")) {
  log_file <- paste0("data/reextract_", format(Sys.Date(), "%Y%m%d"), ".log")
  log_msg <- function(msg, level = "INFO") {
    line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [", level, "] ", msg)
    message(line)
    cat(line, "\n", file = log_file, append = TRUE)
  }
}

# ── Progress table ────────────────────────────────────────────────────────────

ensure_log_table <- function(db) {
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS reextract_log (
      pid          TEXT PRIMARY KEY,
      status       TEXT NOT NULL,
      attempts     INTEGER NOT NULL DEFAULT 0,
      last_attempt TEXT,
      note         TEXT
    )
  ")
}

# Bills still to do: latest extraction is in the old format, not yet handled.
.pending_from <- "
  FROM bills b
  LEFT JOIN (SELECT DISTINCT ON (pid) pid, poznamky FROM impacts ORDER BY pid, id DESC) i ON i.pid = b.pid
  LEFT JOIN reextract_log r ON r.pid = b.pid
  WHERE (i.pid IS NULL
         OR (COALESCE(i.poznamky, '') NOT LIKE 'Vybraná varianta:%'
             AND COALESCE(i.poznamky, '') <> 'Nebyl nalezen vhodný dokument.'))
    AND (r.pid IS NULL OR (r.status = 'failed' AND r.attempts < $1))
"

count_pending <- function(db) {
  as.integer(dbGetQuery(db, paste("SELECT count(*)::int AS n", .pending_from),
                        params = list(REEXTRACT_MAX_ATTEMPTS))$n)
}

get_pending <- function(db, n) {
  dbGetQuery(db, paste("SELECT b.pid", .pending_from, "ORDER BY b.pid LIMIT $2"),
             params = list(REEXTRACT_MAX_ATTEMPTS, as.integer(n)))$pid
}

record_status <- function(db, pid, status, note = "") {
  dbExecute(db, "
    INSERT INTO reextract_log (pid, status, attempts, last_attempt, note)
    VALUES ($1, $2, 1, $3, $4)
    ON CONFLICT (pid) DO UPDATE
      SET status       = EXCLUDED.status,
          attempts     = reextract_log.attempts + 1,
          last_attempt = EXCLUDED.last_attempt,
          note         = EXCLUDED.note
  ", params = list(pid, status, format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), substr(note, 1, 500)))
}

# ── Extraction for one bill (same steps as process_bill() in 06_run_pipeline.R) ─
# Returns list(status = "ok"|"no_document"|"failed", result, doc_path, note)

extract_for_pid <- function(pid, backend = Sys.getenv("LLM_BACKEND", "gemini")) {

  fail <- function(note, status = "failed") list(status = status, note = note)

  docs_err <- NULL
  docs <- tryCatch(get_material_documents(pid), error = function(e) { docs_err <<- e$message; NULL })
  if (is.null(docs))
    return(fail(paste0("VeKLEP JSON nedostupný",
                       if (!is.null(docs_err)) paste0(": ", docs_err) else " (chyba spojení nebo časový limit)")))

  ria_doc <- find_ria_document(docs)
  if (is.null(ria_doc)) return(fail("žádný vhodný dokument", status = "no_document"))

  dl_err <- NULL
  downloaded <- tryCatch(download_document(ria_doc), error = function(e) { dl_err <<- e$message; NULL })
  if (is.null(downloaded))
    return(fail(paste0("stažení dokumentu selhalo: ", ria_doc$name,
                       if (!is.null(dl_err)) paste0(" (", dl_err, ")") else "")))

  rd_err <- NULL
  extracted <- tryCatch(extract_document_text(downloaded$path), error = function(e) { rd_err <<- e$message; NULL })
  if (is.null(extracted))
    return(fail(paste0("čtení textu selhalo: ", basename(downloaded$path),
                       if (!is.null(rd_err)) paste0(" (", rd_err, ")") else "")))

  text_for_llm <- tryCatch(
    extract_section_by_heading(extracted$ria_section, max_chars = 60000),
    error = function(e) trim_to_context(extracted$ria_section, max_chars = 30000)
  )
  if (is.null(text_for_llm) || nchar(text_for_llm) < 50) return(fail("text je příliš krátký"))

  llm_err <- NULL
  result <- tryCatch(
    extract_impacts(text_for_llm, backend = backend, doc_type = downloaded$type),
    error = function(e) { llm_err <<- e$message; log_msg(paste("LLM error:", e$message), "WARN"); NULL }
  )
  if (is.null(result))
    return(fail(if (!is.null(llm_err)) paste0("Gemini: ", substr(llm_err, 1, 300))
                else "odpověď modelu není validní JSON"))

  # as in 06_run_pipeline.R, step 8b
  if (downloaded$type %in% c("zd_fallback", "ma_fallback")) {
    if (!is.null(result$typ_dokumentu) && result$typ_dokumentu == "RIA")
      result$typ_dokumentu <- "prehled_dopadu"
    if (!identical(result$typ_dokumentu, "RIA")) result$ria_provedena <- FALSE
  }

  list(status = "ok", result = result, doc_path = downloaded$path, note = "")
}

# ── Replace the old extraction (one transaction, keeps original created_at) ──

has_impact_row <- function(db, pid) {
  nrow(dbGetQuery(db, "SELECT 1 FROM impacts WHERE pid = $1 LIMIT 1", params = list(pid))) > 0
}

count_items <- function(db, pid) {
  as.integer(dbGetQuery(db, "
    SELECT ((SELECT count(*) FROM costs    WHERE pid = $1) +
            (SELECT count(*) FROM benefits WHERE pid = $1))::int AS n", params = list(pid))$n)
}

replace_impact <- function(db, pid, result, doc_path) {
  dbWithTransaction(db, {
    new_id <- save_impact(db, pid, result, doc_path)
    # keep the original timestamp -> no alert e-mails for old bills
    dbExecute(db, "
      UPDATE impacts SET created_at =
        (SELECT min(created_at) FROM impacts WHERE pid = $1 AND id < $2)
      WHERE id = $2 AND EXISTS (SELECT 1 FROM impacts WHERE pid = $1 AND id < $2)
    ", params = list(pid, new_id))
    dbExecute(db, "DELETE FROM costs    WHERE pid = $1 AND impact_id IS DISTINCT FROM $2", params = list(pid, new_id))
    dbExecute(db, "DELETE FROM benefits WHERE pid = $1 AND impact_id IS DISTINCT FROM $2", params = list(pid, new_id))
    dbExecute(db, "DELETE FROM impacts  WHERE pid = $1 AND id <> $2",                     params = list(pid, new_id))
    new_id
  })
}

# ── Process one bill end-to-end; returns the status written to reextract_log ──

reextract_one <- function(db, pid) {

  out <- tryCatch(extract_for_pid(pid), error = function(e) list(status = "failed", note = e$message))

  if (out$status == "no_document") {
    if (!has_impact_row(db, pid)) {
      # same placeholder row the daily pipeline writes for bills without a document
      saved <- tryCatch({
        save_impact(db, pid, list(ria_provedena = FALSE, typ_dokumentu = "zadne", shrnutí = "",
                                  naklady = list(), prinosy = list(),
                                  poznamky = "Nebyl nalezen vhodný dokument."), "")
        TRUE
      }, error = function(e) FALSE)
      record_status(db, pid, if (saved) "done" else "failed",
                    if (saved) "žádný vhodný dokument — uložen záznam 'bez hodnocení'" else "uložení záznamu selhalo")
      return(if (saved) "done" else "failed")
    }
    record_status(db, pid, "kept_old", out$note)
    return("kept_old")
  }
  if (out$status != "ok") {
    record_status(db, pid, "failed", out$note)
    return("failed")
  }

  result    <- out$result
  new_items <- length(result$naklady) + length(result$prinosy)
  old_items <- count_items(db, pid)

  if (old_items > 0 && new_items == 0) {
    record_status(db, pid, "kept_old",
                  paste0("nový výsledek je prázdný (", result$typ_dokumentu %||% "?",
                         "), původní měl ", old_items, " položek"))
    return("kept_old")
  }

  saved <- tryCatch({ replace_impact(db, pid, result, out$doc_path); TRUE },
                    error = function(e) { log_msg(paste("DB error:", e$message), "ERROR"); FALSE })
  if (!saved) {
    record_status(db, pid, "failed", "uložení do databáze selhalo (původní data zachována)")
    return("failed")
  }

  record_status(db, pid, "done", paste0(result$typ_dokumentu %||% "?", ", položek: ", new_items))
  "done"
}

# ── Daily safety net for bills without any extraction ─────────────────────────
# Called by the daily pipeline (07_scheduled_run.R). A bill that errored once and
# lies outside the scanned session window would otherwise never be retried.
# `exclude` = bills the pipeline has just attempted itself in this run.
# Uses the same reextract_log (max. 3 attempts per bill, reason stored in note).

get_stuck_bills <- function(db) {
  dbGetQuery(db, "
    SELECT b.pid
    FROM bills b
    LEFT JOIN reextract_log r ON r.pid = b.pid
    WHERE NOT EXISTS (SELECT 1 FROM impacts i WHERE i.pid = b.pid)
      AND (r.pid IS NULL OR (r.status = 'failed' AND r.attempts < $1))
    ORDER BY b.pid
    LIMIT 500
  ", params = list(REEXTRACT_MAX_ATTEMPTS))$pid
}

retry_stuck_bills <- function(db, max_n = 10, exclude = character(), pause = 5) {
  ensure_log_table(db)
  pids <- head(setdiff(get_stuck_bills(db), exclude), max_n)
  counts <- c(done = 0L, kept_old = 0L, failed = 0L)
  if (length(pids) == 0) {
    log_msg("Retry: no bills without extraction.")
    return(invisible(counts))
  }
  log_msg(paste("Retry: bills without extraction:", paste(pids, collapse = ", ")))
  consecutive_failures <- 0L
  for (pid in pids) {
    status <- tryCatch(reextract_one(db, pid), error = function(e) {
      log_msg(paste("Retry: unexpected error for", pid, ":", e$message), "ERROR"); "failed"
    })
    counts[[status]] <- counts[[status]] + 1L
    log_msg(paste("Retry:", pid, "->", status))
    consecutive_failures <- if (status == "failed") consecutive_failures + 1L else 0L
    if (consecutive_failures >= 5L) {
      log_msg("Retry: 5 failures in a row — stopping.", "WARN")
      break
    }
    Sys.sleep(pause)
  }
  invisible(counts)
}

# ── Main ──────────────────────────────────────────────────────────────────────

reextract_main <- function() {

  batch   <- as.integer(Sys.getenv("REEXTRACT_BATCH", "40"))
  max_min <- as.numeric(Sys.getenv("REEXTRACT_MAX_MINUTES", "75"))
  dry_run <- tolower(Sys.getenv("REEXTRACT_DRY_RUN", "false")) == "true"

  db <- get_db()
  on.exit(try(dbDisconnect(db), silent = TRUE), add = TRUE)
  ensure_log_table(db)

  log_msg(paste0("=== Re-extraction started (batch ", batch, ", limit ", max_min,
                 " min", if (dry_run) ", DRY RUN" else "", ") ==="))

  total <- count_pending(db)
  log_msg(paste("Bills still to re-extract:", total))

  mark_done <- function() {
    if (dry_run) return(invisible())
    file.create("data/reextract_done.flag")
    log_msg("All old bills re-extracted — this job has nothing left to do and will be disabled.")
    failed <- dbGetQuery(db, "SELECT pid, note FROM reextract_log WHERE status = 'failed'")
    if (nrow(failed) > 0)
      log_msg(paste0(nrow(failed), " bills failed ", REEXTRACT_MAX_ATTEMPTS, "x and keep their old data: ",
                     paste(failed$pid, collapse = ", "),
                     ". Retry later with: UPDATE reextract_log SET attempts = 0 WHERE status = 'failed';"),
              "WARN")
  }

  if (total == 0) { mark_done(); return(invisible("done")) }

  pids <- get_pending(db, batch)
  if (dry_run) {
    log_msg(paste("Would process now:", paste(pids, collapse = ", ")))
    return(invisible("dry_run"))
  }

  started <- Sys.time()
  counts <- c(done = 0L, kept_old = 0L, failed = 0L)
  consecutive_failures <- 0L

  for (pid in pids) {
    if (as.numeric(difftime(Sys.time(), started, units = "mins")) > max_min) {
      log_msg("Time limit reached — the rest continues in the next run.", "WARN")
      break
    }
    log_msg(paste("Re-extracting:", pid))
    status <- tryCatch(reextract_one(db, pid), error = function(e) {
      log_msg(paste("Unexpected error for", pid, ":", e$message), "ERROR"); "failed"
    })
    counts[[status]] <- counts[[status]] + 1L
    log_msg(paste("  ->", status))

    consecutive_failures <- if (status == "failed") consecutive_failures + 1L else 0L
    if (consecutive_failures >= 5L) {
      log_msg("5 failures in a row (Gemini key / quota / network?) — aborting this run.", "ERROR")
      quit(status = 1)
    }
    Sys.sleep(2)
  }

  remaining <- count_pending(db)
  log_msg(paste0("Run finished: done ", counts[["done"]], ", kept_old ", counts[["kept_old"]],
                 ", failed ", counts[["failed"]], " | remaining: ", remaining))
  if (remaining == 0) mark_done()
  invisible("ok")
}

# Run only when executed with `Rscript R/11_reextract_old.R` (not when source()d)
if (!interactive() && sys.nframe() == 0) reextract_main()
