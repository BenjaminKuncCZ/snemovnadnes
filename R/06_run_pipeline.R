library(httr2)
library(rvest)
library(xml2)
library(pdftools)
library(stringr)
library(dplyr)
library(DBI)
library(RSQLite)
library(jsonlite)
library(officer)

source("R/02_crawl_documents.R")
source("R/03_extract_llm.R")
source("R/04_database.R")

# ── Logging ───────────────────────────────────────────────────────────────────

log_file <- paste0("data/pipeline_", format(Sys.Date(), "%Y%m%d"), ".log")

log_msg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] [", level, "] ", msg)
  message(line)
  cat(line, "\n", file = log_file, append = TRUE)
}

# ── Process a single bill (full pipeline: download → extract → save) ──────────

process_bill <- function(db, material, backend = Sys.getenv("LLM_BACKEND", "ollama")) {
  
  pid <- material$pid
  log_msg(paste("Processing:", pid, "|", str_trunc(material$title, 60)))
  
  # Step 1: Check if already processed
  existing <- dbGetQuery(db,
                         "SELECT pid FROM impacts WHERE pid = $1",
                         params = list(pid)
  )
  if (nrow(existing) > 0) {
    meta <- get_material_metadata(pid)
    if (!is.null(meta)) {
      material <- c(material, meta)
      save_bill(db, material)
    } else {
      log_msg(paste("Metadata unavailable — status not updated for:", pid), "WARN")
    }
    log_msg(paste("Already processed, skipping:", pid))
    return("skipped")
  }
  
  # Step 2: Fetch and save VeKLEP metadata
  meta <- tryCatch(
    get_material_metadata(pid),
    error = function(e) { log_msg(paste("Metadata fetch failed:", e$message), "WARN"); NULL }
  )
  if (!is.null(meta)) material <- c(material, meta)
  tryCatch(
    save_bill(db, material),
    error = function(e) log_msg(paste("Failed to save bill:", e$message), "WARN")
  )
  
  # Step 3: Get document list
  docs <- tryCatch(
    get_material_documents(pid),
    error = function(e) { log_msg(paste("Failed to get documents:", e$message), "ERROR"); NULL }
  )
  if (is.null(docs)) return("error")
  
  # Step 4: Find best available document
  ria_doc <- find_ria_document(docs)
  if (is.null(ria_doc)) {
    log_msg(paste("No suitable document found for:", pid), "WARN")
    save_impact(db, pid, list(
      ria_provedena = FALSE, typ_dokumentu = "zadne",
      shrnutí = "", naklady = list(), prinosy = list(),
      poznamky = "Nebyl nalezen vhodný dokument."
    ), "")
    return("no_document")
  }
  
  # Step 5: Download
  downloaded <- tryCatch(
    download_document(ria_doc),
    error = function(e) { log_msg(paste("Download failed:", e$message), "ERROR"); NULL }
  )
  if (is.null(downloaded)) return("error")
  
  # Step 6: Extract text
  extracted <- tryCatch(
    extract_document_text(downloaded$path),
    error = function(e) { log_msg(paste("Text extraction failed:", e$message), "ERROR"); NULL }
  )
  if (is.null(extracted)) return("error")
  
  # Step 7: Prepare text for LLM
  text_for_llm <- tryCatch(
    extract_section_by_heading(extracted$ria_section, max_chars = 10000),
    error = function(e) {
      log_msg(paste("Section extraction failed, using trim:", e$message), "WARN")
      trim_to_context(extracted$ria_section, max_chars = 4000)
    }
  )
  if (is.null(text_for_llm) || nchar(text_for_llm) < 50) {
    log_msg(paste("Text too short to analyse:", pid), "WARN")
    return("error")
  }
  
  # Step 8: LLM extraction
  result <- tryCatch(
    extract_impacts(text_for_llm, backend = backend, doc_type = downloaded$type),
    error = function(e) { log_msg(paste("LLM extraction failed:", e$message), "ERROR"); NULL }
  )
  if (is.null(result)) return("error")
  
  # Step 8b: Downgrade over-optimistic RIA detection for fallback documents
  if (downloaded$type %in% c("zd_fallback", "ma_fallback")) {
    if (!is.null(result$typ_dokumentu) && result$typ_dokumentu == "RIA") {
      result$typ_dokumentu <- "prehled_dopadu"
      log_msg(paste("Downgraded RIA → prehled_dopadu for fallback doc:", pid), "WARN")
    }
    if (result$typ_dokumentu != "RIA") result$ria_provedena <- FALSE
  }
  
  # Step 9: Save
  save_impact(db, pid, result, downloaded$path)
  return("success")
}

# NOTE: auto-execution removed — call process_bill() from 07_scheduled_run.R