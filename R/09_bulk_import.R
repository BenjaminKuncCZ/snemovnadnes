library(httr2)
library(stringr)
library(dplyr)

source("R/01_fetch_agenda.R")
source("R/02_crawl_documents.R")
source("R/03_extract_llm.R")
source("R/04_database.R")
source("R/06_run_pipeline.R")

# ── Download and parse tisky.unl ──────────────────────────────────────────────

fetch_all_tisky <- function(from_date = as.Date("2026-01-01")) {
  
  message("Downloading tisky.zip...")
  tmp <- tempfile(fileext = ".zip")
  
  request("https://www.psp.cz/eknih/cdrom/opendata/tisky.zip") |>
    req_timeout(300) |>
    req_perform(path = tmp)
  
  extract_dir <- tempdir()
  unzip(tmp, exdir = extract_dir, overwrite = TRUE)
  
  lines <- readLines(file.path(extract_dir, "tisky.unl"),
                     encoding = "windows-1250")
  lines <- iconv(lines, from = "windows-1250", to = "UTF-8")
  lines <- lines[nchar(trimws(lines)) > 0]   # drop blank lines
  
  message("Total lines in tisky.unl: ", length(lines))
  
  # ── Diagnose raw structure on first line ──────────────────────────────────
  first_parts <- str_split(lines[1], "\\|")[[1]]
  message("\n--- RAW FIRST LINE ---")
  message(lines[1])
  message("\n--- FIELDS (", length(first_parts), " total) ---")
  for (j in seq_along(first_parts)) {
    message("  [", j, "] '", first_parts[j], "'")
  }
  
  # ── Parse all lines using detected field count ────────────────────────────
  n_fields <- length(first_parts)
  
  df <- do.call(rbind, lapply(lines, function(l) {
    parts <- str_split(l, "\\|")[[1]]
    if (length(parts) < 11) return(NULL)
    data.frame(
      id_tisk   = str_trim(parts[1]),
      id_org    = str_trim(parts[2]),
      navrh_dat = str_trim(parts[7]),
      uplny_naz = str_trim(parts[11]),
      stringsAsFactors = FALSE
    )
  }))
  
  message("\n--- PARSE SUMMARY ---")
  message("Rows parsed: ", nrow(df))
  
  # Show all distinct id_org values so we can confirm which is 10th parliament
  orgs <- sort(unique(df$id_org))
  message("Distinct id_org values (", length(orgs), " total): ",
          paste(tail(orgs, 20), collapse = ", "),   # last 20 = most recent
          if (length(orgs) > 20) " [truncated]" else "")
  
  # Show sample date values to confirm format
  message("Sample navrh_dat values: ",
          paste(head(df$navrh_dat[nchar(df$navrh_dat) > 0], 8), collapse = ", "))
  
  # ── Filter to 10th parliament (id_org "174") ──────────────────────────────
  df_10 <- df[df$id_org == "174", ]
  message("\n--- 10TH PARLIAMENT (id_org = '174') ---")
  message("Rows: ", nrow(df_10))
  
  if (nrow(df_10) == 0) {
    message("WARNING: No rows matched id_org = '174'.")
    message("Check the distinct id_org list above and update the filter.")
    return(df_10)
  }
  
  # ── Parse dates — try three common formats ────────────────────────────────
  df_10$date <- as.Date(df_10$navrh_dat, format = "%d.%m.%Y")
  
  if (all(is.na(df_10$date))) {
    message("Format '%d.%m.%Y' failed — trying '%Y%m%d'...")
    df_10$date <- as.Date(df_10$navrh_dat, format = "%Y%m%d")
  }
  
  if (all(is.na(df_10$date))) {
    message("Format '%Y%m%d' failed — trying '%Y-%m-%d'...")
    df_10$date <- as.Date(df_10$navrh_dat, format = "%Y-%m-%d")
  }
  
  message("Dates parsed: ", sum(!is.na(df_10$date)), " / ", nrow(df_10))
  if (any(!is.na(df_10$date))) {
    message("Date range: ",
            min(df_10$date, na.rm = TRUE), " — ",
            max(df_10$date, na.rm = TRUE))
  }
  
  # ── Final filter ──────────────────────────────────────────────────────────
  result <- df_10[!is.na(df_10$date) & df_10$date >= from_date, ]
  message("\nBills from ", from_date, " onwards: ", nrow(result))
  result
}

# ── Main ──────────────────────────────────────────────────────────────────────

db <- get_db()
init_db(db)
already_done <- dbGetQuery(db, "SELECT pid FROM impacts")$pid

tisky_2026 <- fetch_all_tisky(from_date = as.Date("2026-01-01"))

if (nrow(tisky_2026) == 0) {
  message("\nNo bills found — see diagnostic output above to fix the filter.")
  dbDisconnect(db)
  stop("Nothing to process.")
}

results <- list(success = 0, skipped = 0, error = 0, no_document = 0)

for (i in seq_len(nrow(tisky_2026))) {
  id_tisk <- tisky_2026$id_tisk[i]
  title   <- tisky_2026$uplny_naz[i]
  
  message("\n[", i, "/", nrow(tisky_2026), "] Tisk ", id_tisk, " — ",
          str_trunc(title, 60))
  
  pid <- tryCatch(
    get_veklep_pid_for_tisk(id_tisk),
    error = function(e) NULL
  )
  
  if (is.null(pid)) {
    message("  No VeKLEP PID found — skipping")
    Sys.sleep(0.5)
    next
  }
  
  if (pid %in% already_done) {
    message("  Already processed: ", pid)
    results$skipped <- results$skipped + 1
    next
  }
  
  material <- list(
    pid          = pid,
    title        = title,
    predkladatel = "",
    published    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    id_tisk      = id_tisk,
    session      = NA_integer_
  )
  
  status <- tryCatch(
    process_bill(db, material, backend = "gemini"),
    error = function(e) { message("  Error: ", e$message); "error" }
  )
  
  results[[status]] <- results[[status]] + 1
  already_done <- c(already_done, pid)
  
  if (status != "skipped") Sys.sleep(3)
  
  if (i %% 20 == 0)
    message("--- progress: success=", results$success,
            " errors=",  results$error, " ---")
}

dbDisconnect(db)
message("Done — success=",  results$success,
        " skipped=",        results$skipped,
        " no_doc=",         results$no_document,
        " errors=",         results$error)