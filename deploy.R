# ── deploy.R ──────────────────────────────────────────────────────────────────
# Run this from RStudio (not sourced automatically by anything).
# Place it in the project root: D:/snemovni dopady/snemovni-dopady/deploy.R
#
# Usage: open in RStudio, select all, Ctrl+Enter

library(rsconnect)

# ── Step 1: Verify account is configured ──────────────────────────────────────
# If this errors, run rsconnect::setAccountInfo(...) first (see shinyapps.io
# → Account → Tokens → Show → Copy to clipboard).
accounts <- rsconnect::accounts()
stopifnot("No shinyapps.io account configured — run rsconnect::setAccountInfo() first" =
            "dopady" %in% accounts$name)
cat("✓ Account found:", accounts[accounts$name == "dopady", "server"], "\n")

# ── Step 2: Define exactly which files to bundle ──────────────────────────────
# Only files the Shiny app actually needs at runtime.
# Pipeline scripts (01–09, 06, 07, 08, 09) are deliberately excluded so
# shinyapps.io doesn't auto-source them on startup.

app_files <- c(
  "app.R",
  "secrets.R",
  "R/04_database.R",
  "R/10_alerts.R"
)
# ── Step 3: Verify all files exist before deploying ───────────────────────────
missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing files — fix before deploying:\n  ", paste(missing, collapse = "\n  "))
}
cat("✓ All", length(app_files), "files present\n")

# ── Step 4: Double-check that pipeline scripts are NOT in the list ─────────────
pipeline_scripts <- c(
  "R/01_fetch_agenda.R", "R/02_crawl_documents.R", "R/03_extract_llm.R",
  "R/05_pipeline_testing.R", "R/06_run_pipeline.R", "R/07_scheduled_run.R",
  "R/08_komise_ria.R", "R/09_bulk_import.R"
)
accidentally_included <- intersect(app_files, pipeline_scripts)
if (length(accidentally_included) > 0) {
  stop("Pipeline scripts in app_files — remove them:\n  ",
       paste(accidentally_included, collapse = "\n  "))
}
cat("✓ No pipeline scripts in bundle\n")

# ── Step 5: Print the final file list for a manual sanity check ───────────────
cat("\nFiles that will be deployed:\n")
for (f in app_files) cat("  ", f, "\n")
cat("\nProceed? Edit this script to comment out the stop() below, then re-run.\n")
# stop("Safety stop — comment out this line when you're happy with the file list above.")

# ── Step 6: Deploy ────────────────────────────────────────────────────────────
rsconnect::deployApp(
  appDir      = ".",
  appName     = "snemovnadnes",
  account     = "dopady",
  server      = "shinyapps.io",   # ← add this line
  appFiles    = app_files,
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("\n✓ Deploy complete — verify at https://snemovnadnes.cz\n")
