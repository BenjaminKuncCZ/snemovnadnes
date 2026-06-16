# ── deploy.R ──────────────────────────────────────────────────────────────────
# Run this script from the project root to deploy the Shiny app to shinyapps.io
# Location: D:/snemovni dopady/snemovni-dopady/deploy.R
#
# The Shiny app only needs:
#   app.R             — UI + server
#   R/04_database.R   — DB connection + read functions
#   R/10_alerts.R     — alert CRUD + email sending
#
# Pipeline scripts (01–09) run locally via Task Scheduler and write to Supabase.
# They must NOT be deployed — shinyapps.io has no Ollama, no local file system,
# and deploying 05_pipeline_testing.R caused the previous crash.

library(rsconnect)

# ── 1. Set working directory to project root ───────────────────────────────────
project_root <- "D:/snemovni dopady/snemovni-dopady"
setwd(project_root)

# ── 2. Whitelist exactly the files the Shiny app needs ────────────────────────
app_files <- c(
  "app.R",
  "R/04_database.R",
  "R/10_alerts.R"
)

# Sanity check — abort if any required file is missing
missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing files:\n", paste(" -", missing, collapse = "\n"))
}
message("All required files found.")

# ── 3. Configure shinyapps.io account (only needed once) ─────────────────────
# If you haven't set this up yet, run this block once with your credentials
# from shinyapps.io → Account → Tokens → Show secret:
#
# rsconnect::setAccountInfo(
#   name   = "dopady",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# ── 4. Deploy ─────────────────────────────────────────────────────────────────
rsconnect::deployApp(
  appDir     = project_root,
  appFiles   = app_files,
  appName    = "snemovnadnes",        # must match your shinyapps.io app name
  account    = "dopady",
  forceUpdate = TRUE,
  launch.browser = FALSE              # don't auto-open browser after deploy
)

message("Deployment complete.")
message("Next: set environment variables in shinyapps.io dashboard (see below).")