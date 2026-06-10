# Alert system implementation — step-by-step

## Files delivered

| File | What to do with it |
|---|---|
| `10_alerts.R` | Copy to `R/10_alerts.R` — new file, no conflict |
| `04_database_patch.R` | Insert one block into `04_database.R` (see below) |
| `07_scheduled_run_patch.R` | Four small edits to `07_scheduled_run.R` (see below) |
| `app_patch.R` | Four insertion points in `app.R` (see below) |

---

## Step 1 — Copy 10_alerts.R

No conflicts. Just copy it to `R/10_alerts.R`.

---

## Step 2 — Edit 04_database.R

Find the single line near the end of `init_db()`:

```r
  message("Database initialized")
```

Insert the alerts table block from `04_database_patch.R` immediately **before** that line.
The alerts table is created on every startup via `CREATE TABLE IF NOT EXISTS`, so it is
safe to deploy to an existing database.

---

## Step 3 — Edit 07_scheduled_run.R

**3a.** Fix the stray `p` on line ~59 (it sits alone between `doc_rank()` and
the `# ── 3. Refresh a single bill` comment). Delete it — this is an existing
bug that crashes every run.

**3b.** Add to the `library()` block:
```r
library(uuid)
```
Install once: `install.packages("uuid")`

**3c.** Add to the `source()` block:
```r
source("R/10_alerts.R")
```

**3d.** Replace the bottom `# ── Run ──` section with the version in
`07_scheduled_run_patch.R` (adds `check_and_fire_alerts()` after the komise scraper).

---

## Step 4 — Edit app.R

**4a.** After the existing `source("D:/snemovni dopady/snemovni-dopady/R/04_database.R")`:
```r
source("D:/snemovni dopady/snemovni-dopady/R/10_alerts.R")
```

**4b.** Append the alert CSS block (from `app_patch.R`, the section labelled
CHANGE 2) to the end of the `app_css` string — paste it inside the last
closing quote.

**4c.** Add the bell button to the header (CHANGE 3 in `app_patch.R`):
```r
tags$button(
  class   = "bell-btn",
  title   = "Nastavit e-mailové upozornění",
  onclick = "Shiny.setInputValue('open_alert_modal', Date.now(), {priority: 'event'})",
  "🔔 Upozornění"
)
```
This goes inside `div(class = "app-header", ...)`, after the existing
`div(class = "header-left", ...)` block.

**4d.** Add the four server observers (CHANGE 4 in `app_patch.R`) anywhere
inside `server()`, after the existing `observeEvent(input$go_home, ...)` block.

---

## Step 5 — Environment variables

Add to `.Renviron` (local) and to shinyapps.io Environment Variables:

```
RESEND_API_KEY=re_xxxxxxxxxxxxxxxxxxxx
RESEND_FROM=Sněmovna dnes <alerts@yourdomain.cz>
APP_URL=https://your-app.shinyapps.io/snemovni-dopady
```

- Sign up at **resend.com** (free tier: 3 000 emails/month).
- Verify your sender domain in the Resend dashboard.
- `APP_URL` must NOT end with a trailing slash.

---

## How the system works end-to-end

```
User clicks 🔔 in header
  → modal opens
  → fills email + criteria
  → clicks submit
  → save_alert() inserts row with active=FALSE
  → send_confirmation_email() sends via Resend
  → user clicks link in email → ?confirm=TOKEN
  → app's URL observer fires confirm_alert() → active=TRUE

Daily scheduled run (07_scheduled_run.R)
  → run_pipeline() processes new bills
  → check_and_fire_alerts()
      for each active alert:
        query bills where impacts.created_at > last_fired_at
        apply predkladatel / keyword filters
        if matches found → send_email_resend() → update last_fired_at

User clicks unsubscribe link in email → ?unsubscribe=TOKEN
  → unsubscribe_alert() sets active=FALSE
```

---

## Security summary

| Threat | Mitigation |
|---|---|
| Anyone signing up a stranger's email | Double opt-in: alert stays `active=FALSE` until confirmed |
| Email flooding (many signups) | Max 5 alerts per email address |
| SQL injection via keyword | Parameterised queries throughout (`$1`, `$2`, …) |
| Invalid tokens in URL | UUID format validated with regex before any DB query |
| Missing RESEND_API_KEY | `send_email_resend()` logs a warning and returns FALSE; pipeline continues |
| GDPR | Unsubscribe link in every email; no data beyond email + criteria stored |
