library(httr2)
library(DBI)

# ── Requires the uuid package ─────────────────────────────────────────────────
# install.packages("uuid")
# Add library(uuid) to 07_scheduled_run.R

# ── Table init ────────────────────────────────────────────────────────────────
# Called from init_db() in 04_database.R — see patch file.

init_alerts_table <- function(db) {
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS alerts (
      id            SERIAL PRIMARY KEY,
      email         TEXT NOT NULL,
      predkladatele TEXT,        -- comma-separated ministry codes, e.g. 'MF,MPSV'
      subjekty      TEXT,        -- comma-separated subject types, e.g. 'podnikatelé,domácnosti'
      keyword       TEXT,        -- free-text search in cost/benefit popis
      token         TEXT UNIQUE NOT NULL,
      active        BOOLEAN DEFAULT FALSE,
      created_at    TIMESTAMPTZ DEFAULT now(),
      last_fired_at TIMESTAMPTZ
    )
  ")
  tryCatch(
    dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_alerts_token  ON alerts(token)"),
    error = function(e) invisible(NULL)
  )
  tryCatch(
    dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_alerts_active ON alerts(active) WHERE active = TRUE"),
    error = function(e) invisible(NULL)
  )
  message("Alerts table initialized")
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Collapse a character vector to a comma-separated string; return NA if empty.
.vec_to_csv <- function(x) {
  x <- x[!is.na(x) & nchar(trimws(x)) > 0]
  if (length(x) == 0) NA_character_ else paste(x, collapse = ",")
}

# Parse a comma-separated string back to a character vector.
.csv_to_vec <- function(x) {
  if (is.na(x) || x == "") character(0) else strsplit(x, ",", fixed = TRUE)[[1]]
}

# Minimal HTML escaping without external dependencies.
.esc <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}

# ── Create a new (unconfirmed) alert ─────────────────────────────────────────
# predkladatele : character vector of ministry codes, e.g. c("MF", "MPSV")
# subjekty      : character vector of subject types, e.g. c("podnikatelé")
# keyword       : single free-text string (optional)
# Returns list(ok, message) and — on success — list(ok, token).

save_alert <- function(db, email,
                       predkladatele = character(0),
                       subjekty      = character(0),
                       keyword       = NULL) {
  
  # Email format
  if (!grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE)) {
    return(list(ok = FALSE, message = "Neplatná e-mailová adresa."))
  }
  
  # Rate limit: max 5 alerts per address (active or pending)
  count <- dbGetQuery(db,
                      "SELECT COUNT(*) AS n FROM alerts WHERE email = $1",
                      params = list(email)
  )$n
  if (count >= 5) {
    return(list(ok = FALSE,
                message = "Na tuto adresu je registrováno maximálně 5 upozornění. Zrušte starší, než přidáte nové."
    ))
  }
  
  # Normalise inputs
  predkladatele <- predkladatele[!is.na(predkladatele) & nchar(trimws(predkladatele)) > 0]
  subjekty      <- subjekty     [!is.na(subjekty)      & nchar(trimws(subjekty))      > 0]
  kw <- if (!is.null(keyword) && nchar(trimws(keyword)) > 0) tolower(trimws(keyword)) else ""
  
  # At least one criterion required
  if (length(predkladatele) == 0 && length(subjekty) == 0 && kw == "") {
    return(list(ok = FALSE,
                message = "Zadejte alespoň jedno kritérium (ministerstvo, typ subjektu nebo klíčové slovo)."
    ))
  }
  
  # Keyword length guard
  if (nchar(kw) > 100) {
    return(list(ok = FALSE, message = "Klíčové slovo nesmí být delší než 100 znaků."))
  }
  
  token <- uuid::UUIDgenerate()
  
  tryCatch({
    dbExecute(db, "
      INSERT INTO alerts (email, predkladatele, subjekty, keyword, token, active, created_at)
      VALUES ($1, $2, $3, $4, $5, FALSE, now())
    ", params = list(
      email,
      .vec_to_csv(predkladatele),
      .vec_to_csv(subjekty),
      if (kw == "") NA_character_ else kw,
      token
    ))
    list(ok = TRUE, token = token)
  }, error = function(e) {
    message("save_alert error: ", e$message)
    list(ok = FALSE, message = "Nepodařilo se uložit upozornění. Zkuste to prosím znovu.")
  })
}

# ── Confirm an alert via token ────────────────────────────────────────────────

confirm_alert <- function(db, token) {
  if (!grepl("^[0-9a-f-]{36}$", token, ignore.case = TRUE)) {
    return(list(ok = FALSE, message = "Neplatný odkaz."))
  }
  rows <- dbExecute(db,
                    "UPDATE alerts SET active = TRUE WHERE token = $1 AND active = FALSE",
                    params = list(token)
  )
  if (rows == 1L) {
    list(ok = TRUE,  message = "Upozornění bylo aktivováno. Budeme vás informovat o nových materiálech.")
  } else {
    list(ok = FALSE, message = "Odkaz není platný nebo upozornění již bylo aktivováno.")
  }
}

# ── Unsubscribe via token ─────────────────────────────────────────────────────

unsubscribe_alert <- function(db, token) {
  if (!grepl("^[0-9a-f-]{36}$", token, ignore.case = TRUE)) {
    return(list(ok = FALSE, message = "Neplatný odkaz."))
  }
  rows <- dbExecute(db,
                    "UPDATE alerts SET active = FALSE WHERE token = $1",
                    params = list(token)
  )
  if (rows >= 1L) {
    list(ok = TRUE,  message = "Upozornění bylo zrušeno. Nebudeme vám již zasílat e-maily.")
  } else {
    list(ok = FALSE, message = "Odkaz není platný nebo upozornění nebylo nalezeno.")
  }
}

# ── Send email via Resend API ─────────────────────────────────────────────────
# Sign up at resend.com (free: 3 000 emails/month).
# Set RESEND_API_KEY and RESEND_FROM in .Renviron and shinyapps.io env vars.

send_email_resend <- function(to, subject, html_body) {
  api_key   <- Sys.getenv("RESEND_API_KEY")
  from_addr <- Sys.getenv("RESEND_FROM", "Sněmovna dnes <alerts@snemovnadnes.cz>")
  
  if (api_key == "") {
    message("WARN: RESEND_API_KEY not set — skipping email to ", to)
    return(invisible(FALSE))
  }
  
  ok <- tryCatch({
    request("https://api.resend.com/emails") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      req_body_json(list(
        from    = from_addr,
        to      = list(to),
        subject = subject,
        html    = html_body
      )) |>
      req_timeout(15) |>
      req_perform()
    TRUE
  }, error = function(e) {
    message("Email send failed to ", to, ": ", e$message)
    FALSE
  })
  invisible(ok)
}

# ── Email builders ────────────────────────────────────────────────────────────

.criteria_html <- function(predkladatele, subjekty, keyword) {
  items <- character(0)
  if (length(predkladatele) > 0)
    items <- c(items, paste0("Ministerstva: <strong>", .esc(paste(predkladatele, collapse = ", ")), "</strong>"))
  if (length(subjekty) > 0)
    items <- c(items, paste0("Typy subjektů: <strong>", .esc(paste(subjekty, collapse = ", ")), "</strong>"))
  if (!is.na(keyword) && nchar(keyword) > 0)
    items <- c(items, paste0("Klíčové slovo v dopadech: <strong>", .esc(keyword), "</strong>"))
  paste(items, collapse = "<br>")
}

build_confirmation_email <- function(email, predkladatele, subjekty, keyword,
                                     token, app_url) {
  confirm_url     <- paste0(app_url, "?confirm=", token)
  unsubscribe_url <- paste0(app_url, "?unsubscribe=", token)
  crit_html       <- .criteria_html(predkladatele, subjekty, keyword)
  
  paste0('<!DOCTYPE html><html><body>
<div style="font-family:Georgia,serif;max-width:560px;margin:0 auto;color:#1a1a2e;">
  <div style="background:#1a1a2e;padding:24px 32px;border-bottom:4px solid #c0392b;">
    <h1 style="color:#f5f0e8;font-size:1.5rem;margin:0;">Sněmovna dnes</h1>
    <p style="color:#a0a0b0;margin:6px 0 0;font-size:0.8rem;text-transform:uppercase;letter-spacing:1px;">Potvrzení upozornění</p>
  </div>
  <div style="padding:32px;">
    <p>Žádáte o upozornění na nové legislativní materiály s těmito kritérii:</p>
    <div style="background:#f5f0e8;border-left:3px solid #c0392b;padding:16px 20px;margin:20px 0;">
      ', crit_html, '
    </div>
    <p>Pro aktivaci klikněte na tlačítko níže. Pokud jste o upozornění nežádali, e-mail ignorujte.</p>
    <a href="', confirm_url, '"
       style="display:inline-block;background:#1a1a2e;color:#f5f0e8;padding:12px 28px;
              text-decoration:none;font-size:1rem;font-weight:600;margin:8px 0;">
      Aktivovat upozornění
    </a>
    <p style="margin-top:32px;font-size:0.82rem;color:#7f8c8d;border-top:1px solid #d5cfc4;padding-top:16px;">
      Pro zrušení klikněte <a href="', unsubscribe_url, '" style="color:#c0392b;">zde</a>.
    </p>
  </div>
</div></body></html>')
}

build_notification_email <- function(bills_df, alert_row, predkladatele,
                                     subjekty, keyword, app_url) {
  unsubscribe_url <- paste0(app_url, "?unsubscribe=", alert_row$token)
  crit_sentence   <- .criteria_html(predkladatele, subjekty, keyword)
  
  rows_html <- paste(sapply(seq_len(nrow(bills_df)), function(i) {
    b        <- bills_df[i, ]
    bill_url <- paste0(app_url, "?pid=", b$pid)
    doc_label <- switch(
      if (!is.na(b$typ_dokumentu)) b$typ_dokumentu else "zadne",
      "RIA"            = "Záv&#283;re&#269;ná zpráva RIA",
      "prehled_dopadu" = "P&#345;ehled dopad&#367;",
      "Bez hodnocení"
    )
    meta <- paste(
      Filter(function(x) !is.na(x) && x != "", c(b$predkladatel, doc_label)),
      collapse = " &middot; "
    )
    paste0(
      '<div style="border-bottom:1px solid #d5cfc4;padding:16px 0;">',
      '<a href="', bill_url, '" style="color:#1a1a2e;text-decoration:none;',
      'font-size:1.02rem;font-weight:600;">', .esc(b$title), '</a>',
      if (nchar(meta) > 0)
        paste0('<div style="margin-top:5px;font-size:0.88rem;color:#7f8c8d;">', meta, '</div>'),
      '</div>'
    )
  }), collapse = "\n")
  
  n       <- nrow(bills_df)
  n_label <- if (n == 1) "1 nový materiál" else paste0(n, " nové materiály")
  
  paste0('<!DOCTYPE html><html><body>
<div style="font-family:Georgia,serif;max-width:560px;margin:0 auto;color:#1a1a2e;">
  <div style="background:#1a1a2e;padding:24px 32px;border-bottom:4px solid #c0392b;">
    <h1 style="color:#f5f0e8;font-size:1.5rem;margin:0;">Sněmovna dnes</h1>
    <p style="color:#a0a0b0;margin:6px 0 0;font-size:0.8rem;text-transform:uppercase;letter-spacing:1px;">Nové materiály</p>
  </div>
  <div style="padding:32px;">
    <p>Nalezli jsme <strong>', n_label, '</strong> odpovídající vašemu upozornění:</p>
    <div style="font-size:0.88rem;color:#7f8c8d;margin-bottom:16px;">', crit_sentence, '</div>
    ', rows_html, '
    <div style="margin-top:28px;text-align:center;">
      <a href="', app_url, '"
         style="display:inline-block;background:#1a1a2e;color:#f5f0e8;padding:12px 28px;
                text-decoration:none;font-size:1rem;font-weight:600;">
        Otevřít aplikaci
      </a>
    </div>
    <p style="margin-top:32px;font-size:0.82rem;color:#7f8c8d;border-top:1px solid #d5cfc4;padding-top:16px;">
      Upozornění zaregistrováno na adrese ', .esc(alert_row$email), '.
      Pro zrušení klikněte <a href="', unsubscribe_url, '" style="color:#c0392b;">zde</a>.
    </p>
  </div>
</div></body></html>')
}

# ── Send confirmation email (called from Shiny after save_alert) ──────────────

send_confirmation_email <- function(email, predkladatele, subjekty, keyword, token,
                                    app_url = Sys.getenv("APP_URL", "https://snemovnadnes.cz")) {
  html <- build_confirmation_email(email, predkladatele, subjekty, keyword, token, app_url)
  send_email_resend(
    to        = email,
    subject   = "Sněmovna dnes: potvrďte prosím své upozornění",
    html_body = html
  )
}

# ── Main scheduled function: check alerts and fire notifications ───────────────

check_and_fire_alerts <- function(db,
                                  app_url = Sys.getenv("APP_URL", "https://snemovnadnes.cz")) {
  
  message("=== Checking alerts ===")
  
  alerts <- dbGetQuery(db, "
    SELECT id, email,
           COALESCE(predkladatele, '') AS predkladatele,
           COALESCE(subjekty,      '') AS subjekty,
           COALESCE(keyword,       '') AS keyword,
           token,
           last_fired_at::text         AS last_fired_at
    FROM alerts
    WHERE active = TRUE
    ORDER BY id
  ")
  
  if (nrow(alerts) == 0) {
    message("No active alerts — skipping")
    return(invisible(0L))
  }
  message("Active alerts: ", nrow(alerts))
  
  fired <- 0L
  
  for (i in seq_len(nrow(alerts))) {
    a <- alerts[i, ]
    
    predkladatele <- .csv_to_vec(a$predkladatele)
    subjekty      <- .csv_to_vec(a$subjekty)
    keyword       <- a$keyword
    
    has_pred  <- length(predkladatele) > 0
    has_subj  <- length(subjekty)      > 0
    has_kw    <- nchar(keyword)        > 0
    
    since <- if (!is.na(a$last_fired_at) && a$last_fired_at != "") {
      a$last_fired_at
    } else {
      format(Sys.time() - 48 * 3600, "%Y-%m-%dT%H:%M:%SZ")
    }
    
    # Build query dynamically so each parameter slot is used exactly once.
    # We always filter on impacts.created_at (ensures we only fire on newly
    # processed bills, not on every metadata refresh).
    base_sql <- "
      SELECT DISTINCT b.pid, b.title, b.predkladatel, i.typ_dokumentu
      FROM   bills b
      JOIN   impacts i ON i.pid = b.pid
      WHERE  i.created_at > $1::timestamptz
    "
    params      <- list(since)
    param_n     <- 1L
    
    if (has_pred) {
      param_n  <- param_n + 1L
      # Store predkladatele as CSV; split in SQL with string_to_array
      base_sql <- paste0(base_sql,
                         sprintf("\n  AND b.predkladatel = ANY(string_to_array($%d, ','))", param_n))
      params   <- c(params, list(paste(predkladatele, collapse = ",")))
    }
    
    if (has_subj) {
      param_n  <- param_n + 1L
      subj_csv <- paste(subjekty, collapse = ",")
      base_sql <- paste0(base_sql, sprintf("
  AND (
    EXISTS (SELECT 1 FROM costs    c  WHERE c.pid  = b.pid AND c.subjekt  = ANY(string_to_array($%d, ',')))
    OR
    EXISTS (SELECT 1 FROM benefits bn WHERE bn.pid = b.pid AND bn.subjekt = ANY(string_to_array($%d, ',')))
  )", param_n, param_n))
      params   <- c(params, list(subj_csv))
    }
    
    if (has_kw) {
      param_n  <- param_n + 1L
      base_sql <- paste0(base_sql, sprintf("
  AND (
    EXISTS (SELECT 1 FROM costs    c  WHERE c.pid  = b.pid AND lower(c.popis)  LIKE '%%' || $%d || '%%')
    OR
    EXISTS (SELECT 1 FROM benefits bn WHERE bn.pid = b.pid AND lower(bn.popis) LIKE '%%' || $%d || '%%')
  )", param_n, param_n))
      params   <- c(params, list(keyword))
    }
    
    base_sql <- paste0(base_sql, "\n  ORDER BY b.pid")
    
    matches <- tryCatch(
      dbGetQuery(db, base_sql, params = params),
      error = function(e) {
        message("  Alert ", a$id, " query error: ", e$message)
        data.frame()
      }
    )
    
    if (nrow(matches) == 0) {
      message("  Alert ", a$id, " (", a$email, "): no new matches")
      next
    }
    
    n <- nrow(matches)
    message("  Alert ", a$id, " (", a$email, "): ", n, " match(es) — sending email")
    
    subject <- if (n == 1) {
      "Sněmovna dnes: 1 nový materiál odpovídá vašemu upozornění"
    } else {
      paste0("Sněmovna dnes: ", n, " nové materiály odpovídají vašemu upozornění")
    }
    
    html <- build_notification_email(matches, a, predkladatele, subjekty, keyword, app_url)
    ok   <- send_email_resend(a$email, subject, html)
    
    if (ok) {
      dbExecute(db,
                "UPDATE alerts SET last_fired_at = now() WHERE id = $1",
                params = list(a$id)
      )
      fired <- fired + 1L
    }
    
    Sys.sleep(0.5)
  }
  
  message("Alert emails sent: ", fired, " of ", nrow(alerts), " active alerts")
  invisible(fired)
}