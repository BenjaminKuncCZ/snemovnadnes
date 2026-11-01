library(httr2)
library(rvest)
library(xml2)
library(pdftools)
library(stringr)
library(DBI)

# ── Komise RIA scraper ────────────────────────────────────────────────────────
# Changes vs. the previous version (why bills were missing their verdict):
#   1. Session discovery. The yearly index pages use inconsistent link slugs:
#        .../jednani-c-148-dne-30-ledna-2026/                  (old pattern)
#        .../jednani-komise-ria-c-150-dne-5-cervna-2026/       (new pattern)
#        https://ria.vlada.cz/jednani-c-148-dne-20-unora-2026/ (no /komise/ prefix)
#      The old filter (`jednani-c-\d+` AND `/komise/`) dropped every session
#      except the first one of 2026. Now any link that looks like a session is
#      kept, query strings (?preview=true) are stripped, hosts ria.vlada.cz and
#      ria.gov.cz are both accepted.
#   2. Session parsing no longer assumes "<p><strong>title</strong></p><ul>…".
#      Every bold title starts a block; all links until the next title belong
#      to it. PDFs are accepted on any host and are recognised by href OR link
#      text containing "stanovisk".
#   3. Bills with several VeKLEP links on one block are all updated; bills with
#      no VeKLEP link fall back to a (strict) title match.
#   4. Verdict classification is more tolerant and unclassified conclusions are
#      logged with their text so the rules can be tuned.
#   5. Verdicts are refreshed if a newer stanovisko URL appears.

RIA_HOSTS <- c("ria.vlada.cz", "ria.gov.cz")

# ── Helpers ───────────────────────────────────────────────────────────────────

fetch_html <- function(url, timeout = 60) {
  tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0") |>
      req_timeout(timeout) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_string() |>
      read_html()
  }, error = function(e) {
    message("Fetch failed: ", url, " — ", conditionMessage(e))
    NULL
  })
}

strip_query <- function(u) str_remove(u, "[?#].*$")

url_host <- function(u) {
  h <- str_match(u, "^https?://([^/]+)")[, 2]
  str_remove(tolower(h), "^www\\.")
}

norm_title <- function(x) {
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

CZ_MONTHS <- c(ledna = "ledna", unora = "února", brezna = "března",
               dubna = "dubna", kvetna = "května", cervna = "června",
               cervence = "července", srpna = "srpna", zari = "září",
               rijna = "října", listopadu = "listopadu", prosince = "prosince")

# ── Scrape sessions index for a given year ────────────────────────────────────

get_session_urls <- function(year = as.integer(format(Sys.Date(), "%Y"))) {

  index_paths <- c(
    paste0("/komise/jednani-v-roce-", year, "/"),
    paste0("/komise/jednani-komise-ria-v-roce-", year, "/"),
    paste0("/komise/jednani-", year, "/")
  )

  sessions <- character(0)

  for (host in RIA_HOSTS) {
    for (p in index_paths) {
      index_url <- paste0("https://", host, p)
      message("Trying: ", index_url)
      page <- fetch_html(index_url)
      if (is.null(page)) next

      a     <- html_elements(page, "a")
      hrefs <- html_attr(a, "href")
      texts <- str_squish(html_text(a))

      is_session <- !is.na(hrefs) & (
        str_detect(tolower(hrefs), "jednani[a-z0-9-]*c-\\d+") |
          str_detect(tolower(texts),
                     "^jednání[\\s\\u00a0]+komise[\\s\\u00a0]+ria[\\s\\u00a0]+č\\.?[\\s\\u00a0]*\\d+")
      )

      if (!any(is_session)) next

      urls <- xml2::url_absolute(hrefs[is_session], index_url) |> strip_query()
      urls <- urls[url_host(urls) %in% RIA_HOSTS]
      sessions <- c(sessions, urls)

      if (length(sessions) > 0) break
    }
    if (length(sessions) > 0) break
  }

  sessions <- unique(sessions)
  message("Found ", length(sessions), " sessions for ", year)
  if (length(sessions) == 0) return(NULL)
  sessions
}

# ── Parse one session page ────────────────────────────────────────────────────

node_title <- function(node) {
  nm <- xml_name(node)
  if (nm %in% c("ul", "ol", "table")) return(NA_character_)
  strong <- xml_find_first(node, ".//strong|.//b")
  title <- if (!inherits(strong, "xml_missing")) {
    xml_text(strong)
  } else if (nm %in% c("h2", "h3", "h4", "h5")) {
    xml_text(node)
  } else {
    NA_character_
  }
  if (is.na(title)) return(NA_character_)
  title <- str_squish(title)
  if (nchar(title) <= 30) return(NA_character_)
  if (str_detect(tolower(title),
                 "hodnocení dopadů|úřad vlády|oddělení ria|jednání komise ria|pracovní komise")) {
    return(NA_character_)
  }
  title
}

node_links <- function(node, base) {
  a <- xml_find_all(node, ".//a[@href]")
  if (length(a) == 0) {
    return(data.frame(href = character(), text = character(),
                      stringsAsFactors = FALSE))
  }
  href <- xml_attr(a, "href")
  data.frame(
    href = xml2::url_absolute(href, base),
    text = str_squish(xml_text(a)),
    stringsAsFactors = FALSE
  )
}

finalize_block <- function(b, session_date, session_url) {
  if (is.null(b)) return(NULL)
  l <- b$links

  pids <- character(0)
  if (nrow(l) > 0) {
    m <- str_match(l$href,
                   "(?i)odok\\.(?:gov\\.)?cz/portal/veklep/material/([A-Za-z0-9]{12})")[, 2]
    pids <- unique(toupper(m[!is.na(m)]))
  }

  stan <- NA_character_
  if (nrow(l) > 0) {
    is_pdf  <- str_detect(tolower(strip_query(l$href)), "\\.pdf$")
    is_stan <- str_detect(tolower(l$href), "stanovisk") |
      str_detect(tolower(l$text), "stanovisk")
    cand <- strip_query(l$href[is_pdf & is_stan])
    if (length(cand) > 0) stan <- tail(cand, 1)   # latest stanovisko wins
  }

  if (length(pids) == 0 && is.na(stan)) return(NULL)

  list(
    title          = b$title,
    veklep_pids    = pids,
    stanovisko_url = stan,
    session_date   = session_date,
    session_url    = session_url
  )
}

parse_session_items <- function(page, session_url, session_date) {

  nodes <- xml_find_all(
    page,
    "//*[self::p or self::h2 or self::h3 or self::h4 or self::h5 or self::ul or self::ol or self::table]"
  )

  results <- list()
  current <- NULL

  for (node in nodes) {
    title <- node_title(node)

    if (!is.na(title)) {
      item <- finalize_block(current, session_date, session_url)
      if (!is.null(item)) results[[length(results) + 1]] <- item
      current <- list(title = title,
                      links = data.frame(href = character(), text = character(),
                                         stringsAsFactors = FALSE))
    }

    if (!is.null(current)) {
      current$links <- rbind(current$links, node_links(node, session_url))
    }
  }

  item <- finalize_block(current, session_date, session_url)
  if (!is.null(item)) results[[length(results) + 1]] <- item

  results
}

scrape_session <- function(url) {

  message("Scraping session: ", url)
  page <- fetch_html(url)
  if (is.null(page)) return(list())

  h1 <- xml_find_first(page, "//h1")
  h1_text <- if (inherits(h1, "xml_missing")) "" else xml_text(h1)
  session_date <- str_extract(h1_text, "\\d+\\.\\s*\\p{L}+\\s*\\d{4}")

  if (is.na(session_date)) {
    m <- str_match(tolower(url), "dne-(\\d+)-([a-z]+)-(\\d{4})")
    if (!is.na(m[1, 1])) {
      mon <- if (m[1, 3] %in% names(CZ_MONTHS)) CZ_MONTHS[[m[1, 3]]] else m[1, 3]
      session_date <- paste0(m[1, 2], ". ", mon, " ", m[1, 4])
    } else {
      session_date <- ""
    }
  }

  items <- parse_session_items(page, url, session_date)
  message("  ", length(items), " item(s) with a VeKLEP link and/or stanovisko")
  items
}

# ── Classify verdict into A/B/C/D ─────────────────────────────────────────────

classify_verdict <- function(text) {

  t <- tolower(str_replace_all(text, "[\\s\\u00a0]+", " "))

  if (str_detect(t, "nedoporuč|neschválit|neschválení"))              return("D")
  if (str_detect(t, "přepracov"))                                      return("C")
  if (str_detect(t, "zapracov|zohledn|dopracov|za předpokladu"))       return("B")
  if (str_detect(t, "doporu[čc]\\w*.{0,200}schvál"))                   return("A")

  NA_character_
}

# ── Download and extract verdict from stanovisko PDF ─────────────────────────

extract_verdict <- function(pdf_url, dest_dir = "data/stanoviska") {

  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

  # One cache file per URL (the same file name can exist in several sessions)
  key <- str_remove(str_remove(pdf_url, "^https?://"), "[?#].*$")
  key <- str_replace_all(key, "[^A-Za-z0-9._-]", "_")
  filename  <- str_sub(key, max(1, nchar(key) - 150), nchar(key))
  dest_path <- file.path(dest_dir, filename)

  if (!file.exists(dest_path)) {
    message("Downloading stanovisko: ", pdf_url)
    tryCatch({
      request(pdf_url) |>
        req_headers("User-Agent" = "Mozilla/5.0") |>
        req_timeout(60) |>
        req_retry(max_tries = 3) |>
        req_perform(path = dest_path)
    }, error = function(e) {
      message("Download failed: ", conditionMessage(e))
      if (file.exists(dest_path)) file.remove(dest_path)
    })
  }

  if (!file.exists(dest_path) || file.size(dest_path) < 1000) {
    message("Download failed or file too small, skipping: ", pdf_url)
    if (file.exists(dest_path)) file.remove(dest_path)
    return(NULL)
  }

  text <- tryCatch(
    paste(pdf_text(dest_path), collapse = "\n"),
    error = function(e) {
      message("PDF read failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(text) || nchar(str_squish(text)) < 50) {
    message("No extractable text (scanned PDF?): ", pdf_url)
    return(NULL)
  }
  text <- str_replace_all(text, "[\\s\\u00a0]+", " ")

  # Candidate starts: every "Závěr" heading (optionally "IV. Závěr"), last first
  loc <- str_locate_all(text, "(?i)(?:\\b[IVX]+\\.\\s*)?Záv[ěe]r\\b")[[1]]
  starts <- if (nrow(loc) > 0) rev(loc[, "start"]) else integer(0)

  verdict <- NA_character_
  zaver_text <- ""
  for (s in starts) {
    zt <- str_sub(text, s, s + 2500)
    v  <- classify_verdict(zt)
    if (!is.na(v)) { verdict <- v; zaver_text <- zt; break }
  }
  if (is.na(verdict)) {
    zaver_text <- str_sub(text, max(1, nchar(text) - 3000), nchar(text))
    verdict <- classify_verdict(zaver_text)
  }
  if (is.na(verdict)) {
    message("UNCLASSIFIED conclusion in ", pdf_url, ":\n  ",
            str_trunc(zaver_text, 400))
  }

  list(verdict = verdict, zaver_text = str_trunc(zaver_text, 500),
       pdf_path = dest_path)
}

# ── Save to database ──────────────────────────────────────────────────────────

save_komise_verdict <- function(db, pid, verdict, zaver_text,
                                stanovisko_url, session_date) {
  if (is.na(verdict)) {
    message("Skipping NA verdict for ", pid)
    return(invisible(NULL))
  }

  dbExecute(db, "
    UPDATE bills
    SET komise_verdict = $1,
        komise_zaver   = $2,
        komise_url     = $3,
        komise_date    = $4
    WHERE pid = $5
  ", params = list(verdict, zaver_text, stanovisko_url, session_date, pid))

  message("Saved verdict ", verdict, " for ", pid)
}

# ── Title fallback (only when the session block has no VeKLEP link) ──────────

match_pids_by_title <- function(title, bills_df) {
  nt <- norm_title(title)
  if (nchar(nt) < 40) return(character(0))
  nb <- vapply(bills_df$title, function(x) norm_title(ifelse(is.na(x), "", x)),
               character(1), USE.NAMES = FALSE)
  hit <- which(nchar(nb) >= 40 & (str_detect(nb, fixed(nt)) | str_detect(nt, fixed(nb))))
  if (length(hit) == 1) bills_df$pid[hit] else character(0)   # must be unambiguous
}

# ── Main function ─────────────────────────────────────────────────────────────
# dry_run = TRUE : download and classify, print what WOULD be saved, no DB writes
# force   = TRUE : re-evaluate bills that already have this stanovisko stored
#                  (use once for a backfill: run_komise_scraper(2024:2026, force = TRUE))

run_komise_scraper <- function(
    years   = (as.integer(format(Sys.Date(), "%Y")) - 1):as.integer(format(Sys.Date(), "%Y")),
    db      = NULL,
    dry_run = FALSE,
    force   = FALSE) {

  close_db <- FALSE
  if (is.null(db)) {
    db <- get_db()
    close_db <- TRUE
  }
  on.exit(if (close_db) dbDisconnect(db), add = TRUE)

  if (!dry_run) {
    for (col in c("komise_verdict", "komise_zaver", "komise_url", "komise_date")) {
      tryCatch(
        dbExecute(db, paste0("ALTER TABLE bills ADD COLUMN IF NOT EXISTS ", col, " TEXT")),
        error = function(e) invisible(NULL)
      )
    }
  }

  bills_df <- dbGetQuery(db, "SELECT pid, title, komise_verdict, komise_url FROM bills")
  verdict_cache <- list()

  stats <- list(sessions = 0L, items = 0L, saved = 0L, unchanged = 0L,
                not_in_db = 0L, no_stanovisko = 0L, unclassified = 0L)

  for (year in years) {

    session_urls <- get_session_urls(year)
    if (is.null(session_urls)) next

    for (surl in session_urls) {

      items <- tryCatch(
        scrape_session(surl),
        error = function(e) { message("Session error: ", conditionMessage(e)); list() }
      )
      stats$sessions <- stats$sessions + 1L

      for (item in items) {
        stats$items <- stats$items + 1L

        pids <- item$veklep_pids
        if (length(pids) == 0) {
          pids <- match_pids_by_title(item$title, bills_df)
          if (length(pids) == 1)
            message("  Title match (no VeKLEP link): ", str_trunc(item$title, 70),
                    " → ", pids)
        }
        if (length(pids) == 0) {
          message("  No PID for: ", str_trunc(item$title, 80))
          next
        }

        in_db <- pids[pids %in% bills_df$pid]
        for (p in setdiff(pids, in_db)) {
          message("  PID ", p, " is on Komise page but not tracked in bills table")
          stats$not_in_db <- stats$not_in_db + 1L
        }
        if (length(in_db) == 0) next

        if (is.na(item$stanovisko_url)) {
          message("  No stanovisko PDF yet for: ", str_trunc(item$title, 70))
          stats$no_stanovisko <- stats$no_stanovisko + 1L
          next
        }

        # Skip bills whose stored stanovisko is already this one
        if (!force) {
          row_idx <- match(in_db, bills_df$pid)
          done <- !is.na(bills_df$komise_verdict[row_idx]) &
            !is.na(bills_df$komise_url[row_idx]) &
            bills_df$komise_url[row_idx] == item$stanovisko_url
          if (all(done)) { stats$unchanged <- stats$unchanged + length(in_db); next }
          in_db <- in_db[!done]
        }

        if (is.null(verdict_cache[[item$stanovisko_url]])) {
          res <- tryCatch(
            extract_verdict(item$stanovisko_url),
            error = function(e) { message("Verdict error: ", conditionMessage(e)); NULL }
          )
          verdict_cache[[item$stanovisko_url]] <- if (is.null(res)) FALSE else res
          Sys.sleep(0.5)
        }
        res <- verdict_cache[[item$stanovisko_url]]
        if (isFALSE(res)) next

        if (is.na(res$verdict)) { stats$unclassified <- stats$unclassified + 1L; next }

        for (p in in_db) {
          if (dry_run) {
            message("  [dry run] would save ", res$verdict, " for ", p,
                    " (", item$session_date, ")")
          } else {
            save_komise_verdict(db, p, res$verdict, res$zaver_text,
                                item$stanovisko_url, item$session_date)
            bills_df$komise_verdict[bills_df$pid == p] <- res$verdict
            bills_df$komise_url[bills_df$pid == p]     <- item$stanovisko_url
          }
          stats$saved <- stats$saved + 1L
        }
      }
    }
  }

  message("Komise RIA — sessions: ", stats$sessions,
          " | items: ", stats$items,
          " | ", if (dry_run) "would save: " else "saved: ", stats$saved,
          " | unchanged: ", stats$unchanged,
          " | not in bills table: ", stats$not_in_db,
          " | no stanovisko yet: ", stats$no_stanovisko,
          " | unclassified: ", stats$unclassified)

  invisible(stats)
}
