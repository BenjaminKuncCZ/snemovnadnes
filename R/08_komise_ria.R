library(httr2)
library(rvest)
library(pdftools)
library(stringr)
library(DBI)

# ── Scrape sessions index for a given year ────────────────────────────────────

get_session_urls <- function(year = 2026) {
  
  # Different URL patterns for different years
  url_patterns <- c(
    paste0("https://ria.vlada.cz/komise/jednani-v-roce-", year, "/"),
    paste0("https://ria.vlada.cz/komise/jednani-komise-ria-v-roce-", year, "/"),
    paste0("https://ria.vlada.cz/komise/jednani-", year, "/")
  )
  
  page <- NULL
  for (url in url_patterns) {
    page <- tryCatch({
      message("Trying: ", url)
      request(url) |>
        req_headers("User-Agent" = "Mozilla/5.0") |>
        req_timeout(60) |>
        req_perform() |>
        resp_body_string() |>
        read_html()
    }, error = function(e) NULL)
    if (!is.null(page)) break
  }
  
  if (is.null(page)) {
    message("Could not fetch sessions index for ", year)
    return(NULL)
  }
  
  links <- page |>
    html_elements("a[href*='jednani']") |>
    html_attr("href")
  
  session_links <- links[str_detect(links, "jednani-c-\\d+")]
  session_links <- unique(session_links)
  session_links <- session_links[str_detect(session_links, 
                                            "ria\\.vlada\\.cz/komise/")]
  session_links <- ifelse(
    str_starts(session_links, "http"),
    session_links,
    paste0("https://ria.vlada.cz", session_links)
  )
  
  message("Found ", length(session_links), " sessions for ", year)
  session_links
}

# ── Scrape one session page ────────────────────────────────────────────────────

scrape_session <- function(url) {
  
  message("Scraping session: ", url)
  
  page <- tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0") |>
      req_timeout(60) |>
      req_perform() |>
      resp_body_string() |>
      read_html()
  }, error = function(e) {
    message("Failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(page)) return(NULL)
  
  # Extract session date from URL or heading
  session_date <- str_extract(url, "\\d{4}-\\d{2}-\\d{2}")
  if (is.na(session_date)) {
    session_date <- str_extract(
      html_text(html_element(page, "h1")), 
      "\\d+\\.\\s*\\w+\\s*\\d{4}"
    )
  }
  if (is.na(session_date)) session_date <- ""
  
  # Find all links on page
  all_links <- page |> html_elements("a") |> html_attr("href")
  all_text  <- page |> html_elements("a") |> html_text()
  
  # Extract VeKLEP PIDs from odok.gov.cz links
  veklep_links <- all_links[str_detect(all_links, "odok\\.gov\\.cz.*material")]
  veklep_pids  <- str_extract(veklep_links, "[A-Z0-9]{12,}")
  veklep_pids  <- veklep_pids[!is.na(veklep_pids)]
  
  # Extract OVA numbers as fallback
  ova_links <- all_links[str_detect(all_links, "odok\\.gov\\.cz")]
  
  # Extract stanovisko PDF links
  stanovisko_links <- all_links[str_detect(tolower(all_links), 
                                           "stanovisko|stanoviska|opinion")]
  stanovisko_links <- stanovisko_links[str_detect(stanovisko_links, "\\.pdf$")]
  stanovisko_links <- stanovisko_links[!is.na(stanovisko_links)]
  
  # Make absolute
  stanovisko_links <- ifelse(
    str_starts(stanovisko_links, "http"),
    stanovisko_links,
    paste0("https://ria.vlada.cz", stanovisko_links)
  )
  
  # Get full page text to extract bill titles and structure
  # Each bill block: title (bold), then predkladatel, then OVA/VeKLEP link, then stanovisko
  # Parse by looking at paragraphs with bold headings
  items <- parse_session_items(page, session_date)
  
  items
}

# ── Parse individual bill items from session page ─────────────────────────────

parse_session_items <- function(page, session_date) {
  
  results <- list()
  
  # Get all p and ul elements in order
  nodes <- page |> html_elements("p, ul")
  
  i <- 1
  while (i <= length(nodes)) {
    node <- nodes[[i]]
    
    # Check if this is a <p> with a <strong> bill title
    strong <- html_element(node, "strong")
    if (!is.na(strong)) {
      title <- html_text(strong, trim = TRUE)
      
      # Skip non-bill bold elements
      if (nchar(title) > 30 && 
          !str_detect(tolower(title), "hodnocení dopadů|úřad vlády|oddělení ria")) {
        
        # Look at the next <ul> sibling for links
        veklep_pid     <- NA_character_
        stanovisko_url <- NA_character_
        
        # Check next few nodes for the associated <ul>
        for (j in (i+1):min(i+3, length(nodes))) {
          if (j > length(nodes)) break
          next_node <- nodes[[j]]
          if (html_name(next_node) != "ul") next
          
          node_html <- as.character(next_node)
          
          # Extract VeKLEP PID
          pid_match <- str_extract(node_html,
                                   "odok\\.gov\\.cz/portal/veklep/material/([A-Z0-9]{12,})/",
                                   group = 1)
          if (!is.na(pid_match)) veklep_pid <- pid_match
          
          # Extract stanovisko PDF — specifically links with "stanovisko" text
          stanovisko_match <- str_extract(node_html,
                                          "href=\"(https://ria\\.vlada\\.cz/wp-content/uploads/[^\"]+\\.pdf)\"[^>]*>[^<]*stanovisko",
                                          group = 1)
          if (!is.na(stanovisko_match)) stanovisko_url <- stanovisko_match
          
          break
        }
        
        if (!is.na(veklep_pid) || !is.na(stanovisko_url)) {
          results[[length(results) + 1]] <- list(
            title          = title,
            veklep_pid     = veklep_pid,
            stanovisko_url = stanovisko_url,
            session_date   = session_date
          )
        }
      }
    }
    i <- i + 1
  }
  
  results
}

# ── Download and extract verdict from stanovisko PDF ─────────────────────────

extract_verdict <- function(pdf_url, dest_dir = "data/stanoviska") {
  
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  filename <- str_extract(pdf_url, "[^/]+\\.pdf$")
  dest_path <- file.path(dest_dir, filename)
  
  if (!file.exists(dest_path)) {
    message("Downloading stanovisko: ", filename)
    tryCatch({
      request(pdf_url) |>
        req_headers("User-Agent" = "Mozilla/5.0") |>
        req_timeout(30) |>
        req_perform(path = dest_path)
    }, error = function(e) {
      message("Download failed: ", e$message)
      return(NULL)
    })
  }
  
  if (!file.exists(dest_path) || file.size(dest_path) < 1000) {
    message("Download failed or file too small, skipping: ", filename)
    if (file.exists(dest_path)) file.remove(dest_path)
    return(NULL)
  }
  
  # Extract text from PDF
  text <- tryCatch(
    paste(pdf_text(dest_path), collapse = "\n"),
    error = function(e) {
      message("PDF read failed: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(text)) return(NULL)
  
  # Find Závěr section — always the LAST occurrence of IV. Záv
  all_matches <- str_locate_all(text, "IV\\. Záv")[[1]]
  
  if (nrow(all_matches) > 0) {
    # Take the last match — that's the actual conclusion, not any earlier reference
    zaver_start <- all_matches[nrow(all_matches), "start"]
    zaver_text  <- str_sub(text, zaver_start, zaver_start + 2000)
  } else {
    # Fall back to last 2000 chars
    zaver_text <- str_sub(text, max(1, nchar(text) - 2000), nchar(text))
  }
  
  verdict <- classify_verdict(zaver_text)
  
  list(verdict = verdict, zaver_text = str_trunc(zaver_text, 500), pdf_path = dest_path)
}

# ── Classify verdict into A/B/C/D ─────────────────────────────────────────────

classify_verdict <- function(text) {
  
  t <- tolower(text)
  
  if (str_detect(t, "neschválit|neschválení|nedoporučuje")) {
    return("D")
  }
  if (str_detect(t, "přerušila za účelem přepracování|přepracování")) {
    return("C")
  }
  if (str_detect(t, "za předpokladu zohlednění|zapracování|zapracovat")) {
    return("B")
  }
  if (str_detect(t, "doporučen vládě ke schválení|doporučuje.*schválení")) {
    return("A")
  }
  
  return(NA_character_)
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

# ── Main function ─────────────────────────────────────────────────────────────

run_komise_scraper <- function(years = 2024:2026, db = NULL) {
  
  close_db <- FALSE
  if (is.null(db)) {
    db <- get_db()
    close_db <- TRUE
  }
  on.exit(if (close_db) dbDisconnect(db))
  
  total_matched <- 0
  
  for (year in years) {
    
    session_urls <- get_session_urls(year)
    if (is.null(session_urls)) next
    
    for (surl in session_urls) {
      
      items <- tryCatch(
        scrape_session(surl),
        error = function(e) { message("Session error: ", e$message); list() }
      )
      
      for (item in items) {
        
        pid <- item$veklep_pid
        if (is.null(pid) || is.na(pid)) next
        
        # Check if this PID is in our database
        existing <- dbGetQuery(db, 
                               "SELECT pid FROM bills WHERE pid = $1",
                               params = list(pid)
        )
        if (nrow(existing) == 0) next
        
        # Extract verdict from PDF
        if (!is.null(item$stanovisko_url) && !is.na(item$stanovisko_url)) {
          verdict_result <- tryCatch(
            extract_verdict(item$stanovisko_url),
            error = function(e) { message("Verdict error: ", e$message); NULL }
          )
          
          if (!is.null(verdict_result)) {
            save_komise_verdict(
              db, pid,
              verdict_result$verdict,
              verdict_result$zaver_text,
              item$stanovisko_url,
              item$session_date
            )
            total_matched <- total_matched + 1
          }
        }
        
        Sys.sleep(0.5)
      }
    }
  }
  
  message("Total Komise RIA verdicts matched: ", total_matched)
}
