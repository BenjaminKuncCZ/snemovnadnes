library(httr2)
library(rvest)
library(stringr)
library(xml2)  
library(dplyr)
library(purrr)

# ── Get current session number ────────────────────────────────────────────────

get_current_session <- function() {
  
  page <- request("https://www.psp.cz/sqw/hp.sqw?k=1005") |>
    req_headers("User-Agent" = "Mozilla/5.0") |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_string(encoding = "windows-1250") |>
    read_html()
  
  # Find session numbers from headings like "14. schůze"
  headings <- page |>
    html_elements("h3") |>
    html_text()
  
  session_nums <- str_extract(headings, "^\\d+") |>
    as.integer()
  session_nums <- session_nums[!is.na(session_nums)]
  
  message("Active sessions: ", paste(session_nums, collapse = ", "))
  session_nums
}

# ── Scrape agenda for a specific session ──────────────────────────────────────

scrape_session_agenda <- function(session_num) {
  
  url <- paste0("https://www.psp.cz/schuze/10/", session_num)
  message("Fetching session ", session_num, " agenda: ", url)
  
  page <- tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0") |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_string(encoding = "windows-1250") |>
      read_html()
  }, error = function(e) {
    message("Failed to fetch session ", session_num, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(page)) return(NULL)
  
  # Tisk links use pattern: /sqw/historie.sqw?o=10&t=NUMBER
  tisk_elements <- page |>
    html_elements("a[href*='historie.sqw']")
  
  if (length(tisk_elements) == 0) {
    message("No tisk links found in session ", session_num)
    return(NULL)
  }
  
  # Extract tisk number and surrounding title text
  bill_rows <- lapply(tisk_elements, function(el) {
    href     <- html_attr(el, "href")
    id_tisk  <- str_extract(href, "t=(\\d+)", group = 1)
    
    # Get the parent element text for the bill title
    parent_text <- tryCatch(
      html_text(html_element(el, xpath = ".."), trim = TRUE),
      error = function(e) html_text(el, trim = TRUE)
    )
    
    if (!is.na(id_tisk)) {
      data.frame(
        id_tisk = id_tisk,
        title   = parent_text,
        session = session_num,
        stringsAsFactors = FALSE
      )
    }
  })
  
  bill_df <- do.call(rbind, Filter(Negate(is.null), bill_rows))
  bill_df <- bill_df[!duplicated(bill_df$id_tisk), ]
  
  # Extract reading type from title
  bill_df$cteni <- case_when(
    str_detect(bill_df$title, "třetí čtení|3\\. čtení")  ~ "3. čtení",
    str_detect(bill_df$title, "druhé čtení|2\\. čtení")   ~ "2. čtení",
    str_detect(bill_df$title, "prvé čtení|první čtení|1\\. čtení") ~ "1. čtení",
    str_detect(bill_df$title, "vrácen")                    ~ "vráceno",
    TRUE ~ "jiné"
  )
  
  message("Found ", nrow(bill_df), " bills in session ", session_num)
  bill_df
}

# ── Get tisky.zip for bill titles ─────────────────────────────────────────────

fetch_tisky <- function() {
  
  message("Downloading tisky.zip...")
  tmp <- tempfile(fileext = ".zip")
  
  request("https://www.psp.cz/eknih/cdrom/opendata/tisky.zip") |>
    req_timeout(300) |>
    req_perform(path = tmp)
  
  extract_dir <- tempdir()
  unzip(tmp, exdir = extract_dir, overwrite = TRUE)
  
  # Read tisky table
  tisky <- read_unl(file.path(extract_dir, "tisky.unl"),
                    cols = c("id_tisk", "id_org", "id_druh", "id_navrh",
                             "id_navrh2", "ct", "navrh_dat", "zaver",
                             "navrh_stav", "znacka", "uplny_naz", "uplny_naz_ascii",
                             "zkr_naz", "zkr_naz_ascii", "popis"))
  
  # Filter to 10th parliament
  tisky[tisky$id_org == "174", ]
}

# ── Main function: get upcoming bills ─────────────────────────────────────────

get_upcoming_votes <- function() {
  
  # Get active sessions
  sessions <- tryCatch(
    get_current_session(),
    error = function(e) {
      message("Could not get sessions: ", e$message)
      return(14:15)  # fallback to known current sessions
    }
  )
  
  # Scrape agenda for each session
  all_bills <- lapply(sessions, scrape_session_agenda)
  all_bills <- do.call(rbind, Filter(Negate(is.null), all_bills))
  
  if (is.null(all_bills) || nrow(all_bills) == 0) {
    message("No bills found in current sessions")
    return(NULL)
  }
  
  message("Total bills across all sessions: ", nrow(all_bills))
  all_bills
}

# ── Match PSP tisk number to VeKLEP PID ──────────────────────────────────────

search_veklep_by_title <- function(title) {
  
  # Extract key words from title — skip common legal boilerplate
  stopwords <- c("návrh", "zákona", "kterým", "mění", "zákon", "znění", 
                 "pozdějších", "předpisů", "vydání", "změně", "dalších",
                 "některých", "souvisejících", "zákonů", "vládní",
                 "poslanců", "senátní", "paragraf", "odst")
  
  words <- str_split(tolower(title), "[^a-záčďéěíňóřšťúůýž0-9]+")[[1]]
  words <- words[nchar(words) > 4 & !words %in% stopwords]
  words <- head(unique(words), 5)  # top 5 meaningful words
  
  if (length(words) == 0) return(NULL)
  
  query <- paste(words, collapse = " ")
  
  url <- paste0("https://www.odok.cz/portal/veklep/hledat?",
                "nazev=", URLencode(query, reserved = TRUE),
                "&typ=&stav=&predkladatel=&od=&do=")
  
  page <- tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0") |>
      req_timeout(15) |>
      req_perform() |>
      resp_body_string() |>
      read_html()
  }, error = function(e) NULL)
  
  if (is.null(page)) return(NULL)
  
  # Extract PIDs from result links
  links <- page |>
    html_elements("a[href*='/portal/veklep/material/']") |>
    html_attr("href")
  
  pids <- str_extract(links, "[A-Z0-9]{12,}") 
  pids <- pids[!is.na(pids)]
  
  if (length(pids) == 0) return(NULL)
  pids[1]  # return best (first) match
}

# Build tisk → PID lookup from VeKLEP JSONs
# Get VeKLEP PID directly from PSP tisk page
# Get VeKLEP PID and PSP parliamentary status from the tisk page
get_veklep_pid_for_tisk <- function(id_tisk) {
  
  url <- paste0("https://www.psp.cz/sqw/historie.sqw?o=10&t=", id_tisk)
  
  page <- tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36") |>
      req_timeout(15) |>
      req_perform() |>
      resp_body_string(encoding = "windows-1250") |>
      read_html()
  }, error = function(e) NULL)
  
  if (is.null(page)) return(NULL)
  
  text <- html_text(page)
  
  # PID
  pid <- str_extract(text, "[A-Z]{5}[A-Z0-9]{7,}")
  if (is.na(pid)) return(NULL)
  
  # PSP status — try terminal states first, then intermediate
  psp_status <- tryCatch({
    
    # Published in Sbírka zákonů
    m <- str_extract(text,
                     "Zákon vyhlášen \\d+\\.\\s*\\d+\\.\\s*\\d{4}.{0,80}pod číslem .{3,40}Sb\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Sent for publication
    m <- str_extract(text, "Schválený zákon odeslán k publikaci.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Senate rejected
    m <- str_extract(text, "Senát návrh zamítl.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Senate returned with amendments
    m <- str_extract(text, "Senát návrh vrátil sněmovně.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Senate approved
    m <- str_extract(text, "Senát.{0,20}(?:schválil|souhlasí).{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Sent to Senate — waiting for Senate (do = deadline)
    m <- str_extract(text,
                     "Zákon doručen Senátu.{0,60}Další projednávání možné do \\d+\\.\\s*\\d+\\.\\s*\\d{4}")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Passed to Senate by PS (without deadline line)
    m <- str_extract(text,
                     "Poslanecká sněmovna postoupila.{0,40}návrh zákona Senátu.{0,60}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # President signed
    m <- str_extract(text, "Prezident.{0,20}podepsal.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Passed by PS (3rd reading)
    m <- str_extract(text, "Návrh zákona schválen.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Non-law reports approved (zpráva schválena)
    m <- str_extract(text, "Zpráva schválena.{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Budget/report rejected or returned
    m <- str_extract(text,
                     "(?:Návrh státního rozpočtu vrácen|návrh zákona zamítnut|vzat zpět navrhovatelem).{0,80}\\.")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    # Waiting for next reading — full date DD. M. YYYY
    m <- str_extract(text,
                     "Další projednávání možné od \\d+\\.\\s*\\d+\\.\\s*\\d{4}\\.?")
    if (!is.na(m)) return(list(pid = pid, psp_status = str_trim(m)))
    
    list(pid = pid, psp_status = NA_character_)
    
  }, error = function(e) list(pid = pid, psp_status = NA_character_))
  
  psp_status
}

# Match all bills by fetching PID from PSP directly
match_tisk_to_veklep <- function(bills) {
  
  message("Fetching VeKLEP PIDs from PSP for ", nrow(bills), " bills...")
  
  bills$veklep_pid  <- NA_character_
  bills$psp_status  <- NA_character_
  
  skip_pattern <- "výroční zpráva|zpráva o|ratifik|smlouva mezi|dohoda o|protokol"
  
  for (i in seq_len(nrow(bills))) {
    
    if (str_detect(tolower(bills$title[i]), skip_pattern)) next
    
    result <- tryCatch(
      get_veklep_pid_for_tisk(bills$id_tisk[i]),
      error = function(e) NULL
    )
    
    if (!is.null(result)) {
      bills$veklep_pid[i] <- result$pid
      bills$psp_status[i] <- result$psp_status %||% NA_character_
      message("  Tisk ", bills$id_tisk[i], " → ", result$pid,
              if (!is.na(result$psp_status)) paste0(" [", result$psp_status, "]") else "")
    }
    
    Sys.sleep(0.3)
  }
  
  matched <- bills[!is.na(bills$veklep_pid), ]
  matched <- matched[!duplicated(matched$veklep_pid), ]
  message("Matched ", nrow(matched), " of ", nrow(bills), " bills")
  matched
}

# Match all bills by fetching PID from PSP directly
match_tisk_to_veklep <- function(bills) {
  
  message("Fetching VeKLEP PIDs from PSP for ", nrow(bills), " bills...")
  
  bills$veklep_pid <- NA_character_
  
  # Skip non-law items
  skip_pattern <- "výroční zpráva|zpráva o|ratifik|smlouva mezi|dohoda o|protokol"
  
  for (i in seq_len(nrow(bills))) {
    
    if (str_detect(tolower(bills$title[i]), skip_pattern)) next
    
    pid <- tryCatch(
      get_veklep_pid_for_tisk(bills$id_tisk[i]),
      error = function(e) NULL
    )
    
    if (!is.null(pid)) {
      bills$veklep_pid[i] <- pid
      message("  Tisk ", bills$id_tisk[i], " → ", pid)
    }
    
    Sys.sleep(0.3)
  }
  
  matched <- bills[!is.na(bills$veklep_pid), ]
  matched <- matched[!duplicated(matched$veklep_pid), ]
  message("Matched ", nrow(matched), " of ", nrow(bills), " bills")
  matched
}