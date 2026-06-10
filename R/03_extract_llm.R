# pdf extraction

library(pdftools)
library(stringr)
library(dplyr)

#' Extract and clean text from a PDF

call_gemini <- function(prompt, model = "gemini-2.5-flash") {
  
  message("Sending to Gemini (", model, ")...")
  
  api_key <- Sys.getenv("GEMINI_API_KEY")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                model, ":generateContent?key=", api_key)
  
  resp <- request(url) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(parts = list(list(text = prompt)))
      ),
      generationConfig = list(
        temperature     = 0.1,
        maxOutputTokens = 2048
      )
    )) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_json()
  
  resp$candidates[[1]]$content$parts[[1]]$text
}

#' Returns a list with: full_text, ria_section, costs_section
extract_document_text <- function(file_path) {
  
  message("Extracting text from: ", file_path)
  ext <- tolower(str_extract(file_path, "\\.[a-zA-Z]+$"))
  
  text <- if (ext == ".pdf") {
    
    pages <- tryCatch(
      pdf_text(file_path),
      error = function(e) { message("PDF failed: ", e$message); NULL }
    )
    if (is.null(pages)) return(NULL)
    paste(pages, collapse = "\n\n")
    
  } else if (ext %in% c(".docx", ".doc")) {
    
    # Skip .doc files — LibreOffice not available for conversion
    if (ext == ".doc") {
      message("Skipping .doc file (conversion not available): ", file_path)
      return(NULL)
    }
    
    doc <- tryCatch(
      officer::read_docx(file_path),
      error = function(e) { message("Docx failed: ", e$message); NULL }
    )
    if (is.null(doc)) return(NULL)
    
    content <- officer::docx_summary(doc)
    paste(content$text[!is.na(content$text)], collapse = "\n")
    } else {
    message("Unsupported file type: ", ext)
    return(NULL)
  }
  
  # Clean up
  text <- text |>
    str_replace_all("\\s{3,}", "  ") |>
    str_replace_all("-\n", "")
  
  # Find RIA section
  pages_split <- str_split(text, "\n")[[1]]
  ria_patterns <- c(
    "závěrečná zpráva.*hodnocení dopadů",
    "ZZ RIA",
    "závěrečná zpráva z hodnocení dopadů regulace",
    "závěrečná zpráva ria",  
    "vyhodnocení nákladů a přínosů",           # new
    "identifikace nákladů a přínosů",          # new
    "vyhodnocení nákladů a přínosů variant"    # new
  )
  pattern <- paste(ria_patterns, collapse = "|")
  ria_line <- which(str_detect(tolower(pages_split), pattern))[1]
  
  if (!is.na(ria_line)) {
    end_line <- min(ria_line + 500, length(pages_split))
    ria_section <- paste(pages_split[ria_line:end_line], collapse = "\n")
    message("RIA section found at line ", ria_line)
  } else {
    start <- floor(length(pages_split) * 0.6)
    ria_section <- paste(pages_split[start:length(pages_split)], collapse = "\n")
    message("No RIA section found, using last 40% of document")
  }
  
  list(
    full_text   = text,
    ria_section = ria_section,
    has_ria     = !is.na(ria_line),
    file_type   = ext
  )
}

# llm extraction

library(httr2)
library(jsonlite)
library(stringr)

# ── prompt ────────────────────────────────────────────────────────────────────

RIA_EXTRACTION_PROMPT <- '
Jsi analytik regulatorních dopadů. Odpověz POUZE validním JSON objektem. Žádný jiný text.

Pravidla pro pole "typ_dokumentu":
- "RIA" — pokud dokument obsahuje formální Závěrečnou zprávu z hodnocení dopadů regulace (ZZ RIA), tj. strukturované hodnocení s identifikací variant, náklady a přínosy
- "prehled_dopadu" — pokud dokument obsahuje pouze stručný přehled dopadů, přehled dopadů regulace, nebo sekci Zhodnocení dopadů v důvodové zprávě (ale NE plnohodnotnou ZZ RIA)
- "zadne" — pokud dokument neobsahuje žádné hodnocení dopadů, nebo explicitně uvádí že RIA nebyla zpracována

Pole "dopady_zhodnoceny" musí být v souladu s "typ_dokumentu":
- true  pokud typ_dokumentu je "RIA" nebo "prehled_dopadu"
- false pokud typ_dokumentu je "zadne"

Pravidla pro extrakci nákladů:
- Do pole "naklady" patří negativní dopady regulace nebo jejich rizika
- Pokud je částka vyčíslena v Kč nebo EUR, ulož ji jako číslo do pole "castka"
- Pokud náklad není vyčíslen v penězích, nastav "castka" na null a "jistota" na "kvalitativně"
- Popis musí být konkrétní — ne obecný kontext, ale skutečný dopad
- NEOPAKUJ věty z textu doslovně — shrň je vlastními slovy

Pravidla pro extrakci přínosů:
- Do pole "prinosy" patří konkrétní přínosy regulace
- Pokud přínos nelze vyjádřit v penězích, nastav "monetizovano" na false

Kvalitativní dopady jsou často popsány v textu konzultací nebo hodnocení variant.
Hledej věty jako: přínosem je..., nákladem je..., dopad na...,
zatíží..., ušetří..., zlepší..., omezí....
I pokud nejsou vyjádřeny v Kč, zahrň je jako kvalitativní položky.

Vrať tento JSON:
{
  "dopady_zhodnoceny": true nebo false,
  "typ_dokumentu": "RIA" nebo "prehled_dopadu" nebo "zadne",
  "shrnutí": "2-3 věty o cíli regulace",
  "naklady": [
    {
      "subjekt": "kdo nese náklad (státní rozpočet / obce / podnikatelé / domácnosti / zaměstnavatelé / spotřebitelé / zaměstnanci / životní prostředí / jiné)",
      "castka": číslo v Kč nebo null,
      "periodicita": "jednorázově" nebo "ročně" nebo "neznámá",
      "jistota": "přesně" nebo "odhad" nebo "kvalitativně",
      "popis": "konkrétní popis nákladu vlastními slovy"
    }
  ],
  "prinosy": [
  {
    "subjekt": "kdo získává přínos — POVINNÉ POLE: státní rozpočet / obce / podnikatelé / domácnosti / zaměstnavatelé / spotřebitelé / zaměstnanci / životní prostředí / jiné",
    "monetizovano": true nebo false,
    "castka": číslo v Kč nebo null,
    "popis": "konkrétní popis přínosu vlastními slovy"
  }
],
  "poznamky": "upozornění nebo nejasnosti"
}

Každá položka v "prinosy" musí mít vyplněné pole "subjekt". Nikdy ho nevynechej.
DŮLEŽITÉ: Pokud text říká RIA nebyla zpracována, nevyžaduje RIA, RIA se neprovádí nebo podobně,
nastav dopady_zhodnoceny na false A typ_dokumentu na "zadne".

Výstup musí být česky.

TEXT:
'

# ── main extraction function ──────────────────────────────────────────────────

extract_impacts <- function(text, 
                            backend  = Sys.getenv("LLM_BACKEND", "ollama"),
                            model    = NULL,
                            doc_type = "unknown") {   # <-- add parameter
  
  # Build a document-type preamble so the LLM calibrates its confidence
  doc_context <- switch(doc_type,
                        ria_keyword = "Analyzuješ ZÁVĚREČNOU ZPRÁVU Z HODNOCENÍ DOPADŮ REGULACE (ZZ RIA). Jde o plnohodnotný RIA dokument.",
                        ria_type    = "Analyzuješ ZÁVĚREČNOU ZPRÁVU Z HODNOCENÍ DOPADŮ REGULACE (ZZ RIA). Jde o plnohodnotný RIA dokument.",
                        mp_zip      = "Analyzuješ přílohu (zip), která MŮŽE obsahovat RIA. Nastav dopady_zhodnoceny na true pouze pokud text explicitně obsahuje sekci hodnocení nákladů a přínosů.",
                        zd_fallback = "DŮLEŽITÉ: Analyzuješ DŮVODOVOU ZPRÁVU (ne RIA dokument). Důvodová zpráva není RIA. Nastav dopady_zhodnoceny na false, pokud text neobsahuje explicitní tabulku nebo výčet vyčíslených nákladů a přínosů. Krátký odstavec o dopadu na státní rozpočet nestačí.",
                        ma_fallback = "DŮLEŽITÉ: Analyzuješ obecný MATERIÁL (ne RIA dokument). Nastav dopady_zhodnoceny na false, pokud text neobsahuje explicitní tabulku nebo výčet vyčíslených nákladů a přínosů.",
                        # default — unknown source, be conservative
                        "Analyzuješ dokument neznámého typu. Nastav dopady_zhodnoceny na true pouze pokud text obsahuje explicitní sekci hodnocení nákladů a přínosů s konkrétními položkami."
  )
  
  prompt <- paste0(doc_context, "\n\n", RIA_EXTRACTION_PROMPT, text)
  
  raw <- if (backend == "gemini") {
    call_gemini(prompt, model = model %||% "gemini-2.5-flash")
  } else if (backend == "claude") {
    call_claude_api(prompt)
  } else {
    call_ollama(prompt, model = model %||% "mistral")
  }
  
  parse_llm_response(raw)
}

call_ollama <- function(prompt, model = "mistral") {
  
  message("Sending to Ollama (", model, ")...")
  
  resp <- request("http://localhost:11434/api/chat") |>
    req_body_json(list(
      model   = model,
      stream  = FALSE,
      options = list(temperature = 0.1),  # low temp for structured extraction
      messages = list(
        list(role = "user", content = prompt)
      )
    )) |>
    req_timeout(300) |>   # give Mistral time to think
    req_perform() |>
    resp_body_json()
  
  resp$message$content
}

call_claude_api <- function(prompt) {
  
  message("Sending to Claude API...")
  
  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = "claude-haiku-4-5-20251001",  # cheapest, fast, good enough
      max_tokens = 1500,
      messages   = list(
        list(role = "user", content = prompt)
      )
    )) |>
    req_perform() |>
    resp_body_json()
  
  resp$content[[1]]$text
}

parse_llm_response <- function(raw_text) {
  
  cleaned <- raw_text |>
    str_replace_all("```json|```", "") |>
    str_trim()
  
  result <- tryCatch(
    fromJSON(cleaned, simplifyDataFrame = FALSE),
    error = function(e) {
      message("JSON parse failed: ", e$message)
      message("Raw response was:\n", raw_text)
      NULL
    }
  )
  
  if (is.null(result)) return(NULL)
  
  # ── Fix 1: normalize field name ──────────────────────────────────────────
  # Handle both dopady_zhodnoceny (new prompt) and ria_provedena (old prompt)
  if (!is.null(result$dopady_zhodnoceny)) {
    result$ria_provedena <- result$dopady_zhodnoceny
  }
  

  result
}

trim_to_context <- function(text, max_chars = 12000) {
  if (nchar(text) > max_chars) {
    message("Trimming text from ", nchar(text), " to ", max_chars, " chars")
    str_sub(text, 1, max_chars)
  } else {
    text
  }
}

extract_relevant_chunks <- function(text, max_chars = 6000) {
  
  lines <- str_split(text, "\n")[[1]]
  lines <- lines[nchar(str_trim(lines)) > 0]  # remove empty lines
  
  # Score each line by relevance to costs/benefits
  keywords_high <- c("náklad", "přínos", "kč", "mil\\.", "mld\\.", 
                     "státní rozpočet", "vyčíslen", "finanční dopad",
                     "hospodářský dopad", "celkem", "ročně", "jednorázov")
  
  keywords_med  <- c("dopad", "regulac", "povinnost", "subjekt",
                     "podnikatel", "domácnost", "zaměstnan", "občan")
  
  scores <- sapply(lines, function(line) {
    l <- tolower(line)
    high <- sum(sapply(keywords_high, function(k) str_count(l, k))) * 3
    med  <- sum(sapply(keywords_med,  function(k) str_count(l, k))) * 1
    high + med
  })
  
  # Always include lines around high-scoring lines (context window of ±3 lines)
  top_indices <- which(scores > 0)
  context_indices <- unique(sort(unlist(lapply(top_indices, function(i) {
    max(1, i-3):min(length(lines), i+3)
  }))))
  
  relevant_lines <- lines[context_indices]
  relevant_text  <- paste(relevant_lines, collapse = "\n")
  
  message("Reduced from ", nchar(text), " to ", nchar(relevant_text), " chars")
  
  # Final trim if still too long
  if (nchar(relevant_text) > max_chars) {
    relevant_text <- str_sub(relevant_text, 1, max_chars)
  }
  
  relevant_text
}

extract_section_by_heading <- function(text, max_chars = 6000) {
  
  lines <- str_split(text, "\n")[[1]]
  
  # Skip table of contents — it ends when we see the first real heading repeated
  # TOC lines are recognizable by PAGEREF patterns
  toc_lines <- str_detect(lines, "PAGEREF|\\\\h |TOC \\\\")
  last_toc_line <- max(which(toc_lines), 0)
  
  if (last_toc_line > 0) {
    message("Skipping TOC (", last_toc_line, " lines)")
    lines <- lines[(last_toc_line + 1):length(lines)]
  }
  
  # Now find the costs/benefits section by heading
  section_patterns <- c(
    "vyhodnocení nákladů a přínosů variant",   # most specific — check first
    "vyhodnocení nákladů a přínosů",
    "identifikace nákladů a přínosů",
    "3\\. náklad",
    "3\\.1",
    "3\\.2"
  )
  
  pattern <- paste(section_patterns, collapse = "|")
  section_start <- which(str_detect(tolower(lines), pattern))[1]
  
  if (!is.na(section_start)) {
    message("Found costs/benefits section at line ", section_start, 
            ": ", str_trim(lines[section_start]))
    # Extract from that heading onwards
    end_line <- min(section_start + 300, length(lines))
    relevant <- paste(lines[section_start:end_line], collapse = "\n")
  } else {
    message("No section heading found, using keyword chunking")
    relevant <- paste(lines, collapse = "\n")
  }
  
  # Final trim
  if (nchar(relevant) > max_chars) {
    relevant <- str_sub(relevant, 1, max_chars)
  }
  
  message("Final text length: ", nchar(relevant), " chars")
  relevant
}

call_gemini <- function(prompt, model = "gemini-2.5-flash") {
  
  message("Sending to Gemini (", model, ")...")
  
  api_key <- Sys.getenv("GEMINI_API_KEY")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                model, ":generateContent?key=", api_key)
  
  resp <- request(url) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents = list(
        list(parts = list(list(text = prompt)))
      ),
      generationConfig = list(
        temperature     = 0.1,
        maxOutputTokens = 8192
      )
    )) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_json()
  
  resp$candidates[[1]]$content$parts[[1]]$text
}