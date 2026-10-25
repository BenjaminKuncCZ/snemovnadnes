# pdf extraction

library(pdftools)
library(stringr)
library(dplyr)

#' Extract and clean text from a PDF

call_gemini <- function(prompt, model = Sys.getenv("GEMINI_MODEL", "gemini-flash-latest")) {
  message("Sending to Gemini (", model, ")...")
  api_key <- Sys.getenv("GEMINI_API_KEY")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                model, ":generateContent?key=", api_key)
  
  request(url) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      contents         = list(list(parts = list(list(text = prompt)))),
      generationConfig = list(temperature = 0.1, maxOutputTokens = 16384)
    )) |>
    req_timeout(90) |>
    req_retry(
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
      max_tries    = 5,
      backoff      = \(i) c(10, 30, 60, 120)[min(i, 4)]
    ) |>
    req_perform() |>
    resp_body_json() |>
    (\(r) r$candidates[[1]]$content$parts[[1]]$text)()
}

read_docx_text <- function(path) {
  # officer only opens files with a .docx extension; some VeKLEP attachments are
  # docx files that were uploaded under a ".doc" name
  if (!str_detect(tolower(path), "\\.docx$")) {
    tmp <- tempfile(fileext = ".docx")
    file.copy(path, tmp)
    on.exit(unlink(tmp), add = TRUE)
    path <- tmp
  }
  doc <- tryCatch(officer::read_docx(path),
                  error = function(e) { message("Docx failed: ", e$message); NULL })
  if (is.null(doc)) return(NULL)
  content <- officer::docx_summary(doc)
  paste(content$text[!is.na(content$text)], collapse = "\n")
}

read_doc_text <- function(path) {
  if (!nzchar(Sys.which("antiword"))) {
    message("antiword not installed — skipping .doc: ", path)
    return(NULL)
  }
  out <- tryCatch(
    system2("antiword", c("-m", "UTF-8.txt", "-w", "0", shQuote(path)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL, warning = function(w) NULL
  )
  # antiword reports its own failures on stdout (exit code 0), e.g.
  # "I'm afraid the text stream of this file is too small to handle."
  if (is.null(out) || length(out) == 0 || any(str_detect(out, "^I'm afraid|is not a Word Document"))) {
    message("antiword could not read: ", path)
    return(NULL)
  }
  paste(out, collapse = "\n")
}

extract_document_text <- function(file_path) {
  
  message("Extracting text from: ", file_path)
  ext  <- tolower(str_extract(file_path, "\\.[a-zA-Z]+$"))
  text <- NULL
  
  if (ext == ".pdf") {
    pages <- tryCatch(pdf_text(file_path),
                      error = function(e) { message("PDF failed: ", e$message); NULL })
    if (is.null(pages)) return(NULL)
    text <- paste(pages, collapse = "\n\n")
    
  } else if (ext %in% c(".docx", ".doc")) {
    is_zip <- identical(readBin(file_path, "raw", 2L), as.raw(c(0x50, 0x4b)))
    if (is_zip)              text <- read_docx_text(file_path)
    else if (ext == ".doc")  text <- read_doc_text(file_path)
    else { message("Not a valid docx: ", file_path); return(NULL) }
    
  } else {
    message("Unsupported file type: ", ext)
    return(NULL)
  }
  
  if (is.null(text) || !nzchar(text)) return(NULL)
  
  text <- text |>
    str_replace_all("[ \\t]{3,}", "  ") |>
    str_replace_all("-\n", "")
  
  pages_split <- str_split(text, "\n")[[1]]
  
  ria_patterns <- c(            # all lower-case: matched against tolower(text)
    "závěrečná zpráva.*hodnocení dopadů",
    "zz ria",
    "závěrečná zpráva ria",
    "identifikace variant",
    "popis variant",
    "varianty řešení",
    "vyhodnocení nákladů a přínosů",
    "identifikace nákladů a přínosů",
    "předpokládaný hospodářský a finanční dopad"
  )
  ria_line <- which(str_detect(tolower(pages_split),
                               paste(ria_patterns, collapse = "|")))[1]
  
  if (!is.na(ria_line)) {
    ria_section <- paste(pages_split[ria_line:length(pages_split)], collapse = "\n")
    message("RIA/variants section starts at line ", ria_line)
  } else {
    ria_section <- text
    message("No RIA heading found, passing the whole document")
  }
  
  list(full_text = text, ria_section = ria_section,
       has_ria = !is.na(ria_line), file_type = ext)
}

# llm extraction

library(httr2)
library(jsonlite)
library(stringr)

# ── prompt ────────────────────────────────────────────────────────────────────

RIA_EXTRACTION_PROMPT <- '
Jsi analytik regulatorních dopadů. Odpověz POUZE validním JSON objektem. Žádný jiný text.

KROK 1 — IDENTIFIKACE VYBRANÉ VARIANTY (proveď nejdřív, před extrakcí)
Závěrečná zpráva RIA obvykle porovnává několik variant řešení (např. "varianta 0 — nulová/nezasahovat",
"varianta 1", "varianta 2" ...). Text níže je dlouhý výňatek z dokumentu; projdi ho celý a najdi:
  a) seznam a stručný popis všech variant (sekce typu "Identifikace variant", "Popis variant", "Návrh variant"),
  b) VARIANTU, KTERÁ BYLA PŘEDKLADATELEM VYBRÁNA / DOPORUČENA. Hledej sekce a formulace jako
     "Výběr nejvhodnější varianty", "Závěr a doporučení", "doporučená varianta", "navrhovaná varianta",
     "preferovaná varianta", "jako vhodnější byla vyhodnocena varianta", "navrhuje se varianta ...",
     "zvolena byla varianta ...", "porovnání variant" a návazná tabulka s hodnocením.
  c) Pokud dokument varianty neobsahuje (např. jde o důvodovou zprávu, transpozici nebo přehled dopadů),
     je "vybranou variantou" samotná NAVRHOVANÁ ÚPRAVA (navrhovaný zákon či novela). Do pole
     "vybrana_varianta" napiš "bez variant (jediná navrhovaná úprava)". Absence variant NEZNAMENÁ, že nejsou
     žádné dopady k extrakci — dopady navrhované úpravy extrahuj podle KROKU 2.
  d) Pokud varianty existují, ale vybraná není z textu jednoznačně patrná, nastav "vybrana_varianta"
     na "nelze určit" a v "poznamky" to vysvětli. NEHÁDEJ a nepředpokládej, že vybraná je poslední varianta.

KROK 2 — EXTRAKCE NÁKLADŮ A PŘÍNOSŮ POUZE PRO VYBRANOU VARIANTU
- Do "naklady" a "prinosy" patří VÝHRADNĚ dopady vybrané (doporučené) varianty.
- NEZAHRNUJ náklady a přínosy ostatních variant, ani variantu 0 (nulovou), pokud nebyla sama vybrána.
- Pokud tabulka srovnává varianty, použij jen sloupec/řádky vybrané varianty.
- Pokud jsou dopady vybrané varianty popsány vzhledem k variantě 0 (rozdíl oproti současnému stavu),
  uveď je tak, jak jsou popsány pro vybranou variantu.
- Nepřičítej ani neodečítej částky mezi variantami. Přebírej částky tak, jak jsou uvedeny pro vybranou variantu.

Pokud dokument NEOBSAHUJE varianty (jediná navrhovaná úprava), extrahuj VŠECHNY dopady navrhované úpravy,
které dokument kdekoli uvádí — i krátké, slovní a nevyčíslené. Projdi zejména části o dopadech na:
státní rozpočet a veřejné rozpočty, obce a kraje, podnikatele a podnikatelské prostředí (včetně
administrativní zátěže), zaměstnavatele a zaměstnance, domácnosti a občany, spotřebitele, sociální dopady,
životní prostředí a ostatní dopady (např. na rovnost žen a mužů, bezpečnost, korupci, ochranu soukromí).
Typické nadpisy: "Předpokládaný hospodářský a finanční dopad", "Dopady na státní rozpočet",
"Hodnocení dopadů regulace", "Sociální dopady", "Dopady na podnikatelské prostředí", "Zhodnocení dopadů".
Věta typu "návrh nemá dopad na státní rozpočet" NENÍ náklad ani přínos — nezapisuj ji jako položku;
pouze ji zmiň v "poznamky".

Pravidla pro pole "typ_dokumentu":
- "RIA" — pokud dokument obsahuje formální Závěrečnou zprávu z hodnocení dopadů regulace (ZZ RIA), tj. strukturované hodnocení s identifikací variant, náklady a přínosy
- "prehled_dopadu" — pokud dokument popisuje dopady navrhované úpravy jakkoli (i stručně, slovně, bez čísel), např. sekcí Zhodnocení dopadů či Předpokládaný hospodářský a finanční dopad v důvodové zprávě (ale NE plnohodnotná ZZ RIA)
- "zadne" — POUZE pokud dokument žádné dopady navrhované úpravy nepopisuje, nebo výslovně uvádí, že návrh nemá žádné dopady
Poznámka: věta, že "RIA nebyla zpracována / se neprovádí", znamená jen, že neexistuje formální ZZ RIA
(typ_dokumentu pak nesmí být "RIA"). Neznamená to "zadne" — pokud dokument dopady popisuje jinde, je to "prehled_dopadu".

Pole "dopady_zhodnoceny" musí být v souladu s "typ_dokumentu":
- true  pokud typ_dokumentu je "RIA" nebo "prehled_dopadu"
- false pokud typ_dokumentu je "zadne"

Pravidla pro extrakci nákladů:
- Do pole "naklady" patří negativní dopady vybrané varianty nebo jejich rizika
- Pokud je částka vyčíslena v Kč nebo EUR, ulož ji jako číslo do pole "castka" (v Kč; mil. = 1 000 000, mld. = 1 000 000 000)
- Pokud náklad není vyčíslen v penězích, nastav "castka" na null a "jistota" na "kvalitativně"
- Popis musí být konkrétní — ne obecný kontext, ale skutečný dopad
- NEOPAKUJ věty z textu doslovně — shrň je vlastními slovy

Pravidla pro extrakci přínosů:
- Do pole "prinosy" patří konkrétní přínosy vybrané varianty
- Pokud přínos nelze vyjádřit v penězích, nastav "monetizovano" na false

Kvalitativní dopady jsou často popsány v textu konzultací nebo hodnocení variant.
Hledej věty jako: přínosem je..., nákladem je..., dopad na...,
zatíží..., ušetří..., zlepší..., omezí....
I pokud nejsou vyjádřeny v Kč, zahrň je jako kvalitativní položky (ale vždy jen pro vybranou variantu).

Vrať tento JSON:
{
  "vybrana_varianta": "např. \'Varianta 2 — zavedení povinného registru\' nebo \'bez variant (jediná navrhovaná úprava)\' nebo \'nelze určit\'",
  "zduvodneni_vyberu": "1 věta: na základě čeho/kde v dokumentu je patrné, že jde o vybranou variantu",
  "oddil_dopadu": "přesný nadpis (max. 120 znaků) hlavního oddílu dokumentu, ze kterého čerpáš náklady a přínosy, např. \'Předpokládaný hospodářský a finanční dopad\'; více oddílů odděl středníkem; pokud žádný, prázdný řetězec",
  "dopady_zhodnoceny": true nebo false,
  "typ_dokumentu": "RIA" nebo "prehled_dopadu" nebo "zadne",
  "shrnutí": "2-3 věty o cíli regulace a o tom, co vybraná varianta znamená",
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
DŮLEŽITÉ: Pokud text říká, že RIA nebyla zpracována, nevyžaduje se nebo se neprovádí, nesmí být
typ_dokumentu "RIA". Pokud dokument dopady navrhované úpravy popisuje jinde (viz výše), použij
"prehled_dopadu" a dopady extrahuj. Teprve pokud dokument žádné dopady nepopisuje, nastav
dopady_zhodnoceny na false A typ_dokumentu na "zadne".

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
                        mp_zip      = "Analyzuješ přílohu (zip), která MŮŽE obsahovat ZZ RIA. Pokud neobsahuje formální ZZ RIA, ale popisuje dopady navrhované úpravy, použij typ_dokumentu \"prehled_dopadu\".",
                        zd_fallback = "DŮLEŽITÉ: Analyzuješ DŮVODOVOU ZPRÁVU (ne samostatnou ZZ RIA). typ_dokumentu \"RIA\" nepoužívej. Pokud důvodová zpráva popisuje dopady navrhované úpravy (i stručně a slovně, např. dopad na státní rozpočet, podnikatele, občany), použij \"prehled_dopadu\" a tyto dopady extrahuj; \"zadne\" jen pokud žádné dopady nepopisuje.",
                        ma_fallback = "DŮLEŽITÉ: Analyzuješ obecný MATERIÁL (ne samostatnou ZZ RIA). typ_dokumentu \"RIA\" nepoužívej. Pokud materiál popisuje dopady navrhované úpravy (i stručně a slovně), použij \"prehled_dopadu\" a tyto dopady extrahuj; \"zadne\" jen pokud žádné dopady nepopisuje.",
                        # default — unknown source
                        "Analyzuješ dokument neznámého typu. Pokud popisuje dopady navrhované úpravy, extrahuj je a použij \"prehled_dopadu\" (nebo \"RIA\" při formální ZZ RIA); \"zadne\" jen pokud žádné dopady nepopisuje."
  )
  
  prompt <- paste0(doc_context, "\n\n", RIA_EXTRACTION_PROMPT, text)
  
  raw <- if (backend == "gemini") {
    call_gemini(prompt, model = model %||% Sys.getenv("GEMINI_MODEL", "gemini-flash-latest"))
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
  
  if (!is.null(result$dopady_zhodnoceny)) {
    result$ria_provedena <- result$dopady_zhodnoceny
  }
  
  # Store the selected variant in poznamky
  varianta <- result$vybrana_varianta
  if (!is.null(varianta) && is.character(varianta) && nzchar(varianta)) {
    zduv <- result$zduvodneni_vyberu
    oddil <- result$oddil_dopadu
    prefix <- paste0("Vybraná varianta: ", varianta,
                     if (is.character(zduv) && nzchar(zduv)) paste0(" (", zduv, ")") else "",
                     ".",
                     if (is.character(oddil) && nzchar(oddil)) paste0(" Zdroj dopadů: ", oddil, ".") else "")
    result$poznamky <- trimws(paste(prefix, result$poznamky %||% ""))
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
  keywords_high <- c("náklad", "přínos",  "mil\\.", "mld\\.", 
                     "státní rozpočet", "vyčíslen", "finanční dopad",
                     "hospodářský dopad", "kč", "celkem", "ročně", "jednorázov")
  
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

extract_section_by_heading <- function(text, max_chars = 60000) {
  
  lines <- str_split(text, "\n")[[1]]
  
  toc_lines <- str_detect(lines, "PAGEREF|\\\\h |TOC \\\\")
  last_toc_line <- max(which(toc_lines), 0)
  if (last_toc_line > 0) {
    message("Skipping TOC (", last_toc_line, " lines)")
    lines <- lines[(last_toc_line + 1):length(lines)]
  }
  
  low   <- tolower(lines)
  short <- nchar(str_trim(lines)) < 160
  num   <- "^\\s*(\\d+(\\.\\d+)*\\.?\\s*)?"
  
  variant_pattern <- paste0(num, "(identifikace variant|popis variant|návrh variant|",
                            "varianty řešení|variantní řešení|varianta\\s*[01]\\b)")
  cb_pattern <- paste0(num, "(vyhodnocení nákladů a přínosů|identifikace nákladů a přínosů|",
                       "předpokládaný hospodářský a finanční dopad|dopady? na státní rozpočet|",
                       "hodnocení dopadů)")
  
  start_variant <- which(short & str_detect(low, variant_pattern))[1]
  start_cb      <- which(short & str_detect(low, cb_pattern))[1]
  starts <- c(start_variant, start_cb)
  starts <- starts[!is.na(starts)]
  
  if (length(starts) > 0) {
    section_start <- min(starts)
    message("Section starts at line ", section_start, ": ", str_trim(lines[section_start]))
    end_line <- min(section_start + 1500, length(lines))
    relevant <- paste(lines[section_start:end_line], collapse = "\n")
  } else {
    full <- paste(lines, collapse = "\n")
    if (nchar(full) <= max_chars) {
      message("No section heading found, document fits — using all of it")
      relevant <- full
    } else {
      message("No section heading found, using keyword chunking")
      relevant <- extract_relevant_chunks(full, max_chars = max_chars)
    }
  }
  
  if (nchar(relevant) > max_chars) relevant <- str_sub(relevant, 1, max_chars)
  message("Final text length: ", nchar(relevant), " chars")
  relevant
}
