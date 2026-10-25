library(httr2)
library(rvest)
library(xml2)
library(pdftools)
library(stringr)

# ── 1. RSS feed: get latest materials ────────────────────────────────────────

fetch_latest_materials <- function(n = 40) {
  
  message("Fetching VeKLEP RSS feed...")
  
  rss <- request("https://www.odok.cz/portal/veklep/rss/novinky") |>
    req_headers("User-Agent" = "Mozilla/5.0 (compatible; SnemovniDopady/1.0)") |>
    req_perform() |>
    resp_body_string() |>
    read_xml()
  
  # Parse RSS items
  items <- xml_find_all(rss, ".//item")
  
  materials <- lapply(items[seq_len(min(n, length(items)))], function(item) {
    list(
      title       = xml_text(xml_find_first(item, "title")),
      url         = xml_text(xml_find_first(item, "link")),
      pid         = xml_text(xml_find_first(item, "guid")),
      predkladatel = xml_text(xml_find_first(item, "description")) |>
        str_extract("Předkladatel:\\s*(\\w+)", group = 1),
      published   = xml_text(xml_find_first(item, "pubDate"))
    )
  })
  
  message("Found ", length(materials), " materials")
  materials
}

# ── 2. Get documents from JSON API ───────────────────────────────────────────

get_material_documents <- function(pid) {
  
  url <- paste0("https://www.odok.cz/portal/veklep/material/", pid, "/json")
  message("Fetching material JSON: ", pid)
  
  result <- tryCatch({
    request(url) |>
      req_headers("User-Agent" = "Mozilla/5.0 (compatible; SnemovniDopady/1.0)") |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    message("Failed to fetch JSON: ", e$message)
    return(NULL)
  })
  
  if (is.null(result)) return(NULL)
  
  if (length(result$attachments) == 0) {
    message("No attachments in material JSON for ", pid,
            " — top-level keys: ", paste(names(result), collapse = ", "))
    return(data.frame())          # find_ria_document() returns NULL -> "no_document"
  }
  
  attachments <- lapply(result$attachments, function(a) {
    data.frame(
      pid       = a$pid,
      name      = a$name,
      old_name  = a$old_name,
      type_id   = a$type$id,
      type_name = a$type$name,
      size      = a$size,
      uri       = a$uri,
      stringsAsFactors = FALSE
    )
  })
  
  docs <- do.call(rbind, attachments)
  message("Found ", nrow(docs), " attachments: ", paste(docs$type_id, collapse = ", "))
  docs
}

# ── 3. Find the RIA document ──────────────────────────────────────────────────

find_ria_document <- function(docs) {
  
  if (is.null(docs) || nrow(docs) == 0) return(NULL)
  
  # Priority 1: file explicitly named as Závěrečná zpráva z hodnocení dopadů
  # This is the only reliable indicator of a proper RIA document
  ria_title_pattern <- "záv.*zpr.*hodnocen|hodnocen.*dopad.*regul|zz.ria|závěrečná.*ria"
  ria_match <- docs[str_detect(tolower(docs$old_name), ria_title_pattern), ]
  if (nrow(ria_match) > 0) {
    message("RIA found by filename: ", ria_match$old_name[1])
    return(list(uri = ria_match$uri[1], type = "ria_keyword",
                name = ria_match$old_name[1]))
  }
  
  # Priority 2: document type code is explicitly 'ria'
  ria_type <- docs[!is.na(docs$type_id) & tolower(docs$type_id) == "ria", ]
  if (nrow(ria_type) > 0) {
    message("RIA found by type code: ", ria_type$old_name[1])
    return(list(uri = ria_type$uri[1], type = "ria_type",
                name = ria_type$old_name[1]))
  }
  
  # Priority 3: mp (příloha) — may contain RIA as zip
  mp_match <- docs[docs$type_id == "mp", ]
  if (nrow(mp_match) > 0) {
    message("Using mp (příloha): ", mp_match$old_name[1])
    return(list(uri = mp_match$uri[1], type = "mp_zip",
                name = mp_match$old_name[1]))
  }
  
  # Priority 4: zd (důvodová zpráva) — fallback only, not RIA
  zd_match <- docs[docs$type_id == "zd", ]
  if (nrow(zd_match) > 0) {
    message("No RIA found, falling back to důvodová zpráva: ", zd_match$old_name[1])
    return(list(uri = zd_match$uri[1], type = "zd_fallback",
                name = zd_match$old_name[1]))
  }
  
  # Priority 5: ma (materiál) as last resort
  ma_match <- docs[docs$type_id == "ma", ]
  if (nrow(ma_match) > 0) {
    message("Last resort - using ma: ", ma_match$old_name[1])
    return(list(uri = ma_match$uri[1], type = "ma_fallback",
                name = ma_match$old_name[1]))
  }
  
  NULL
}

# ── 4. Download PDF ────────────────────────────────────────────────────────────

download_document <- function(ria_doc, dest_dir = "data/pdfs") {
  
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  att_id <- str_extract(ria_doc$uri, "[A-Za-z0-9]{8,}(?=/?$)")
  if (is.na(att_id)) att_id <- format(Sys.time(), "%H%M%S")
  clean_name <- str_replace_all(ria_doc$name, "[/\\\\:*?\"<>|]", "_")
  safe_name  <- paste0(att_id, "_", clean_name)
  dest_path  <- file.path(dest_dir, safe_name)
  
  # throw away broken leftovers
  if (file.exists(dest_path) && file.size(dest_path) < 1000) file.remove(dest_path)
  
  if (!file.exists(dest_path)) {
    message("Downloading: ", ria_doc$uri)
    ok <- tryCatch({
      request(ria_doc$uri) |>
        req_headers("User-Agent" = "Mozilla/5.0 (compatible; SnemovniDopady/1.0)") |>
        req_timeout(60) |>
        req_retry(max_tries = 3, backoff = ~ 5) |>
        req_perform(path = dest_path)
      TRUE
    }, error = function(e) {
      message("Download failed: ", e$message)
      FALSE
    })
    if (!ok || !file.exists(dest_path) || file.size(dest_path) < 1000) {
      if (file.exists(dest_path)) file.remove(dest_path)
      message("Download failed or file too small: ", safe_name)
      return(NULL)
    }
  } else {
    message("Already downloaded: ", dest_path)
  }
  
  if (str_detect(dest_path, "\\.zip$")) {
    message("Extracting zip...")
    extract_dir <- file.path(dest_dir, str_remove(safe_name, "\\.zip$"))
    dir.create(extract_dir, showWarnings = FALSE)
    unzip(dest_path, exdir = extract_dir, overwrite = TRUE)
    
    inner_files <- list.files(extract_dir,
                              pattern = "\\.(pdf|docx?)$",
                              full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    message("Files in zip: ", paste(basename(inner_files), collapse = ", "))
    if (length(inner_files) == 0) {
      message("No usable files found in zip")
      return(NULL)
    }
    ria_inner <- inner_files[str_detect(tolower(inner_files),
                                        "ria|dopad|hodnocen|naklad|prinosy")]
    chosen <- if (length(ria_inner) > 0) ria_inner[1] else inner_files[1]
    message("Using from zip: ", basename(chosen))
    return(list(path = chosen, type = ria_doc$type))
  }
  
  list(path = dest_path, type = ria_doc$type)
}

get_material_metadata <- function(pid) {
  
  url <- paste0("https://www.odok.cz/portal/veklep/material/", pid, "/json")
  
  result <- tryCatch({
    request(url) |>
      req_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        "Referer"    = "https://www.odok.cz/portal/veklep/materialy",
        "Accept"     = "application/json, text/plain, */*"
      ) |>
      req_timeout(15) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    message("WARN: Metadata fetch failed for ", pid, ": ", e$message)
    NULL
  })
  
  if (is.null(result)) return(NULL)
  
  status_id   <- result$status$id   %||% ""
  status_name <- result$status$name %||% ""
  
  if (status_id == "" || status_name == "") {
    message("WARN: empty status fields for ", pid,
            " — top-level keys: ", paste(names(result), collapse = ", "),
            " — result$status class: ", class(result$status))
  }
  
  # veklep_modified: try the most common field names for the last-modified timestamp.
  # Once you can see the top-level keys in the log above, pick the right one.
  veklep_modified <- result$modified %||%
    result$lastModified %||%
    result$updated_at %||%
    result$dateModified %||% ""
  
  list(
    status_id       = status_id,
    status_name     = status_name,
    description     = result$description             %||% "",
    government_date = result$government_program$date %||% "",
    veklep_modified = veklep_modified
  )
}