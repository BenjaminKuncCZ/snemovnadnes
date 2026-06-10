# ── 0. Load scripts ───────────────────────────────────────────────────────────
source("R/02_crawl_documents.R")
source("R/03_extract_llm.R")

# ── 1. Fetch latest materials from RSS ────────────────────────────────────────
materials <- fetch_latest_materials(n = 20)

for (m in materials) cat(m$pid, "|", m$title, "\n")

# ── 2. Pick a material ────────────────────────────────────────────────────────
test_pid <- "KORNDSJHRRGS"  # change this to whichever PID you want to test

# ── 3. Get document list from JSON API ────────────────────────────────────────
docs <- get_material_documents(test_pid)
print(docs)

# ── 4. Find the RIA document ──────────────────────────────────────────────────
ria_doc <- find_ria_document(docs)
cat("Document found:", ria_doc$name, "(", ria_doc$type, ")\n")

# ── 5. Download it ────────────────────────────────────────────────────────────
downloaded <- download_document(ria_doc)
cat("Local path:", downloaded$path, "\n")

# ── 6. Extract text ───────────────────────────────────────────────────────────
extracted <- extract_document_text(downloaded$path)

cat("Has RIA section:", extracted$has_ria, "\n")
cat("File type:", extracted$file_type, "\n")
cat("\n--- First 500 chars of RIA section ---\n")
cat(str_sub(extracted$ria_section, 1, 500))

# ── 7. Trim text and send to LLM ─────────────────────────────────────────────
text_for_llm <- extract_section_by_heading(extracted$ria_section, max_chars = 6000)
cat("\n--- Section preview ---\n")
cat(str_sub(text_for_llm, 1, 500))

result <- extract_impacts(text_for_llm)

# ── 8. Print results ──────────────────────────────────────────────────────────
cat("\n\n=== VÝSLEDEK ANALÝZY ===\n")
cat("RIA provedena:", result$ria_provedena, "\n")
cat("Typ dokumentu:", result$typ_dokumentu, "\n")

# Handle missing summary
if (is.null(result$shrnutí) || result$shrnutí == "") {
  cat("Shrnutí: Shrnutí nebylo nalezeno.\n\n")
} else {
  cat("Shrnutí:", result$shrnutí, "\n\n")
}

# Handle costs
cat("NÁKLADY:\n")
if (length(result$naklady) == 0) {
  cat("  Předpokládaný hospodářský dopad na státní rozpočet a veřejné rozpočty nebyl vyčíslen nebo není relevantní.\n")
} else {
  for (n in result$naklady) {
    castka_text <- if (is.null(n$castka) || n$castka == "") {
      "kvalitativně"
    } else {
      paste(format(n$castka, big.mark = " "), "Kč")
    }
    cat(" -", n$subjekt, ":", castka_text, "\n")
    cat("   ", n$popis, "\n")
  }
}

# Handle benefits
cat("\nPŘÍNOSY:\n")
if (length(result$prinosy) == 0) {
  cat("  Předpokládané přínosy nebyly vyčísleny nebo nejsou relevantní.\n")
} else {
  for (p in result$prinosy) {
    monetizovano <- ifelse(isTRUE(p$monetizovano), 
                           paste(format(p$castka, big.mark = " "), "Kč"), 
                           "nevyčísleno")
    cat(" -", p$popis, "(", monetizovano, ")\n")
  }
}

# Overall verdict
cat("\nZÁVĚR:\n")
if (!isTRUE(result$ria_provedena) && length(result$naklady) == 0) {
  cat("  Tento právní předpis nemá vyčíslené dopady — buď nebyla zpracována RIA,",
      "nebo předkladatel uvádí, že regulace nemá finanční dopady na veřejné rozpočty.\n")
} else if (!isTRUE(result$ria_provedena)) {
  cat("  RIA nebyla provedena. Odhadnuté dopady buď pochází z přehledu dopadů\n
      nebo jde o kvalitativní odhad.")
} else {
  cat("  RIA byla provedena.\n")
}

result <- extract_impacts(text_for_llm)

# Debug: check what the LLM actually returned for prinosy
cat("\n--- PRINOSY DEBUG ---\n")
for (p in result$prinosy) {
  cat("subjekt:", p$subjekt %||% "(missing)", "\n")
  cat("popis:  ", p$popis   %||% "(missing)", "\n\n")
}

# ── 9. Save to database ───────────────────────────────────────────────────────
source("R/04_database.R")

db <- get_db()
init_db(db)

# Save the bill metadata
save_bill(db, materials[[14]])  # adjust index to match your test bill

# Save the extraction result
save_impact(db, test_pid, result, downloaded$path)

# Verify it saved correctly
cat("\n=== DATABASE CHECK ===\n")
print(get_all_bills(db))
cat("\nCosts:\n")
print(get_bill_costs(db, test_pid))
cat("\nBenefits:\n")
print(get_bill_benefits(db, test_pid))

dbDisconnect(db)
