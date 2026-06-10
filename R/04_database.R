library(DBI)
library(jsonlite)
library(stringr)

# ── Connect ───────────────────────────────────────────────────────────────────

get_db <- function() {
  library(RPostgres)
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST",     "aws-0-eu-west-1.pooler.supabase.com"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname   = Sys.getenv("SUPABASE_DB",       "postgres"),
    user     = Sys.getenv("SUPABASE_USER",     "postgres.ktehtgonacsexkglnohi"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
}
  
  # Local development — use .Renviron
  library(RPostgres)
  dbConnect(
    RPostgres::Postgres(),
    host     = host,
    port     = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname   = Sys.getenv("SUPABASE_DB", "postgres"),
    user     = Sys.getenv("SUPABASE_USER", "postgres"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
}

# ── Create tables ─────────────────────────────────────────────────────────────

init_db <- function(db) {
  
  # Detect DB type
  is_postgres <- inherits(db, "PqConnection")
  
  if (is_postgres) {
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS bills (
        pid           TEXT PRIMARY KEY,
        title         TEXT,
        predkladatel  TEXT,
        published     TEXT,
        processed_at  TEXT
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS impacts (
        id              SERIAL PRIMARY KEY,
        pid             TEXT,
        ria_provedena   INTEGER,
        typ_dokumentu   TEXT,
        shrnutí         TEXT,
        poznamky        TEXT,
        doc_path        TEXT,
        raw_json        TEXT,
        created_at      TEXT,
        FOREIGN KEY (pid) REFERENCES bills(pid)
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS costs (
        id            SERIAL PRIMARY KEY,
        impact_id     INTEGER,
        pid           TEXT,
        subjekt       TEXT,
        castka        REAL,
        periodicita   TEXT,
        jistota       TEXT,
        popis         TEXT,
        FOREIGN KEY (impact_id) REFERENCES impacts(id)
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS benefits (
        id            SERIAL PRIMARY KEY,
        impact_id     INTEGER,
        pid           TEXT,
        subjekt       TEXT,          
        monetizovano  INTEGER,
        castka        REAL,
        popis         TEXT,
        FOREIGN KEY (impact_id) REFERENCES impacts(id)
      )
    ")
    
  } else {
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS bills (
        pid           TEXT PRIMARY KEY,
        title         TEXT,
        predkladatel  TEXT,
        published     TEXT,
        processed_at  TEXT
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS impacts (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        pid             TEXT,
        ria_provedena   INTEGER,
        typ_dokumentu   TEXT,
        shrnutí         TEXT,
        poznamky        TEXT,
        doc_path        TEXT,
        raw_json        TEXT,
        created_at      TEXT,
        FOREIGN KEY (pid) REFERENCES bills(pid)
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS costs (
        id            INTEGER PRIMARY KEY AUTOINCREMENT,
        impact_id     INTEGER,
        pid           TEXT,
        subjekt       TEXT,
        castka        REAL,
        periodicita   TEXT,
        jistota       TEXT,
        popis         TEXT,
        FOREIGN KEY (impact_id) REFERENCES impacts(id)
      )
    ")
    
    dbExecute(db, "
      CREATE TABLE IF NOT EXISTS benefits (
        id            INTEGER PRIMARY KEY AUTOINCREMENT,
        impact_id     INTEGER,
        pid           TEXT,
        subjekt       TEXT,          
        monetizovano  INTEGER,
        castka        REAL,
        popis         TEXT,
        FOREIGN KEY (impact_id) REFERENCES impacts(id)
      )
    ")
  }
  
  # ══════════════════════════════════════════════════════════════════════════════
  
  # ── Alerts table (email notification subscriptions) ──────────────────────
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS alerts (
      id            SERIAL PRIMARY KEY,
      email         TEXT NOT NULL,
      predkladatel  TEXT,
      keyword       TEXT,
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
  
  # ── Alerts table ─────────────────────────────────────────────────────────
  dbExecute(db, "
    CREATE TABLE IF NOT EXISTS alerts (
      id            SERIAL PRIMARY KEY,
      email         TEXT NOT NULL,
      predkladatele TEXT,        -- comma-separated codes: 'MF,MPSV'
      subjekty      TEXT,        -- comma-separated subject types: 'podnikatelé,domácnosti'
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
  
  # Add PSP parliamentary status column (safe to run on existing DB)
  tryCatch(
    dbExecute(db, "ALTER TABLE bills ADD COLUMN IF NOT EXISTS psp_status TEXT"),
    error = function(e) invisible(NULL)
  )
  
  message("Database initialized")   # ← already there; keep it last
  
  # ══════════════════════════════════════════════════════════════════════════════
  # That is the only change needed in 04_database.R.
  # The alert CRUD functions live in 10_alerts.R.
  # ══════════════════════════════════════════════════════════════════════════════
}

# ── Save a bill ───────────────────────────────────────────────────────────────

save_bill <- function(db, material) {
  
  existing <- dbGetQuery(db,
                         "SELECT pid FROM bills WHERE pid = $1",
                         params = list(material$pid)
  )
  
  if (nrow(existing) > 0) {
    dbExecute(db, "
      UPDATE bills 
      SET id_tisk         = COALESCE(id_tisk, $1),
          session         = COALESCE(session, $2),
          status_id       = CASE WHEN $3 <> '' THEN $3 ELSE status_id END,
          status_name     = CASE WHEN $4 <> '' THEN $4 ELSE status_name END,
          description     = CASE WHEN $5 <> '' THEN $5 ELSE description END,
          government_date = CASE WHEN $6 <> '' THEN $6 ELSE government_date END,
          veklep_modified = CASE WHEN $8 <> '' THEN $8 ELSE veklep_modified END,
          psp_status = CASE WHEN $9 <> '' THEN $9 ELSE psp_status END
      WHERE pid = $9
    ", params = list(
      material$id_tisk        %||% NA,
      as.integer(material$session %||% NA),
      material$status_id      %||% "",
      material$status_name    %||% "",
      material$description    %||% "",
      material$government_date %||% "",
      material$pid,
      material$veklep_modified %||% ""
    ))
    message("Bill already in DB: ", material$pid)
    return(invisible(NULL))
  }
  
  dbExecute(db, "
    INSERT INTO bills 
      (pid, title, predkladatel, published, processed_at, 
       id_tisk, session, status_id, status_name, description,
       government_date, veklep_modified, psp_status)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
  ", params = list(
    material$pid,
    material$title,
    material$predkladatel   %||% "",
    material$published      %||% "",
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    material$id_tisk        %||% NA,
    as.integer(material$session %||% NA),
    material$status_id      %||% "",
    material$status_name    %||% "",
    material$description    %||% "",
    material$government_date %||% "",
    material$veklep_modified %||% ""
  ))
  
  message("Saved bill: ", material$pid)
}

# ── Save extraction result ────────────────────────────────────────────────────

save_impact <- function(db, pid, result, doc_path = "") {
  raw_json <- toJSON(result, auto_unbox = TRUE)
  
  # INSERT INTO impacts — this was missing entirely
  dbExecute(db, "
    INSERT INTO impacts (pid, ria_provedena, typ_dokumentu, shrnutí, poznamky, doc_path, raw_json, created_at)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
  ", params = list(
    pid,
    as.integer(isTRUE(result$ria_provedena)),
    result$typ_dokumentu %||% "zadne",
    result$shrnutí %||% "",
    result$poznamky %||% "",
    doc_path,
    raw_json,
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  ))
  
  impact_id <- dbGetQuery(db,
                          "SELECT id FROM impacts WHERE pid = $1 ORDER BY id DESC LIMIT 1",
                          params = list(pid))$id[1]
  
  # costs loop ... (unchanged)
  # benefits loop ... (unchanged)
  
  
  # Save costs
  if (length(result$naklady) > 0) {
    for (n in result$naklady) {
      dbExecute(db, "
        INSERT INTO costs (impact_id, pid, subjekt, castka, periodicita, jistota, popis)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
      ", params = list(
        impact_id,
        pid,
        n$subjekt %||% "",
        n$castka %||% NA,
        n$periodicita %||% "neznámá",
        n$jistota %||% "kvalitativně",
        n$popis %||% ""
      ))
    }
    message("Saved ", length(result$naklady), " costs")
  }
  
  # Save benefits
  if (length(result$prinosy) > 0) {
    # In save_impact — fix the benefits loop (remove stray INSERT above it)
    for (p in result$prinosy) {
      dbExecute(db, "
    INSERT INTO benefits (impact_id, pid, subjekt, monetizovano, castka, popis)
    VALUES ($1, $2, $3, $4, $5, $6)
  ", params = list(
    impact_id,
    pid,
    p$subjekt %||% "",          
    as.integer(isTRUE(p$monetizovano)),
    p$castka %||% NA,
    p$popis %||% ""
  ))
    }
    message("Saved ", length(result$prinosy), " benefits")
  }
  
  message("Impact saved for: ", pid)
  impact_id
}

# ── Read functions for Shiny ──────────────────────────────────────────────────

get_all_bills <- function(db) {
  dbGetQuery(db, "
    SELECT b.pid, b.title, b.predkladatel, b.published,
           b.id_tisk, b.session, b.status_id, b.status_name, 
           b.description, b.government_date,
           b.komise_verdict, b.komise_url, b.komise_date,
           i.ria_provedena, i.typ_dokumentu, i.shrnutí, b.psp_status
    FROM bills b
    LEFT JOIN (
      SELECT DISTINCT ON (pid) *
      FROM impacts
      ORDER BY pid, id DESC
    ) i ON b.pid = i.pid
    ORDER BY 
      b.government_date IS NOT NULL ASC,
      b.government_date DESC NULLS FIRST,
      CAST(b.id_tisk AS INTEGER) DESC NULLS LAST
  ")
}

get_bill_costs <- function(db, pid) {
  dbGetQuery(db, "
    SELECT c.subjekt, c.castka, c.periodicita, c.jistota, c.popis
    FROM costs c
    WHERE c.pid = $1
    ORDER BY c.castka DESC NULLS LAST
  ", params = list(pid))
}

get_bill_benefits <- function(db, pid) {
  dbGetQuery(db, "
    SELECT b.subjekt, b.monetizovano, b.castka, b.popis
    FROM benefits b
    WHERE b.pid = $1
  ", params = list(pid))
}

# ── Migrate SQLite → PostgreSQL ───────────────────────────────────────────────

migrate_to_supabase <- function(sqlite_path = NULL) {
  
  if (is.null(sqlite_path)) {
    library(rprojroot)
    root <- find_rstudio_root_file()
    sqlite_path <- file.path(root, "data", "snemovna.db")
  }
  
  message("Connecting to SQLite...")
  library(RSQLite)
  sqlite <- dbConnect(RSQLite::SQLite(), sqlite_path)
  
  message("Connecting to Supabase...")
  pg <- get_db()
  init_db(pg)
  
  # Migrate bills
  bills <- dbGetQuery(sqlite, "SELECT * FROM bills")
  message("Migrating ", nrow(bills), " bills...")
  for (i in seq_len(nrow(bills))) {
    tryCatch(
      dbExecute(pg, "
        INSERT INTO bills (pid, title, predkladatel, published, processed_at)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (pid) DO NOTHING
      ", params = as.list(bills[i, ])),
      error = function(e) message("Bill skip: ", e$message)
    )
  }
  
  # Migrate impacts
  impacts <- dbGetQuery(sqlite, "SELECT * FROM impacts")
  message("Migrating ", nrow(impacts), " impacts...")
  
  # Keep a mapping of old sqlite id -> new postgres id
  id_map <- list()
  
  for (i in seq_len(nrow(impacts))) {
    row <- impacts[i, ]
    tryCatch({
      dbExecute(pg, "
        INSERT INTO impacts
          (pid, ria_provedena, typ_dokumentu, shrnutí, poznamky, doc_path, raw_json, created_at)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ", params = list(
        row$pid, row$ria_provedena, row$typ_dokumentu,
        row$shrnutí, row$poznamky, row$doc_path,
        row$raw_json, row$created_at
      ))
      new_id <- dbGetQuery(pg,
                           "SELECT id FROM impacts WHERE pid = $1 ORDER BY id DESC LIMIT 1",
                           params = list(row$pid)
      )$id[1]
      id_map[[as.character(row$id)]] <- new_id
    }, error = function(e) message("Impact skip: ", e$message))
  }
  
  # Migrate costs
  costs <- dbGetQuery(sqlite, "SELECT * FROM costs")
  message("Migrating ", nrow(costs), " costs...")
  for (i in seq_len(nrow(costs))) {
    row <- costs[i, ]
    new_impact_id <- id_map[[as.character(row$impact_id)]]
    if (is.null(new_impact_id)) next
    tryCatch(
      dbExecute(pg, "
        INSERT INTO costs (impact_id, pid, subjekt, castka, periodicita, jistota, popis)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
      ", params = list(
        new_impact_id, row$pid, row$subjekt,
        row$castka, row$periodicita, row$jistota, row$popis
      )),
      error = function(e) message("Cost skip: ", e$message)
    )
  }
  
  # Migrate benefits
  benefits <- dbGetQuery(sqlite, "SELECT * FROM benefits")
  message("Migrating ", nrow(benefits), " benefits...")
  for (i in seq_len(nrow(benefits))) {
    row <- benefits[i, ]
    new_impact_id <- id_map[[as.character(row$impact_id)]]
    if (is.null(new_impact_id)) next
    tryCatch(
      dbExecute(pg, "
        INSERT INTO benefits (impact_id, pid, subjekt, monetizovano, castka, popis)
        VALUES ($1, $2, $3, $4, $5, $6)
      ", params = list(
        new_impact_id, row$pid, row$monetizovano,
        row$castka, row$popis
      )),
      error = function(e) message("Benefit skip: ", e$message)
    )
  }
  
  dbDisconnect(sqlite)
  dbDisconnect(pg)
  message("Migration complete!")
}

# ── Helper ────────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && a != "") a else b