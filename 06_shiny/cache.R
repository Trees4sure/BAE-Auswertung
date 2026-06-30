# cache.R ----
# SQLite-Cache fuer wiederholte teure Abfragen.
# Bodendaten brauchen keinen Cache hier – Quelle ist bereits SQLite,
# In-Memory-Cache in reactiveValues ist schneller fuer Einzelpunkte.
#
# Dieser Cache lohnt sich fuer:
#   - standort_bae:  alle 38 Klimalaeufe fuer eine MASTER_ID (2-4 Sek. Laden)
#   - wm_cache_<AP>: komplette WM-CSV (alle MASTER_IDs) je AP/Klimalauf/Baumart,
#                    indiziert auf MASTER_ID. wm_cache_meta haelt die
#                    Quell-mtime fest und triggert Reimport bei Aenderung.

# Absoluter Pfad ueber den App-Anker (APP_DIR_ABS aus app.R), damit der Cache
# unabhaengig vom Arbeitsverzeichnis im reaktiven Kontext geschrieben/gelesen
# wird. Fallback auf den relativen Pfad, falls cache.R isoliert gesourct wird.
CACHE_PATH <- if (exists("APP_DIR_ABS")) file.path(APP_DIR_ABS, "cache", "app_cache.sqlite") else (
    "cache/app_cache.sqlite")

# ── Initialisierung (einmalig beim App-Start) ─────────────────
init_app_cache <- function(path = CACHE_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  
  # WAL-Modus: parallele Reads bei Shiny-Mehrsitzungen
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA synchronous  = NORMAL")
  
  # Tabelle: alle BAE-Ergebnisse je Standort (fuer Standortanalyse)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS standort_bae (
      MASTER_ID TEXT,
      Baumart   TEXT,
      TV        TEXT,
      Zeitlauf  TEXT,
      Szenario  TEXT,
      Modell    TEXT,
      Zeitraum  TEXT,
      BAE_3ST   TEXT,
      BAE_4ST   TEXT,
      BAE_5ST   TEXT,
      BAE_7ST   TEXT,
      PRIMARY KEY (MASTER_ID, Baumart, TV, Zeitlauf)
    ) WITHOUT ROWID")
  
  # Tabelle: Cache-Metadaten je WM-Datei (mtime-basierte Invalidierung).
  # Die eigentlichen Werte liegen in dynamisch angelegten wm_cache_<Kuerzel>-
  # Tabellen (eine komplette WM-CSV je Kombination, indiziert auf MASTER_ID).
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS wm_cache_meta (
      Kuerzel      TEXT,
      Region       TEXT,
      Szenario     TEXT,
      Modell       TEXT,
      Zeitraum     TEXT,
      Baumart      TEXT,
      source_path  TEXT,
      source_mtime TEXT,
      n_rows       INTEGER,
      imported_at  TEXT,
      PRIMARY KEY (Kuerzel, Region, Szenario, Modell, Zeitraum, Baumart)
    ) WITHOUT ROWID")
  
  message("App-Cache bereit: ", path)
  invisible(path)
}

# ── standort_bae: Lesen ───────────────────────────────────────
cache_standort_get <- function(master_id, path = CACHE_PATH) {
  if (!file.exists(path)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  res <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT * FROM standort_bae WHERE MASTER_ID = ?",
                    params = list(master_id)),
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res) == 0) NULL else res
}

# ── standort_bae: Schreiben ───────────────────────────────────
cache_standort_set <- function(df, path = CACHE_PATH) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  
  # Nur die Cache-Spalten behalten
  keep <- c("MASTER_ID", "Baumart", "TV", "Zeitlauf",
            "Szenario",  "Modell",  "Zeitraum",
            "BAE_3ST",   "BAE_4ST", "BAE_5ST", "BAE_7ST")
  df_write <- df[, intersect(keep, names(df)), drop = FALSE]
  # Fehlende BAE-Spalten als NA ergaenzen
  for (col in setdiff(keep, names(df_write)))
    df_write[[col]] <- NA_character_
  
  tryCatch({
    DBI::dbWriteTable(con, "tmp_ins", df_write, temporary = TRUE, overwrite = TRUE)
    DBI::dbExecute(con, "INSERT OR IGNORE INTO standort_bae SELECT * FROM tmp_ins")
  }, error = function(e) message("Cache-Schreiben standort_bae: ", e$message))
  invisible(NULL)
}

# ── WM-Datei-Cache: Tabellenname je Kuerzel ───────────────────
wm_cache_table <- function(kuerzel) paste0("wm_cache_", gsub("[^A-Za-z0-9]", "_", kuerzel))

# ── wm_cache_meta: Lesen (prueft ob Cache fuer Kombination aktuell ist) ──
wm_cache_meta_get <- function(kuerzel, region, szenario, modell, zeitraum, baumart,
                               path = CACHE_PATH) {
  if (!file.exists(path)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  res <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT * FROM wm_cache_meta
       WHERE Kuerzel=? AND Region=? AND Szenario=? AND Modell=? AND Zeitraum=? AND Baumart=?",
                    params = list(kuerzel, region, szenario, modell, zeitraum, baumart)),
    error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) NULL else res[1, , drop = FALSE]
}

# ── wm_cache_<Kuerzel>: komplette CSV importieren ─────────────
# Ersetzt evtl. vorhandene Zeilen dieser Kombination (Reimport bei
# geaenderter Quelldatei) und aktualisiert den Meta-Eintrag.
wm_cache_import <- function(kuerzel, region, szenario, modell, zeitraum, baumart,
                             df, source_path, source_mtime, path = CACHE_PATH) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  tbl <- wm_cache_table(kuerzel)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))

  # Alte Zeilen dieser Kombination entfernen (Tabelle existiert beim
  # allerersten Import fuer dieses Kuerzel noch nicht -> Fehler ignorieren)
  tryCatch(
    DBI::dbExecute(con, sprintf(
      "DELETE FROM %s WHERE Region=? AND Szenario=? AND Modell=? AND Zeitraum=? AND Baumart=?", tbl),
      params = list(region, szenario, modell, zeitraum, baumart)),
    error = function(e) NULL)

  DBI::dbWriteTable(con, tbl, df, append = TRUE)
  DBI::dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s ON %s (Region, Szenario, Modell, Zeitraum, Baumart, MASTER_ID)",
    tbl, tbl))

  DBI::dbExecute(con,
    "INSERT OR REPLACE INTO wm_cache_meta
       (Kuerzel, Region, Szenario, Modell, Zeitraum, Baumart, source_path, source_mtime, n_rows, imported_at)
     VALUES (?,?,?,?,?,?,?,?,?,?)",
    params = list(kuerzel, region, szenario, modell, zeitraum, baumart,
                   source_path, source_mtime, nrow(df), as.character(Sys.time())))
  invisible(NULL)
}

# ── wm_cache_<Kuerzel>: indizierte Abfrage fuer einen MASTER_ID ───
wm_cache_query <- function(kuerzel, region, szenario, modell, zeitraum, baumart, master_id,
                            path = CACHE_PATH) {
  tbl <- wm_cache_table(kuerzel)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, tbl)) return(NULL)
  res <- tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM %s WHERE Region=? AND Szenario=? AND Modell=? AND Zeitraum=? AND Baumart=? AND MASTER_ID=?", tbl),
      params = list(region, szenario, modell, zeitraum, baumart, master_id)),
    error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) NULL else res[1, , drop = FALSE]
}

# ── Cache-Statistik (fuer Info-Tab) ──────────────────────────
cache_stats <- function(path = CACHE_PATH) {
  if (!file.exists(path)) return(list(standorte = 0, wm = 0, groesse_mb = 0))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  n_st <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT COUNT(DISTINCT MASTER_ID) AS n FROM standort_bae")$n,
    error = function(e) 0)
  n_wm <- tryCatch(
    DBI::dbGetQuery(con,
                    "SELECT COUNT(*) AS n FROM wm_cache_meta")$n,
    error = function(e) 0)
  list(standorte  = n_st,
       wm         = n_wm,
       groesse_mb = round(file.size(path) / 1024^2, 1))
}

