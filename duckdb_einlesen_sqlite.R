# ============================================================================
# duckdb_einlesen_sqlite.R
# ----------------------------------------------------------------------------
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App). Zwei Dinge:
#
#   1) Eine DuckDB-Datei (*.duckdb) einlesen und die Tabellen in R
#      verfügbar machen (als Liste `tabellen` + einzelne data.frames).
#   2) Denselben Inhalt 1:1 in eine SQLite-Datei (*.sqlite) schreiben, damit
#      man sie mit "DB Browser for SQLite" (DB4S) anschauen kann.
#
# Hintergrund: DB Browser for SQLite kann NUR SQLite lesen, KEIN DuckDB.
# Deshalb der Zwischenschritt: DuckDB -> data.frame -> SQLite.
#
# Benötigte Pakete: DBI, duckdb, RSQLite
#   install.packages(c("DBI", "duckdb", "RSQLite"))
#
# Nutzung: Pfade unten anpassen, dann Skript durchlaufen lassen (Source) oder
# Abschnitt für Abschnitt ausführen (jede Zwischengröße ist einsehbar).
# ============================================================================

library(DBI)
library(duckdb)
library(RSQLite)


# ----  PFADE (hier anpassen) ----
# Pfad zur vorhandenen DuckDB-Datei. Wird der Pfad leer gelassen, sucht das
# Skript weiter unten selbst unter 01_data/ nach der ersten *.duckdb-Datei.
duckdb_pfad <- ""

# Zielpfad für die SQLite-Kopie. Leer lassen -> gleicher Name/Ordner wie die
# DuckDB, nur mit Endung .sqlite (z.B. daten.duckdb -> daten.sqlite).
sqlite_pfad <- ""

# Optional: nur diese Tabellen übertragen. NULL = alle Tabellen der DuckDB.
nur_tabellen <- NULL


# ----  DuckDB-Datei finden (falls oben nicht gesetzt) ----
if (!nzchar(duckdb_pfad)) {
  kandidaten  <- list.files("01_data", pattern = "\\.duckdb$",
                            recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(kandidaten))
    stop("Keine *.duckdb-Datei unter 01_data/ gefunden. Bitte 'duckdb_pfad' oben setzen.")
  duckdb_pfad <- kandidaten[1]
  message("Automatisch gewählte DuckDB: ", duckdb_pfad,
          if (length(kandidaten) > 1) paste0("  (", length(kandidaten), " gefunden, erste genommen)") else "")
}
if (!file.exists(duckdb_pfad)) stop("DuckDB-Datei nicht gefunden: ", duckdb_pfad)

if (!nzchar(sqlite_pfad))
  sqlite_pfad <- sub("\\.duckdb$", ".sqlite", duckdb_pfad, ignore.case = TRUE)


# ----  DuckDB einlesen ----
# read_only = TRUE: die DuckDB wird nur gelesen, nie verändert.
con_duck <- dbConnect(duckdb::duckdb(), dbdir = duckdb_pfad, read_only = TRUE)

tabellen_namen <- dbListTables(con_duck)
if (!is.null(nur_tabellen)) tabellen_namen <- intersect(nur_tabellen, tabellen_namen)
if (!length(tabellen_namen)) stop("Keine (passenden) Tabellen in der DuckDB gefunden.")

message("DuckDB-Tabellen (", length(tabellen_namen), "): ",
        paste(tabellen_namen, collapse = ", "))

# Alle Tabellen als benannte Liste von data.frames einlesen.
# `tabellen$<name>` enthält danach die jeweilige Tabelle -> gut zum Reinschauen.
tabellen <- lapply(tabellen_namen, function(tab) dbReadTable(con_duck, tab))
names(tabellen) <- tabellen_namen

dbDisconnect(con_duck, shutdown = TRUE)

# Kurzer Überblick: Zeilen/Spalten je Tabelle
uebersicht <- data.frame(
  Tabelle = tabellen_namen,
  Zeilen  = vapply(tabellen, nrow, integer(1)),
  Spalten = vapply(tabellen, ncol, integer(1)),
  row.names = NULL
)
print(uebersicht)


# ----  In SQLite schreiben (für DB Browser) ----
# Vorhandene Zieldatei ersetzen, damit kein alter Stand übrig bleibt.
if (file.exists(sqlite_pfad)) file.remove(sqlite_pfad)

con_sqlite <- dbConnect(RSQLite::SQLite(), dbname = sqlite_pfad)

for (tab in tabellen_namen) {
  dbWriteTable(con_sqlite, name = tab, value = tabellen[[tab]], overwrite = TRUE)
  message("  geschrieben: ", tab, " (", nrow(tabellen[[tab]]), " Zeilen)")
}

dbDisconnect(con_sqlite)

message("---")
message("Fertig. SQLite-Datei: ", normalizePath(sqlite_pfad))
message("In 'DB Browser for SQLite' öffnen: Datei > Datenbank öffnen > diese .sqlite wählen.")


# ----  ALTERNATIVE: direkt in DuckDB per SQLite-Extension (ohne R-Umweg) ----
# Schneller bei großen Datenbanken, braucht aber einmalig Internet für die
# Extension. Bei Bedarf einkommentieren statt des R-Umwegs oben:
#
# con_duck <- dbConnect(duckdb::duckdb(), dbdir = duckdb_pfad, read_only = TRUE)
# dbExecute(con_duck, "INSTALL sqlite; LOAD sqlite;")
# dbExecute(con_duck, sprintf("ATTACH '%s' AS ziel (TYPE SQLITE);", sqlite_pfad))
# for (tab in dbListTables(con_duck))
#   dbExecute(con_duck, sprintf('CREATE TABLE ziel."%s" AS SELECT * FROM "%s";', tab, tab))
# dbDisconnect(con_duck, shutdown = TRUE)
