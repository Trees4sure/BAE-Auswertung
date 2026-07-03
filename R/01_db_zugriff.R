# =====================================================================
# 01_db_zugriff.R
# ---------------------------------------------------------------------
# Zugriff auf die SQLite-Bodendatenbanken (Ersatz fuer den frueheren
# SQL-Server-Zugriff via RODBC/sqlQuery in BAE_Auswertung_all.R).
#
# FRUEHER (SQL Server, eine Verbindung, viele Tabellen):
#   DB_Lp_BWI <- sqlQuery(DB_Verbindung, "select * from dbo.MRS_BWI_03_Leitprofile")
#   DB_Ka_BWI <- sqlQuery(DB_Verbindung, "select * from dbo.MRS_BWI_02_Kartiereinheiten")
#   ...
#
# JETZT (SQLite, eine Datei je Datenquelle):
#   01_data/Grundlagen/Bodendatenbank/....MRS_BWI....sqlite3
#   01_data/Grundlagen/Bodendatenbank/....MRS_BZE....sqlite3
#   01_data/Grundlagen/Bodendatenbank/....MRS_NR ....sqlite3
#
#   Tabellen INNERHALB jeder Datei (Tabellenblaetter):
#     00_BESCHREIBUNG
#     01_KOPFDATEN
#     02_KARTIEREINHEITEN     <- entspricht "..._02_Kartiereinheiten"
#     03_LEITPROFILE          <- entspricht "..._03_Leitprofile"
#     04_BUNDESLAND
#     05_QUALITAETSSCHLUESSEL
# =====================================================================

# Benoetigte Pakete: DBI, RSQLite, dplyr, stringr, tibble

# ---- Grundpfade (wie im Ausgangs-Skript) ----------------------------
ground_zero        <- "01_data/Grundlagen"
Daten_Bodensynopse <- file.path(ground_zero, "Bodendatenbank")

# ---- Standard-Tabellennamen innerhalb der SQLite-Dateien ------------
TAB <- list(
  BESCHREIBUNG    = "00_BESCHREIBUNG",
  KOPFDATEN       = "01_KOPFDATEN",
  KARTIEREINHEITEN= "02_KARTIEREINHEITEN",
  LEITPROFILE     = "03_LEITPROFILE",
  BUNDESLAND      = "04_BUNDESLAND",
  QUALITAET       = "05_QUALITAETSSCHLUESSEL"
)


# ---------------------------------------------------------------------
# SQLite-Dateien der drei Quellen (BWI / BZE / NR) finden
# ---------------------------------------------------------------------
#' @return benannte Liste mit Pfaden: $BWI, $BZE, $NR (NA falls nicht gefunden)
boden_dateien <- function(verzeichnis = Daten_Bodensynopse) {
  alle <- list.files(verzeichnis, recursive = TRUE, full.names = TRUE)
  alle <- grep("\\.sqlite3?$", alle, value = TRUE, ignore.case = TRUE)
  pick <- function(muster) {
    hit <- grep(muster, alle, value = TRUE, ignore.case = TRUE)
    if (length(hit)) hit[1] else NA_character_
  }
  list(
    BWI  = pick("MRS_BWI"),
    BZE  = pick("MRS_BZE"),
    NR   = pick("MRS_NR"),
    STOK = pick("StoK|STOK|_MV")   # Standortskarte MV, falls als eigene Datei vorhanden
  )
}

#' Pfad zur SQLite-Datei einer Quelle liefern
db_pfad <- function(quelle = c("BWI", "BZE", "NR", "STOK"),
                    dateien = boden_dateien()) {
  quelle <- match.arg(quelle)
  p <- dateien[[quelle]]
  if (is.na(p)) stop("Keine SQLite-Datei fuer Quelle '", quelle,
                     "' unter ", Daten_Bodensynopse, " gefunden.")
  p
}


# ---------------------------------------------------------------------
# Elementare SQLite-Helfer
# ---------------------------------------------------------------------
db_connect <- function(pfad) {
  if (!file.exists(pfad)) stop("SQLite-Datei nicht gefunden: ", pfad)
  DBI::dbConnect(RSQLite::SQLite(), dbname = pfad)
}

db_tabellen <- function(pfad) {
  con <- db_connect(pfad); on.exit(DBI::dbDisconnect(con))
  DBI::dbListTables(con)
}

db_spalten <- function(pfad, tabelle) {
  con <- db_connect(pfad); on.exit(DBI::dbDisconnect(con))
  DBI::dbListFields(con, tabelle)
}

db_read <- function(pfad, tabelle) {
  con <- db_connect(pfad); on.exit(DBI::dbDisconnect(con))
  if (!tabelle %in% DBI::dbListTables(con)) {
    stop("Tabelle '", tabelle, "' nicht vorhanden. Verfuegbar: ",
         paste(DBI::dbListTables(con), collapse = ", "))
  }
  tibble::as_tibble(DBI::dbReadTable(con, tabelle))
}


# ---------------------------------------------------------------------
# Spalten fuer Munsell-Farbe / Bodenart automatisch erkennen
# ---------------------------------------------------------------------
.finde_spalte <- function(df, kandidaten) {
  namen <- names(df)
  for (k in kandidaten) {                              # exakter Treffer
    hit <- namen[toupper(namen) == toupper(k)]
    if (length(hit)) return(hit[1])
  }
  for (k in kandidaten) {                              # Teilstring-Treffer
    hit <- namen[grepl(k, namen, ignore.case = TRUE)]
    if (length(hit)) return(hit[1])
  }
  NA_character_
}


# ---------------------------------------------------------------------
# Komfort-Loader
# ---------------------------------------------------------------------
#' Leitprofile (Tabelle 03_LEITPROFILE) einer Quelle laden
#'
#' @param quelle  "BWI", "BZE", "NR" oder "STOK"
#' @param region  optionaler Filter auf Spalte BL (z.B. "MV")
#' @return  tibble inkl. Zusatzspalten .munsell und .boart
lade_leitprofile <- function(quelle = "BWI", region = NULL,
                            munsell_spalte = NULL, boart_spalte = NULL) {
  df <- db_read(db_pfad(quelle), TAB$LEITPROFILE)

  if (!is.null(region) && "BL" %in% names(df)) {
    df <- dplyr::filter(df, BL == region)
  }

  if (is.null(munsell_spalte))
    munsell_spalte <- .finde_spalte(df, c("MUNSELL", "BODENFARBE", "FARBE",
                                          "FARBE_FEUCHT", "MUNSELL_F", "BOFA"))
  if (is.null(boart_spalte))
    boart_spalte <- .finde_spalte(df, c("BOART", "BODENART", "BOART_KA5",
                                        "BODENART_KA5", "KOERNUNG", "BART", "KA5"))

  df$.munsell <- if (!is.na(munsell_spalte)) as.character(df[[munsell_spalte]]) else NA_character_
  df$.boart   <- if (!is.na(boart_spalte))   as.character(df[[boart_spalte]])   else NA_character_

  message("[", quelle, " / 03_LEITPROFILE]  Zeilen: ", nrow(df),
          " | Munsell-Spalte: ", ifelse(is.na(munsell_spalte), "keine", munsell_spalte),
          " | Bodenart-Spalte: ", ifelse(is.na(boart_spalte),  "keine", boart_spalte))
  df
}

#' Kartiereinheiten (Tabelle 02_KARTIEREINHEITEN) einer Quelle laden
lade_kartiereinheiten <- function(quelle = "BWI", region = NULL) {
  df <- db_read(db_pfad(quelle), TAB$KARTIEREINHEITEN)
  if (!is.null(region) && "BL" %in% names(df)) df <- dplyr::filter(df, BL == region)
  df
}

#' Alle frueheren DB-Objekte in einem Rutsch bereitstellen
#'
#' Liefert eine Liste mit denselben Namen wie im alten SQL-Server-Skript.
#' @return list(DB_Lp_BWI, DB_Ka_BWI, DB_Lp_NR, DB_Ka_NR, DB_Lp_BZE, DB_Ka_BZE,
#'              DB_STOK_Lp, DB_STOK_Ka)  (STOK nur, falls Datei vorhanden)
lade_alle_db <- function() {
  dateien <- boden_dateien()
  hol <- function(q, tab) if (!is.na(dateien[[q]])) db_read(dateien[[q]], tab) else NULL

  out <- list(
    DB_Lp_BWI = hol("BWI", TAB$LEITPROFILE),
    DB_Ka_BWI = hol("BWI", TAB$KARTIEREINHEITEN),
    DB_Lp_BZE = hol("BZE", TAB$LEITPROFILE),
    DB_Ka_BZE = hol("BZE", TAB$KARTIEREINHEITEN),
    DB_Lp_NR  = hol("NR",  TAB$LEITPROFILE),
    DB_Ka_NR  = hol("NR",  TAB$KARTIEREINHEITEN)
  )
  if (!is.na(dateien$STOK)) {
    out$DB_STOK_Lp <- hol("STOK", TAB$LEITPROFILE)
    out$DB_STOK_Ka <- hol("STOK", TAB$KARTIEREINHEITEN)
  }
  out
}
