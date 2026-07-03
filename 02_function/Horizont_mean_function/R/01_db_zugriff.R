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
# Verknuepfung 03_LEITPROFILE <-> 02_KARTIEREINHEITEN
# ---------------------------------------------------------------------
# 03_LEITPROFILE traegt KEIN SOEH_KRZ/BL direkt, sondern nur die
# Spalte group_ID. Diese ist aufgebaut als:
#     group_ID = <BL>_<SOEH_KRZ>_<Version>     z.B. "MV_BiS_1", "ST_MüS_1"
# (SOEH_KRZ kann selbst "/" enthalten, z.B. "MV_MüS/BiS_1").
#
# SOEH_KRZ wird ueber die eindeutigen (group_ID, SOEH_KRZ)-Paare aus
# 02_KARTIEREINHEITEN ergaenzt; das BL wird aus dem group_ID-Praefix
# abgeleitet. Wichtig: NICHT direkt joinen, sonst vervielfacht sich das
# Leitprofil um jede MASTER_ID -> daher distinct().

#' BL (Bundesland) aus dem group_ID-Praefix ableiten
.bl_aus_group_id <- function(group_id) sub("^([^_]+)_.*$", "\\1", group_id)

#' SOEH_KRZ aus dem group_ID ableiten (Mittelteil zwischen BL und Version)
.soehkrz_aus_group_id <- function(group_id) sub("^[^_]+_(.*)_[^_]+$", "\\1", group_id)

#' Leitprofile (Tabelle 03_LEITPROFILE) einer Quelle laden
#'
#' Ergaenzt automatisch SOEH_KRZ (aus 02_KARTIEREINHEITEN) und BL
#' (aus group_ID), damit nach Feinbodenform und Region gefiltert
#' werden kann.
#'
#' @param quelle  "BWI", "BZE", "NR" oder "STOK"
#' @param region  optionaler Filter auf BL (z.B. "MV")
#' @return  tibble inkl. SOEH_KRZ, BL sowie Zusatzspalten .munsell und .boart
lade_leitprofile <- function(quelle = "BWI", region = NULL,
                            munsell_spalte = NULL, boart_spalte = NULL) {
  pfad <- db_pfad(quelle)
  df   <- db_read(pfad, TAB$LEITPROFILE)

  # --- SOEH_KRZ / BL ueber group_ID ergaenzen ---
  if ("group_ID" %in% names(df)) {
    if (!"SOEH_KRZ" %in% names(df)) {
      map <- tryCatch(db_read(pfad, TAB$KARTIEREINHEITEN), error = function(e) NULL)
      if (!is.null(map) && all(c("group_ID", "SOEH_KRZ") %in% names(map))) {
        map <- dplyr::distinct(map, group_ID, SOEH_KRZ)
        df  <- dplyr::left_join(df, map, by = "group_ID")
      } else {
        # Fallback: SOEH_KRZ direkt aus group_ID parsen
        df$SOEH_KRZ <- .soehkrz_aus_group_id(df$group_ID)
      }
    }
    if (!"BL" %in% names(df)) df$BL <- .bl_aus_group_id(df$group_ID)
  }

  if (!is.null(region) && "BL" %in% names(df)) {
    df <- dplyr::filter(df, BL == region)
  }

  df <- .ergaenze_farb_boart(df, munsell_spalte, boart_spalte)

  message("[", quelle, " / 03_LEITPROFILE]  Zeilen: ", nrow(df),
          " | Munsell-Spalte: ", ifelse(is.na(attr(df, "spalte_munsell")), "keine", attr(df, "spalte_munsell")),
          " | Bodenart-Spalte: ", ifelse(is.na(attr(df, "spalte_boart")),  "keine", attr(df, "spalte_boart")))
  df
}

#' Leitprofil(e) EINER Feinbodenform gezielt per SQL laden
#'
#' Entspricht der DB-Browser-Abfrage: verknuepft 03_LEITPROFILE mit den
#' eindeutigen (group_ID, SOEH_KRZ)-Paaren aus 02_KARTIEREINHEITEN.
#'
#' @param soeh_krz  Feinbodenform-Kuerzel (z.B. "BiS")
#' @param region    optionaler BL-Filter (z.B. "MV")
#' @param quelle    "BWI", "BZE", "NR" oder "STOK"
lade_leitprofil_fuer <- function(soeh_krz, region = NULL, quelle = "BWI",
                                munsell_spalte = NULL, boart_spalte = NULL) {
  con <- db_connect(db_pfad(quelle)); on.exit(DBI::dbDisconnect(con))
  # Nur lp.* selektieren (03_LEITPROFILE fuehrt SOEH_KRZ meist selbst) und
  # ueber die Kartiereinheiten-group_IDs filtern -> keine doppelte SOEH_KRZ-Spalte.
  sql <- paste(
    'SELECT lp.*',
    'FROM "03_LEITPROFILE" AS lp',
    'WHERE lp.group_ID IN (',
    '  SELECT DISTINCT group_ID FROM "02_KARTIEREINHEITEN" WHERE SOEH_KRZ = ?)',
    sep = "\n")
  df <- DBI::dbGetQuery(con, sql, params = list(soeh_krz))

  # SOEH_KRZ nur ergaenzen, falls die Leitprofil-Tabelle sie nicht fuehrt
  if (nrow(df) > 0 && !"SOEH_KRZ" %in% names(df)) {
    map <- DBI::dbGetQuery(con, 'SELECT DISTINCT group_ID, SOEH_KRZ FROM "02_KARTIEREINHEITEN"')
    df  <- dplyr::left_join(df, map, by = "group_ID")
  }
  if ("group_ID" %in% names(df) && !"BL" %in% names(df)) df$BL <- .bl_aus_group_id(df$group_ID)
  if (!is.null(region) && "BL" %in% names(df)) df <- df[df$BL == region, , drop = FALSE]

  df <- .ergaenze_farb_boart(df, munsell_spalte, boart_spalte)
  tibble::as_tibble(df)
}

# Kandidaten-Spaltennamen (an einer Stelle gepflegt).
# Fuer die NR/BWI/BZE-Datenbanken heisst die KA5-Bodenart-Spalte "BODART"
# (nicht "BOART"); eine gemessene Munsell-Farbe existiert dort nicht.
.MUNSELL_KANDIDATEN <- c("MUNSELL", "BODENFARBE", "FARBE", "FARBE_FEUCHT", "MUNSELL_F", "BOFA")
.BOART_KANDIDATEN   <- c("BODART", "BOART", "BODENART", "BOART_KA5",
                        "BODENART_KA5", "KOERNUNG", "BART", "KA5")

#' Munsell-/Bodenart-Spalten erkennen und als .munsell/.boart anhaengen
#' (die gewaehlten Spaltennamen stehen danach in attr(df, "spalte_munsell")
#' bzw. attr(df, "spalte_boart"))
.ergaenze_farb_boart <- function(df, munsell_spalte = NULL, boart_spalte = NULL) {
  if (is.null(munsell_spalte)) munsell_spalte <- .finde_spalte(df, .MUNSELL_KANDIDATEN)
  if (is.null(boart_spalte))   boart_spalte   <- .finde_spalte(df, .BOART_KANDIDATEN)
  df$.munsell <- if (!is.na(munsell_spalte)) as.character(df[[munsell_spalte]]) else NA_character_
  df$.boart   <- if (!is.na(boart_spalte))   as.character(df[[boart_spalte]])   else NA_character_
  attr(df, "spalte_munsell") <- munsell_spalte
  attr(df, "spalte_boart")   <- boart_spalte
  df
}

#' Quelle(n) aus dem MASTER_ID-Praefix ableiten (NR_.. / BWI_.. / BZE_..)
.quelle_aus_master_id <- function(master_id) {
  praefix <- toupper(sub("^([A-Za-z]+)_.*$", "\\1", master_id))
  if (praefix %in% c("NR", "BWI", "BZE", "STOK")) praefix else c("NR", "BWI", "BZE")
}

#' Leitprofil ueber eine MASTER_ID laden (Quelle automatisch)
#'
#' Schlaegt zu einer MASTER_ID die group_ID in 02_KARTIEREINHEITEN nach und
#' laedt das zugehoerige Leitprofil aus 03_LEITPROFILE. Die Datenquelle wird
#' aus dem MASTER_ID-Praefix (NR_/BWI_/BZE_) bestimmt; ist sie unklar, werden
#' NR, BWI und BZE der Reihe nach durchsucht.
#'
#' @param master_id  eine MASTER_ID (z.B. "NR_130_08_66519")
#' @param quelle     optional erzwingen ("NR"/"BWI"/"BZE"/"STOK")
#' @return  tibble mit dem Leitprofil (inkl. SOEH_KRZ, BL, group_ID,
#'          .munsell, .boart); Attribut "quelle" nennt die gefundene Quelle.
lade_leitprofil_master <- function(master_id, quelle = NULL,
                                  munsell_spalte = NULL, boart_spalte = NULL) {
  stopifnot(length(master_id) == 1L)
  quellen <- if (!is.null(quelle)) quelle else .quelle_aus_master_id(master_id)
  dateien <- boden_dateien()

  gefunden_ohne_lp <- character(0)   # Quellen, wo MASTER_ID existiert, aber kein Leitprofil

  for (q in quellen) {
    if (is.null(dateien[[q]]) || is.na(dateien[[q]])) next
    pfad <- dateien[[q]]

    # MASTER_ID -> group_ID / SOEH_KRZ
    con <- db_connect(pfad)
    ke <- tryCatch(
      DBI::dbGetQuery(con,
        'SELECT DISTINCT group_ID, SOEH_KRZ FROM "02_KARTIEREINHEITEN" WHERE MASTER_ID = ?',
        params = list(master_id)),
      error = function(e) NULL)
    DBI::dbDisconnect(con)
    if (is.null(ke) || nrow(ke) == 0) next          # MASTER_ID nicht in dieser Quelle

    gid <- unique(ke$group_ID)

    # Leitprofil zu den group_ID(s) laden
    con <- db_connect(pfad)
    ph  <- paste(rep("?", length(gid)), collapse = ", ")
    lp  <- DBI::dbGetQuery(con,
             paste0('SELECT * FROM "03_LEITPROFILE" WHERE group_ID IN (', ph, ')'),
             params = as.list(gid))
    DBI::dbDisconnect(con)

    # --- Ausweichen: group_ID hat kein eigenes Leitprofil ---
    # Typisch bei Kombiformen (SOEH_KRZ "MüS/BiS"). Dann auf die Teilformen
    # IN DERSELBEN REGION ausweichen (z.B. MV_MüS_1, MV_BiS_1) und vermerken.
    if (nrow(lp) == 0) {
      soeh  <- unique(ke$SOEH_KRZ)
      bl    <- unique(.bl_aus_group_id(gid))[1]
      teile <- unique(trimws(unlist(strsplit(soeh, "/", fixed = TRUE))))
      message("MASTER_ID '", master_id, "' -> group_ID ", paste(gid, collapse = ", "),
              " (SOEH_KRZ ", paste(soeh, collapse = ", "),
              ") hat KEIN eigenes Leitprofil -> Ausweichen auf: ",
              paste(teile, collapse = ", "), " (Region ", bl, ")")

      teile_df <- list(); gefunden <- character(0)
      for (t in teile) {
        d <- tryCatch(
          lade_leitprofil_fuer(t, region = bl, quelle = q,
                               munsell_spalte = munsell_spalte, boart_spalte = boart_spalte),
          error = function(e) NULL)
        if (!is.null(d) && nrow(d) > 0) { teile_df[[t]] <- d; gefunden <- c(gefunden, t) }
      }
      if (length(teile_df) == 0) {
        message("  Keine Ausweichform mit Leitprofil gefunden.")
        gefunden_ohne_lp <- c(gefunden_ohne_lp, q)
        next
      }
      fehlt <- setdiff(teile, gefunden)
      out <- dplyr::bind_rows(teile_df)
      out$AUSWEICH_VON       <- soeh        # Vermerk: urspruengliche (Kombi-)Form
      out$MASTER_ID_ANFRAGE  <- master_id
      message("  Ausweich-Profile: ", paste(sort(unique(out$group_ID)), collapse = ", "),
              if (length(fehlt)) paste0("  (ohne Leitprofil: ", paste(fehlt, collapse = ", "), ")") else "")
      out <- tibble::as_tibble(out)
      attr(out, "quelle")       <- q
      attr(out, "ausweich")     <- TRUE
      attr(out, "ausweich_von") <- soeh
      return(out)
    }

    # SOEH_KRZ nur ergaenzen, falls die Leitprofil-Tabelle sie nicht selbst
    # fuehrt (sonst entstehen beim Join doppelte SOEH_KRZ.x/.y-Spalten).
    if (!"SOEH_KRZ" %in% names(lp))
      lp <- dplyr::left_join(lp, dplyr::distinct(ke, group_ID, SOEH_KRZ), by = "group_ID")
    if (!"BL" %in% names(lp)) lp$BL <- .bl_aus_group_id(lp$group_ID)
    lp <- .ergaenze_farb_boart(lp, munsell_spalte, boart_spalte)

    message("MASTER_ID '", master_id, "' -> Quelle ", q, ", group_ID ",
            paste(gid, collapse = ", "), ", SOEH_KRZ ",
            paste(unique(ke$SOEH_KRZ), collapse = ", "))
    lp <- tibble::as_tibble(lp)
    attr(lp, "quelle") <- q
    return(lp)
  }

  if (length(gefunden_ohne_lp))
    stop("MASTER_ID '", master_id, "' existiert in ",
         paste(unique(gefunden_ohne_lp), collapse = ", "),
         ", hat dort aber kein Leitprofil (vgl. Kommentar 'keine Form - Ausweichen').")
  stop("MASTER_ID '", master_id, "' in keiner Quelle (",
       paste(quellen, collapse = ", "), ") gefunden.")
}

#' Pruefen, fuer welche group_ID einer Feinbodenform ein Leitprofil existiert
#'
#' Gleicht die in 02_KARTIEREINHEITEN bekannten group_ID(s) einer SOEH_KRZ
#' mit den tatsaechlich in 03_LEITPROFILE vorhandenen Horizontzeilen ab.
#' Erklaert Faelle wie: SOEH_KRZ "MüS" ist in Kartiereinheiten fuer MV_MüS_1
#' UND ST_MüS_1 bekannt, aber nur MV_MüS_1 hat tatsaechlich ein Leitprofil.
#'
#' @param soeh_krz  eine oder mehrere Feinbodenformen (z.B. "MüS")
#' @param quelle    "BWI", "BZE", "NR" oder "STOK"
#' @return  data.frame: group_ID, SOEH_KRZ, BL, n_horizonte, leitprofil (TRUE/FALSE)
pruefe_leitprofil <- function(soeh_krz, quelle = "NR") {
  pfad <- db_pfad(quelle)
  ke   <- db_read(pfad, TAB$KARTIEREINHEITEN)
  lp   <- db_read(pfad, TAB$LEITPROFILE)

  ke <- dplyr::distinct(
    ke[ke$SOEH_KRZ %in% soeh_krz, c("group_ID", "SOEH_KRZ"), drop = FALSE])
  if (nrow(ke) == 0) {
    message("Keine group_ID in 02_KARTIEREINHEITEN fuer: ",
            paste(soeh_krz, collapse = ", "))
    return(invisible(ke))
  }

  n_hz <- table(lp$group_ID[lp$group_ID %in% ke$group_ID])
  ke$BL          <- .bl_aus_group_id(ke$group_ID)
  ke$n_horizonte <- as.integer(n_hz[ke$group_ID])
  ke$n_horizonte[is.na(ke$n_horizonte)] <- 0L
  ke$leitprofil  <- ke$n_horizonte > 0L
  ke <- ke[order(ke$SOEH_KRZ, ke$group_ID), ]
  rownames(ke) <- NULL

  ohne <- ke$group_ID[!ke$leitprofil]
  if (length(ohne))
    message("Ohne Leitprofil in 03_LEITPROFILE (werden nicht geplottet): ",
            paste(ohne, collapse = ", "))
  ke
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
