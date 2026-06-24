# =============================================================================
# MASTER_ID-Anbindung fuer die WL-/Trend-Diagramme + Ausgabe als Region-CSVs
#
# Ziel: pro Diagrammtyp 12 CSV (BWI + NR-01..NR-11) in einem Ordner, Long-Format,
# auf MASTER_ID gekeyt. Die App filtert nur per Region (Dateiname) und zeichnet.
#
# BWI und NR erreichen MASTER_ID UNTERSCHIEDLICH:
#   BWI : Tabellen-Join  cell_id -> id_bwi_bze -> master_id_boden   (1 Zelle:1 ID)
#   NR  : Polygon-Mittel  Klimaraster --exact_extract(mean)--> MASTER_ID
#         (raeumliche Aggregation; NICHT in diesem File - braucht GEO_NR.shp und
#          georeferenzierte Raster, s. Hinweis unten / wl_master_id_nr.R)
#
# Dieses File deckt den BWI-Pfad + die gemeinsamen Bausteine ab:
#   wl_split_quelle()        quelle (BWI/NR-XX) + sauberen Zeitlauf trennen
#   read_nc_id_grid()        id-Raster (cell_id -> id_bwi_bze) per ncdf4 lesen
#   build_bwi_master_lookup() quelle | cell_id | MASTER_ID  (fuer BWI)
#   attach_master_id()       MASTER_ID an WL-/Trend-Long anhaengen (generisch)
#   write_region_csvs()      je quelle eine CSV schreiben
# =============================================================================


# ---- quelle (BWI / NR-XX) + Zeitlauf trennen --------------------------------
#' Aus dem kombinierten Schluessel der Bruecken (z.B. "bwi-bze_OBS_DWD_1961-1990"
#' bzw. "nr-01_OBS_DWD_1961-1990") die Region herausziehen und den Zeitlauf
#' bereinigen. Ergebnis: Spalte 'quelle' ("BWI" / "NR-01"...) + bereinigter
#' 'Zeitlauf' ("OBS_DWD_1961-1990").
#'
#' @param df  Long-data.frame mit Spalte \code{col} (Default "Zeitlauf").
#' @param col Name der Schluesselspalte.
#' @return df mit zusaetzlicher Spalte 'quelle' und bereinigtem Zeitlauf.
wl_split_quelle <- function(df, col = "Zeitlauf") {
  z   <- as.character(df[[col]])
  pat <- "^(bwi[-_]bze|nr-?[0-9]{2})_(.*)$"
  reg <- sub(pat, "\\1", z, ignore.case = TRUE)
  run <- sub(pat, "\\2", z, ignore.case = TRUE)

  quelle <- ifelse(grepl("^bwi", reg, ignore.case = TRUE),
                   "BWI",
                   toupper(sub("^nr-?", "NR-", reg, ignore.case = TRUE)))
  # Falls Pattern nicht greift (kein Regionspraefix): quelle = NA, Zeitlauf bleibt
  nogo <- reg == z
  quelle[nogo] <- NA_character_
  run[nogo]    <- z[nogo]

  df[[col]]   <- run
  df$quelle   <- quelle
  df[, c("quelle", setdiff(names(df), "quelle")), drop = FALSE]
}


# ---- id-Raster (cell_id -> id_bwi_bze) per ncdf4 lesen ----------------------
#' Ein einlagiges Geometrie-/id-Raster (z.B. 8002 = id_bwi_bze) in dasselbe
#' Voll-Gitter ueberfuehren wie die Klima-Leser -> cell_id deckungsgleich.
#'
#' @param nc_file    Pfad zum id-NetCDF.
#' @param varname    Variablenname; NULL = erste Variable mit >=2 Dimensionen.
#' @param value_name Spaltenname fuer die Werte (Default "id_bwi_bze").
#' @return data.frame: cell_id | x | y | <value_name>
read_nc_id_grid <- function(nc_file, varname = NULL, value_name = "id_bwi_bze") {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Paket 'ncdf4' wird benoetigt.")
  datei <- basename(nc_file)
  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  easting  <- ncdf4::ncvar_get(nc, "easting")
  northing <- ncdf4::ncvar_get(nc, "northing")

  if (is.null(varname)) {
    cand <- Filter(function(v) isTRUE(v$ndims >= 2), nc$var)
    if (length(cand) == 0)
      stop("[", datei, "] keine Variable mit >=2 Dimensionen gefunden.")
    varname <- names(cand)[1]
    message("[", datei, "] id-Variable automatisch gewaehlt: '", varname, "'.")
  }
  vals <- as.vector(ncdf4::ncvar_get(nc, varname))   # [x,y] oder [x,y,1] -> flach

  grid <- expand.grid(x = easting, y = northing)
  grid$cell_id <- seq_len(nrow(grid))
  if (length(vals) != nrow(grid))
    stop("[", datei, "] Werteanzahl (", length(vals), ") passt nicht zum ",
         "Gitter (", nrow(grid), "). Anderes Gitter als die Klimadateien?")
  grid[[value_name]] <- vals

  grid[order(grid$cell_id), c("cell_id", "x", "y", value_name)]
}


# ---- BWI: cell_id -> MASTER_ID Lookup --------------------------------------
#' BWI-Lookup quelle | cell_id | MASTER_ID aus id-Raster + Join-Tabelle bauen.
#'
#' Kette: cell_id --(id-Raster)--> id_bwi_bze --(join_csv)--> master_id_boden.
#' Zeilen ohne (sichtbares) master_id_boden werden verworfen (kein Boden).
#'
#' @param id_nc_file id-NetCDF mit id_bwi_bze (z.B. das 8002-Raster).
#' @param join_csv   BWI-BZE_Klima_Boden_Join.csv (read.csv2 / Semikolon).
#' @param varname    id-Variablenname (NULL = Autoerkennung).
#' @param id_col,master_col  Spaltennamen in join_csv.
#' @return data.frame: quelle ("BWI") | cell_id | MASTER_ID
build_bwi_master_lookup <- function(id_nc_file, join_csv,
                                    varname    = NULL,
                                    id_col     = "id_bwi_bze",
                                    master_col = "master_id_boden") {
  idg <- read_nc_id_grid(id_nc_file, varname, value_name = "id_bwi_bze")

  m <- utils::read.csv2(join_csv, stringsAsFactors = FALSE)
  for (cc in c(id_col, master_col))
    if (!cc %in% names(m))
      stop("Spalte '", cc, "' fehlt in ", basename(join_csv), ".")
  m <- m[, c(id_col, master_col)]
  names(m) <- c("id_bwi_bze", "MASTER_ID")

  out <- merge(idg[, c("cell_id", "id_bwi_bze")], m, by = "id_bwi_bze")
  out <- out[grepl("\\S", out$MASTER_ID), ]   # leere MASTER_ID (kein Boden) raus

  message(sprintf("BWI-Lookup: %d Zellen mit MASTER_ID (von %d id-Zellen).",
                  nrow(out), nrow(idg)))
  data.frame(quelle = "BWI", cell_id = out$cell_id, MASTER_ID = out$MASTER_ID,
             stringsAsFactors = FALSE)
}


# ---- MASTER_ID an WL-/Trend-Long anhaengen (generisch) ----------------------
#' MASTER_ID ueber einen Lookup (quelle | cell_id | MASTER_ID) anhaengen.
#'
#' Erwartet ein Long-df mit Spalten 'quelle' (s. wl_split_quelle) und 'id'
#' (= cell_id aus den Bruecken). Join ueber (quelle, id==cell_id); Punkte ohne
#' MASTER_ID fallen weg (inner join).
#'
#' @param long_df Long-Format mit Spalten quelle, id, ...
#' @param lookup  data.frame quelle | cell_id | MASTER_ID (BWI und/oder NR).
#' @return long_df mit zusaetzlicher Spalte MASTER_ID (nur gematchte Zeilen).
attach_master_id <- function(long_df, lookup) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  for (cc in c("quelle", "id"))
    if (!cc %in% names(long_df)) stop("long_df braucht Spalte '", cc, "'.")
  for (cc in c("quelle", "cell_id", "MASTER_ID"))
    if (!cc %in% names(lookup)) stop("lookup braucht Spalte '", cc, "'.")

  out <- dplyr::inner_join(long_df, lookup,
                           by = c("quelle" = "quelle", "id" = "cell_id"))
  if (nrow(out) == 0)
    warning("attach_master_id: kein Treffer - passen quelle/cell_id zum Lookup?",
            call. = FALSE)
  dplyr::relocate(out, "MASTER_ID")
}


# ---- Je Region eine CSV schreiben ------------------------------------------
#' Long-df nach quelle aufteilen und je Region eine CSV (Semikolon) schreiben.
#'
#' @param long_df Long-Format mit Spalte 'quelle' und MASTER_ID.
#' @param out_dir Zielordner (wird angelegt).
#' @param prefix  Dateipraefix, z.B. "WL_monthly" -> WL_monthly_BWI.csv etc.
#' @return (unsichtbar) Vektor der geschriebenen Pfade.
write_region_csvs <- function(long_df, out_dir, prefix) {
  if (!"quelle" %in% names(long_df)) stop("long_df braucht Spalte 'quelle'.")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  teile <- split(long_df, long_df$quelle)
  pfade <- vapply(names(teile), function(q) {
    f <- file.path(out_dir, paste0(prefix, "_", q, ".csv"))
    utils::write.csv2(teile[[q]], f, row.names = FALSE)
    message(sprintf("geschrieben: %s  (%d Zeilen)", f, nrow(teile[[q]])))
    f
  }, character(1))
  invisible(unname(pfade))
}


# ---- Beispiel: BWI-Monats-CSV erzeugen (auskommentiert) --------------------
# source("02_function/WL_Diagramme/nc_monthly_tables.R")
# source("02_function/WL_Diagramme/walther_lieth_input.R")   # .wl_detect_scale()
# source("02_function/WL_Diagramme/wl_master_id.R")
#
# base <- "../../../data/data_raw/extra_downloads"
# liste    <- function(id) list.files(file.path(base, id),
#                pattern = paste0("^", id, "_.*\\.nc$"), full.names = TRUE, recursive = TRUE)
# read_all <- function(files, fun) do.call(rbind,
#                lapply(files, function(f) fun(f, assign_global = FALSE)))
#
# # nur BWI-Dateien (NR separat ueber den Extract-Pfad):
# bwi_1155 <- grep("bwi[-_]bze", liste("1155"), value = TRUE, ignore.case = TRUE)
# bwi_1157 <- grep("bwi[-_]bze", liste("1157"), value = TRUE, ignore.case = TRUE)
#
# temp_df <- read_all(bwi_1155, nc.1155_function)
# prec_df <- read_all(bwi_1157, nc.1157_function)
# wl <- wl_long_from_tables(temp_df, prec_df) |> wl_split_quelle()
#
# lookup <- build_bwi_master_lookup(
#   id_nc_file = "<Pfad zum 8002-id-Raster>.nc",
#   join_csv   = "../../../data/data_raw/Klimaparameter/BWI-BZE_Klima_Boden_Join.csv")
#
# wl_master <- attach_master_id(wl, lookup)
# write_region_csvs(wl_master, out_dir = "WL_CSV/monthly", prefix = "WL_monthly")
# # -> WL_CSV/monthly/WL_monthly_BWI.csv  (App filtert hierauf)
