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
  # Idempotent: ist 'quelle' schon da (z.B. split_quelle=TRUE in den Bruecken),
  # nichts tun - sonst wuerde der bereits bereinigte Zeitlauf zu quelle=NA fuehren.
  if ("quelle" %in% names(df)) return(df)
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
#' @param geom       optional: data.frame mit Punkt-Metadaten (z.B. geom_bwi),
#'                   verknuepft ueber \code{geom_id} == id_bwi_bze. Die in
#'                   \code{geom_cols} genannten Spalten (altitude/Lon/Lat) werden
#'                   in den Lookup uebernommen -> spaeter im Plot-Kopf sichtbar.
#' @param geom_id    Spalte in \code{geom} mit der BWI-Punkt-id (Default "id").
#' @param geom_cols  zu uebernehmende Metadaten-Spalten (Default altitude/Lon/Lat).
#' @return data.frame: quelle ("BWI") | cell_id | MASTER_ID [| altitude | Lon | Lat]
build_bwi_master_lookup <- function(id_nc_file, join_csv,
                                    varname    = NULL,
                                    id_col     = "id_bwi_bze",
                                    master_col = "master_id_boden",
                                    geom       = NULL,
                                    geom_id    = "id",
                                    geom_cols  = c("altitude", "Lon", "Lat")) {
  idg <- read_nc_id_grid(id_nc_file, varname, value_name = "id_bwi_bze")

  m <- utils::read.csv2(join_csv, stringsAsFactors = FALSE)
  for (cc in c(id_col, master_col))
    if (!cc %in% names(m))
      stop("Spalte '", cc, "' fehlt in ", basename(join_csv), ".")
  m <- m[, c(id_col, master_col)]
  names(m) <- c("id_bwi_bze", "MASTER_ID")

  out <- merge(idg[, c("cell_id", "id_bwi_bze")], m, by = "id_bwi_bze")
  out <- out[grepl("\\S", out$MASTER_ID), ]   # leere MASTER_ID (kein Boden) raus

  res <- data.frame(quelle = "BWI", cell_id = out$cell_id,
                    MASTER_ID = out$MASTER_ID, id_bwi_bze = out$id_bwi_bze,
                    stringsAsFactors = FALSE)

  # optional Punkt-Metadaten (Hoehe/Lon/Lat) ueber die BWI-Punkt-id anhaengen
  if (!is.null(geom)) {
    if (!geom_id %in% names(geom))
      stop("geom_id '", geom_id, "' fehlt in geom (vorhanden: ",
           paste(names(geom), collapse = ", "), ").")
    have <- intersect(geom_cols, names(geom))
    if (length(have) == 0)
      warning("Keine der geom_cols (", paste(geom_cols, collapse = ", "),
              ") in geom gefunden - keine Metadaten angehaengt.", call. = FALSE)
    g <- as.data.frame(geom)[, c(geom_id, have), drop = FALSE]
    names(g)[1] <- "id_bwi_bze"
    g <- g[!duplicated(g$id_bwi_bze), ]
    res <- merge(res, g, by = "id_bwi_bze", all.x = TRUE)
  }

  message(sprintf("BWI-Lookup: %d Zellen mit MASTER_ID (von %d id-Zellen)%s.",
                  nrow(res), nrow(idg),
                  if (!is.null(geom)) " inkl. Geometrie" else ""))
  res$id_bwi_bze <- NULL
  res[order(res$cell_id), ]
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


# ---- Zell-Long auf MASTER_ID-Mittel verdichten (App-Schluessel) -------------
#' Zell-genaues Long-df (id=cell_id) je MASTER_ID mitteln.
#'
#' Mehrere Klimazellen koennen auf dieselbe MASTER_ID fallen -> die App klickt
#' aber EINE MASTER_ID und erwartet je (MASTER_ID, Zeitlauf, Periode) genau einen
#' Wert. Diese Funktion mittelt T/P je Gruppe (analog zum NR-Polygon-Mittel) und
#' fuehrt die Metadaten (Hoehe/Lon/Lat) als ersten Wert mit. Danach ist 'MASTER_ID'
#' der Plot-Schluessel (plot_walther_lieth_from_long(..., id_col = "MASTER_ID")).
#'
#' @param long_df    Long mit MASTER_ID + Wert-/Perioden-Spalten (aus attach_master_id).
#' @param period_col Perioden-Spalte ("Monat" bzw. "Jahr").
#' @param value_cols zu mittelnde Wert-Spalten (Default T_mean/P_sum).
#' @param by_cols    Gruppierung neben MASTER_ID/Periode (Default quelle, Zeitlauf;
#'                   Kalenderjahr wird automatisch mitgenommen, falls vorhanden).
#' @param meta_cols  Metadaten, die je Gruppe als erster Wert erhalten bleiben.
#' @return getiltes Long: by_cols | MASTER_ID | period_col | value_cols | meta
wl_aggregate_master <- function(long_df, period_col = "Monat",
                                value_cols = c("T_mean", "P_sum"),
                                by_cols    = c("quelle", "Zeitlauf"),
                                meta_cols  = c("altitude", "Lon", "Lat")) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  if (!"MASTER_ID" %in% names(long_df)) stop("long_df braucht Spalte 'MASTER_ID'.")
  vcs  <- intersect(value_cols, names(long_df))
  if (length(vcs) == 0) stop("keine der value_cols in long_df gefunden.")
  grp  <- intersect(c(by_cols, "MASTER_ID", period_col,
                      if ("Kalenderjahr" %in% names(long_df)) "Kalenderjahr"),
                    names(long_df))
  meta <- intersect(meta_cols, names(long_df))

  long_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(vcs),  ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(meta), ~ dplyr::first(.x)),
      .groups = "drop")
}


# ---- BWI-Punkt-Metadaten (Hoehe/Lon/Lat) an ein Long-df anhaengen -----------
#' altitude/Lon/Lat aus geom_bwi ueber die id-Werte an ein Long-df haengen.
#'
#' Reihenfolge-sicher: die id (8002) wird mit read_nc_id_grid() im SELBEN
#' ncdf4-Gitter wie die Klimadaten gelesen (cell_id -> id_bwi_bze), dann wird
#' geom ueber den id-WERT (nicht die Position) verknuepft. Fuellt im Plot-Kopf
#' Hoehe/Laenge/Breite (plot_walther_lieth_from_long liest altitude/Lon/Lat).
#'
#' @param long_df   WL-/Trend-Long mit Spalte 'id' (= cell_id).
#' @param id_nc_file id-NetCDF (8002-Raster).
#' @param geom      data.frame mit Punkt-Metadaten (z.B. geom_bwi).
#' @param varname   id-Variablenname (NULL = Autoerkennung).
#' @param geom_id   Spalte in geom mit der BWI-Punkt-id (Default "id").
#' @param geom_cols zu uebernehmende Spalten (Default altitude/Lon/Lat).
#' @return long_df mit zusaetzlichen Metadaten-Spalten (left join, NA wo fehlt).
attach_bwi_geometry <- function(long_df, id_nc_file, geom, varname = NULL,
                                geom_id = "id",
                                geom_cols = c("altitude", "Lon", "Lat")) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  if (!"id" %in% names(long_df)) stop("long_df braucht Spalte 'id' (= cell_id).")
  if (!geom_id %in% names(geom))
    stop("geom_id '", geom_id, "' fehlt in geom (vorhanden: ",
         paste(names(geom), collapse = ", "), ").")

  idg  <- read_nc_id_grid(id_nc_file, varname, value_name = "id_bwi_bze")
  have <- intersect(geom_cols, names(geom))
  if (length(have) == 0)
    warning("Keine der geom_cols (", paste(geom_cols, collapse = ", "),
            ") in geom - nichts angehaengt.", call. = FALSE)
  g <- as.data.frame(geom)[, c(geom_id, have), drop = FALSE]
  names(g)[1] <- "id_bwi_bze"
  g <- g[!duplicated(g$id_bwi_bze), ]

  meta <- merge(idg[, c("cell_id", "id_bwi_bze")], g, by = "id_bwi_bze")
  meta$id_bwi_bze <- NULL
  dplyr::left_join(long_df, meta, by = c("id" = "cell_id"))
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


# ---- Je (Region, Lauf) eine CSV: out_dir/<Region>/<Lauf>.csv ----------------
#' Long-df nach Region UND Lauf splitten -> Ordner je Region, darin eine CSV je
#' Lauf. Genau das App-Layout: Klick waehlt Region + Lauf -> es wird direkt die
#' passende, schon vorgefilterte Datei geladen.
#'
#'   out_dir/
#'     BWI-BZE/ OBS_DWD_1961-1990.csv ... (je Lauf eine)
#'     NR-01/   ...
#'
#' @param long_df    Long mit Region- und Lauf-Spalte (+ MASTER_ID, Werte).
#' @param out_dir    Basisordner (wird angelegt).
#' @param region_col Spalte mit der Region (Default "quelle").
#' @param run_col    Spalte mit dem Lauf (Default "Zeitlauf").
#' @return (unsichtbar) Vektor der geschriebenen Pfade.
write_run_csvs <- function(long_df, out_dir, region_col = "quelle",
                           run_col = "Zeitlauf") {
  for (cc in c(region_col, run_col))
    if (!cc %in% names(long_df)) stop("long_df braucht Spalte '", cc, "'.")
  pfade <- character(0)
  for (rg in unique(long_df[[region_col]])) {
    d_rg   <- long_df[long_df[[region_col]] == rg, , drop = FALSE]
    dir_rg <- file.path(out_dir, as.character(rg))
    if (!dir.exists(dir_rg)) dir.create(dir_rg, recursive = TRUE)
    laeufe <- unique(d_rg[[run_col]])
    for (rn in laeufe) {
      d <- d_rg[d_rg[[run_col]] == rn, , drop = FALSE]
      f <- file.path(dir_rg, paste0(rn, ".csv"))
      # fwrite (";"-getrennt, PUNKT-Dezimal) -> passend mit fread einlesen,
      # NICHT read.csv2 (das erwartet Komma-Dezimal -> Character-Spalten).
      data.table::fwrite(d, f, sep = ";")
      pfade <- c(pfade, f)
    }
    message(sprintf("%s: %d Lauf-CSVs -> %s", rg, length(laeufe), dir_rg))
  }
  invisible(pfade)
}


# ---- Deskriptives Regions-/Sammel-Diagramm aus einer abgelegten CSV ---------
#' Walther-Lieth-Diagramm aus einer fertigen Region/Lauf-CSV.
#'
#' Liest NICHT die .nc, sondern die in write_run_csvs() abgelegte CSV
#' (<out>/<Region>/<Lauf>.csv) und mittelt T_mean/P_sum - und, falls vorhanden,
#' altitude/Lon/Lat - ueber ALLE MASTER_ID der Datei. Ergebnis: ein in sich
#' geschlossenes Regions-Klimadiagramm (Kopf zeigt Mittelhoehe + Jahresmittel).
#' Funktioniert fuer NR-Regionen (quelle = "NR-XX") wie fuer BWI ("BWI-BZE").
#'
#' @param csv_path Pfad zur Region/Lauf-CSV (fwrite: ";"-getrennt, Punkt-Dezimal
#'   -> mit fread lesen, NICHT read.csv2).
#' @param name     optionaler Titel (Default "<quelle> (Regionsmittel)").
#' @param ...      an plot_walther_lieth_from_long() weitergereicht.
#' @return ggplot-/patchwork-Objekt.
wl_region_diagram <- function(csv_path, name = NULL, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  d <- as.data.frame(data.table::fread(csv_path))
  for (cc in c("quelle", "Zeitlauf", "Monat", "T_mean", "P_sum"))
    if (!cc %in% names(d)) stop("Spalte '", cc, "' fehlt in ", basename(csv_path), ".")

  meta <- intersect(c("altitude", "Lon", "Lat", "X_Centroid", "Y_Centroid"), names(d))
  m <- d %>%
    dplyr::group_by(.data$quelle, .data$Zeitlauf, .data$Monat) %>%
    dplyr::summarise(
      T_mean = mean(.data$T_mean, na.rm = TRUE),
      P_sum  = mean(.data$P_sum,  na.rm = TRUE),
      dplyr::across(dplyr::all_of(meta), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop")

  plot_walther_lieth_from_long(
    m, id_val = m$quelle[1], run = m$Zeitlauf[1], id_col = "quelle",
    name = if (is.null(name)) paste0(m$quelle[1], " (Regionsmittel)") else name, ...)
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
