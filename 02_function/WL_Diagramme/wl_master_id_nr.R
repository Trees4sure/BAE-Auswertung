# =============================================================================
# NR-Pfad: Klimaraster je Nachbarschaftsregion -> Polygon-Mittel -> MASTER_ID
#
# Anders als BWI (Tabellen-Join ueber 7001/7002-Geometrie) ist NR eine RAEUMLICHE
# Aggregation:
#   1) aus der breiten Klimatabelle EINER NR-Datei direkt ein georeferenziertes
#      Raster bauen (terra::rast(type="xyz", crs="EPSG:25832"))
#   2) je Region mit den StoKa/Boden-Polygonen (GEO_NR.shp) verschneiden und den
#      Mittelwert je MASTER_ID berechnen (exactextractr::exact_extract, fun=mean)
#   3) ins Long-Format bringen: MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
#
# KOORDINATEN: Die NR-.nc sind bereits korrekt georeferenziert (EPSG:25832,
# 250 m) - easting/northing sind ECHTE Meter. .nc_layer_table() liefert die x/y
# also direkt richtig; es braucht WEDER 7001/7002-Raster NOCH die Predict-CSVs.
# (Nur die BWI-.nc tragen ein synthetisches 1..304-Gitter und brauchen 7001/7002
#  -> das macht der BWI-Pfad/geom_bwi separat.)
#
# Pakete: terra, sf, exactextractr, dplyr, tidyr.
# Optional: walther_lieth_input.R fuer .wl_detect_scale().
# =============================================================================


# ---- StoKa/Boden-Polygone laden + Region (nbrg) taggen ----------------------
#' GEO_NR.shp einlesen und Regions-Tag 'nbrg' ("NR-01"...) ergaenzen.
#'
#' @param shp_path  Pfad zu GEO_NR.shp.
#' @param master_col Spalte mit der MASTER_ID (Default "MASTER_ID").
#' @return sf-Objekt mit Spalten MASTER_ID, nbrg, geometry.
load_nr_polygons <- function(shp_path, master_col = "MASTER_ID") {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Paket 'sf' wird benoetigt.")
  poly <- sf::st_read(shp_path, quiet = TRUE)
  if (!master_col %in% names(poly))
    stop("Spalte '", master_col, "' fehlt in ", basename(shp_path), ".")
  # nbrg aus MASTER_ID: Stellen 6-7 = "nr", Stellen 8-9 = Regionsnummer 01..11
  # (z.B. "...nr07..." -> "07" -> "NR-07").
  poly$nbrg <- paste0("NR-", substring(poly[[master_col]], 8, 9))
  if (master_col != "MASTER_ID") poly$MASTER_ID <- poly[[master_col]]
  poly[, c("MASTER_ID", "nbrg")]
}


# ---- Eine breite (Region, Lauf)-Tabelle -> georeferenziertes Raster ----------
#' Aus den ECHTEN x/y der Tabelle + n Wert-Spalten ein n-Layer-SpatRaster bauen.
#'
#' Die Koordinaten stecken bereits in der Tabelle (Spalten x_col/y_col aus
#' .nc_layer_table) - kein Zell-id-Join, keine Zusatz-Geometrie noetig.
#'
#' @param wide_run    breite Tabelle EINER NR-Datei (x, y + Wert-Spalten).
#' @param value_cols  Namen der Wert-Spalten (Reihenfolge = Layer-Reihenfolge).
#' @param x_col,y_col Koordinatenspalten (Default "x","y").
#' @param crs         CRS-String (Default "EPSG:25832").
#' @return SpatRaster mit nach value_cols benannten Layern.
nr_build_raster <- function(wide_run, value_cols, x_col = "x", y_col = "y",
                            crs = "EPSG:25832") {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Paket 'terra' wird benoetigt.")
  for (cc in c(x_col, y_col, value_cols))
    if (!cc %in% names(wide_run))
      stop("Spalte '", cc, "' fehlt in der NR-Tabelle.")
  terra::rast(wide_run[, c(x_col, y_col, value_cols)], type = "xyz", crs = crs)
}


# ---- Polygon-Mittel je MASTER_ID -> Long ------------------------------------
#' exact_extract(mean) eines Layer-Stacks ueber die Region-Polygone.
#'
#' @param r           n-Layer-SpatRaster (Monate bzw. Jahre).
#' @param poly_region sf-Polygone EINER Region (Spalte MASTER_ID).
#' @param value_name  Zielspalte ("T_mean" bzw. "P_sum").
#' @param period_name Name der Perioden-Spalte ("Monat" bzw. "Jahr").
#' @return data.frame: MASTER_ID | <period_name> | <value_name>
nr_extract_long <- function(r, poly_region, value_name,
                            period_name = "Monat") {
  if (!requireNamespace("exactextractr", quietly = TRUE))
    stop("Paket 'exactextractr' wird benoetigt.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Paket 'tidyr' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  ex <- exactextractr::exact_extract(r, poly_region, fun = "mean",
                                     append_cols = "MASTER_ID", progress = FALSE)
  # Spalten heissen je nach Layerzahl "mean.<label>" bzw. "<label>" -> bereinigen
  val_cols <- setdiff(names(ex), "MASTER_ID")
  clean    <- sub("^mean\\.?", "", val_cols)
  names(ex)[match(val_cols, names(ex))] <- clean

  out <- ex %>%
    tidyr::pivot_longer(dplyr::all_of(clean),
                        names_to = "Label", values_to = value_name) %>%
    # Periode = POSITION im Layer-Stack (robust gegen lokalisierte Monatsnamen)
    dplyr::mutate(.period = match(.data$Label, clean)) %>%
    dplyr::select(-"Label")
  names(out)[names(out) == ".period"] <- period_name
  out
}


# ---- Treiber: NR-Long-Format ueber alle Regionen + Laeufe -------------------
#' NR-Monatsdaten ins MASTER_ID-Long-Format (alle NR-Regionen + Laeufe).
#'
#' temp_df/prec_df sind die GESTAPELTEN breiten Tabellen (read_all) - es werden
#' nur die NR-Dateien verarbeitet (am Dateinamen erkannt). Pro Datei wird aus den
#' tabelleneigenen x/y ein Raster gebaut und ueber die passenden Region-Polygone
#' gemittelt.
#'
#' @param temp_df,prec_df breite NR(+BWI)-Tabellen (cell_id|x|y|Monate|name).
#' @param polygons     sf-Polygone aus load_nr_polygons() (Spalten MASTER_ID,nbrg).
#' @param month_cols   optional: die 12 Monatsspaltennamen. NULL = automatisch
#'                     (alle Spalten ausser cell_id/x/y/name, in Tabellen-Reihenfolge).
#' @param scale        NULL = Auto (.wl_detect_scale), sonst 1/0.1.
#' @return tibble: MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
wl_long_nr_from_tables <- function(temp_df, prec_df, polygons,
                                   month_cols = NULL, scale = NULL) {
  `%>%` <- dplyr::`%>%`

  # Monatsspalten aus der Tabelle ableiten (Position = Jan..Dez)
  if (is.null(month_cols))
    month_cols <- setdiff(names(temp_df), c("cell_id", "x", "y", "name"))
  if (length(month_cols) != 12)
    stop("Erwarte 12 Monatsspalten, gefunden ", length(month_cols), " (",
         paste(month_cols, collapse = ", "), "). month_cols explizit angeben.")

  # name -> quelle ("NR-01") + Zeitlauf; quelle im nbrg-Format (mit Bindestrich!)
  key <- function(df) {
    z   <- sub("\\.nc$", "", sub("^[0-9]+_", "", df$name))   # "nr01_OBS_DWD_..."
    nr  <- grepl("^nr", z, ignore.case = TRUE)
    num <- suppressWarnings(as.integer(sub("^nr-?0*([0-9]+)_.*$", "\\1", z,
                                           ignore.case = TRUE)))
    data.frame(name     = df$name,
               quelle   = ifelse(nr, sprintf("NR-%02d", num), "BWI"),
               Zeitlauf = sub("^(nr-?[0-9]{2}|bwi[-_]bze)_", "", z,
                              ignore.case = TRUE),
               stringsAsFactors = FALSE)
  }
  tmeta <- key(temp_df); pmeta <- key(prec_df)
  is_nr <- grepl("^NR-", tmeta$quelle)
  if (!any(is_nr)) stop("Keine NR-Dateien in temp_df (name-Spalte) erkannt.")

  # Sicherheitsnetz: Datei-Regionen muessen sich mit den Polygon-Regionen (nbrg)
  # ueberschneiden - sonst lieber lauter Abbruch als stille Leer-CSVs.
  file_regs <- sort(unique(tmeta$quelle[is_nr]))
  poly_regs <- sort(unique(polygons$nbrg))
  if (length(intersect(file_regs, poly_regs)) == 0)
    stop("Region-Mismatch: Dateien -> {", paste(file_regs, collapse = ", "),
         "}, Polygone(nbrg) -> {", paste(poly_regs, collapse = ", "),
         "}. nbrg-Ableitung in load_nr_polygons (Stellen 8-9) pruefen.")

  fac <- if (exists(".wl_detect_scale", mode = "function"))
    .wl_detect_scale(as.matrix(temp_df[is_nr, month_cols, drop = FALSE]), scale) else
    if (is.null(scale)) 1 else scale

  ergebnisse <- list()
  for (nm in unique(temp_df$name[is_nr])) {
    region <- tmeta$quelle[match(nm, tmeta$name)]
    run    <- tmeta$Zeitlauf[match(nm, tmeta$name)]
    nm_p   <- pmeta$name[pmeta$quelle == region & pmeta$Zeitlauf == run][1]
    if (is.na(nm_p)) { warning("Kein Niederschlag zu ", nm, call. = FALSE); next }

    poly_r <- polygons[polygons$nbrg == region, ]
    if (nrow(poly_r) == 0) {
      warning("Keine Polygone fuer ", region, " (nbrg). Uebersprungen.", call. = FALSE)
      next
    }

    r_t <- nr_build_raster(temp_df[temp_df$name == nm, ],   month_cols)
    r_p <- nr_build_raster(prec_df[prec_df$name == nm_p, ], month_cols)

    t_long <- nr_extract_long(r_t, poly_r, "T_mean")
    p_long <- nr_extract_long(r_p, poly_r, "P_sum")

    ergebnisse[[nm]] <- dplyr::inner_join(t_long, p_long, by = c("MASTER_ID", "Monat")) %>%
      dplyr::mutate(quelle = region, Zeitlauf = run,
                    T_mean = .data$T_mean * fac, P_sum = .data$P_sum * fac)
    message(sprintf("[%s] %s: %d MASTER_ID x 12 Monate.", region, run,
                    dplyr::n_distinct(ergebnisse[[nm]]$MASTER_ID)))
  }

  dplyr::bind_rows(ergebnisse) %>%
    dplyr::select("MASTER_ID", "quelle", "Zeitlauf", "Monat", "T_mean", "P_sum") %>%
    dplyr::arrange(.data$quelle, .data$Zeitlauf, .data$MASTER_ID, .data$Monat)
}


# ---- Treiber: NR-TREND (Jahre, 1049/1050) -> MASTER_ID-Long -----------------
#' NR-Jahresdaten ins MASTER_ID-Trend-Long-Format (alle NR-Regionen + Laeufe).
#'
#' Pendant zu wl_long_nr_from_tables() fuer die JAHRES-Produkte (1049 Temp /
#' 1050 Niederschlag). Da Laeufe unterschiedlich viele Jahre haben (21/29/30),
#' werden die Dateien EINZELN gelesen (kein gemeinsames rbind) und je
#' (Region, Lauf) ueber die Region-Polygone gemittelt. Das Kalenderjahr kommt
#' aus den Jahres-Spaltennamen (NC-Zeitstempel).
#'
#' @param temp_files,prec_files NR-Dateilisten (1049 bzw. 1050).
#' @param polygons   sf-Polygone aus load_nr_polygons() (Spalten MASTER_ID,nbrg).
#' @param read_temp,read_prec  Lesefunktionen (Default nc.1049_/nc.1050_function).
#' @param scale      NULL = Auto (.wl_detect_scale je Datei), sonst 1/0.1.
#' @return tibble: MASTER_ID|quelle|Zeitlauf|Jahr|Kalenderjahr|T_year|P_year
wl_trend_nr_from_tables <- function(temp_files, prec_files, polygons,
                                    read_temp = nc.1049_function,
                                    read_prec = nc.1050_function,
                                    scale = NULL) {
  `%>%` <- dplyr::`%>%`

  stem   <- function(f) sub("\\.nc$", "", sub("^[0-9]+_", "", basename(f)))
  reg_of <- function(f) sprintf("NR-%02d", suppressWarnings(as.integer(
    sub("^nr-?0*([0-9]+)_.*$", "\\1", stem(f), ignore.case = TRUE))))
  run_of <- function(f) sub("^(nr-?[0-9]{2}|bwi[-_]bze)_", "", stem(f),
                            ignore.case = TRUE)

  is_nr <- function(v) v[grepl("nr-?[0-9]{2}", basename(v), ignore.case = TRUE)]
  temp_files <- is_nr(temp_files); prec_files <- is_nr(prec_files)
  if (!length(temp_files)) stop("Keine NR-Temp-Dateien (1049) uebergeben.")

  file_regs <- sort(unique(vapply(temp_files, reg_of, "")))
  poly_regs <- sort(unique(polygons$nbrg))
  if (length(intersect(file_regs, poly_regs)) == 0)
    stop("Region-Mismatch (Trend): Dateien -> {", paste(file_regs, collapse = ", "),
         "}, Polygone(nbrg) -> {", paste(poly_regs, collapse = ", "), "}.")

  pkey <- paste(vapply(prec_files, reg_of, ""), vapply(prec_files, run_of, ""))

  ergebnisse <- list()
  for (tf in temp_files) {
    region <- reg_of(tf); run <- run_of(tf)
    pf <- prec_files[match(paste(region, run), pkey)]
    if (is.na(pf)) { warning("Kein 1050 zu ", basename(tf), call. = FALSE); next }

    poly_r <- polygons[polygons$nbrg == region, ]
    if (nrow(poly_r) == 0) {
      warning("Keine Polygone fuer ", region, ". Uebersprungen.", call. = FALSE); next
    }

    t_wide <- read_temp(tf, assign_global = FALSE)
    p_wide <- read_prec(pf, assign_global = FALSE)
    yc_t <- setdiff(names(t_wide), c("cell_id", "x", "y", "name"))
    yc_p <- setdiff(names(p_wide), c("cell_id", "x", "y", "name"))

    fac <- if (exists(".wl_detect_scale", mode = "function"))
      .wl_detect_scale(as.matrix(t_wide[, yc_t, drop = FALSE]), scale) else
      if (is.null(scale)) 1 else scale

    r_t <- nr_build_raster(t_wide, yc_t)
    r_p <- nr_build_raster(p_wide, yc_p)
    t_long <- nr_extract_long(r_t, poly_r, "T_year", period_name = "Jahr")
    p_long <- nr_extract_long(r_p, poly_r, "P_year", period_name = "Jahr")

    # Kalenderjahr aus den Spaltennamen (Position -> Jahr); nur echte Jahre (>1900)
    kj <- suppressWarnings(as.integer(yc_t)); kj[kj < 1900] <- NA_integer_

    ergebnisse[[basename(tf)]] <-
      dplyr::inner_join(t_long, p_long, by = c("MASTER_ID", "Jahr")) %>%
      dplyr::mutate(quelle = region, Zeitlauf = run,
                    Kalenderjahr = kj[.data$Jahr],
                    T_year = .data$T_year * fac, P_year = .data$P_year * fac)
    message(sprintf("[%s] %s: %d MASTER_ID x %d Jahre.", region, run,
                    dplyr::n_distinct(ergebnisse[[basename(tf)]]$MASTER_ID),
                    length(yc_t)))
  }

  dplyr::bind_rows(ergebnisse) %>%
    dplyr::select("MASTER_ID", "quelle", "Zeitlauf", "Jahr", "Kalenderjahr",
                  "T_year", "P_year") %>%
    dplyr::arrange(.data$quelle, .data$Zeitlauf, .data$MASTER_ID, .data$Jahr)
}


# ---- Beispiel (auskommentiert) ---------------------------------------------
# source("02_function/WL_Diagramme/nc_monthly_tables.R")
# source("02_function/WL_Diagramme/walther_lieth_input.R")   # .wl_detect_scale()
# source("02_function/WL_Diagramme/wl_master_id_nr.R")
#
# polygons <- load_nr_polygons("01_data/Grundlagen/Bodendatenbank/NR/Geodaten/GEO_NR.shp")
#
# # Nur die NR-Monatsdateien einlesen (x/y sind darin schon echte EPSG:25832-Meter):
# nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
# nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)
# temp_df <- read_all(nr_1155, nc.1155_function)
# prec_df <- read_all(nr_1157, nc.1157_function)
#
# wl_nr <- wl_long_nr_from_tables(temp_df, prec_df, polygons)
# write_region_csvs(wl_nr, out_dir = "WL_CSV/monthly", prefix = "WL_monthly")
# # -> WL_CSV/monthly/WL_monthly_NR-01.csv ... NR-11.csv
#
# # --- Trend (Jahre 1049/1050): Dateien EINZELN, da unterschiedlich viele Jahre:
# tr_nr <- wl_trend_nr_from_tables(liste("1049"), liste("1050"), polygons)
# write_region_csvs(tr_nr, out_dir = "WL_CSV/trend", prefix = "WL_trend")
# # -> WL_CSV/trend/WL_trend_NR-01.csv ... NR-11.csv
