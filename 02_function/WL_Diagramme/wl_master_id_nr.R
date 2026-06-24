# =============================================================================
# NR-Pfad: Klimaraster je Nachbarschaftsregion -> Polygon-Mittel -> MASTER_ID
#
# Anders als BWI (Tabellen-Join) ist NR eine RAEUMLICHE Aggregation, analog zur
# bestehenden NR-Funktion:
#   1) aus den breiten Klimatabellen + ECHTEN EPSG:25832-Koordinaten je Zelle
#      ein georeferenziertes Raster bauen  (terra::rast(type="xyz", crs=...))
#   2) je Region mit den StoKa/Boden-Polygonen (GEO_NR.shp) verschneiden und den
#      Mittelwert je MASTER_ID berechnen (exactextractr::exact_extract, fun=mean)
#   3) ins Long-Format bringen: MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
#
# WICHTIG zu den Koordinaten: die easting/northing IN den .nc sind ein
# normiertes 1..304-Gitter, KEINE Meter. Die echten EPSG:25832-Koordinaten je
# Zelle kommen - wie bei BWI ueber 7001/7002 - aus den NR-Rechtswert/Hochwert-
# Rastern. read_real_coords() liest sie (gittergleich ueber read_nc_id_grid()).
#
# Pakete: terra, sf, exactextractr, dplyr, tidyr.
# Setzt wl_master_id.R (read_nc_id_grid, wl_split_quelle) voraus.
# =============================================================================


# ---- Echte EPSG:25832-Koordinaten je Zelle ----------------------------------
#' Rechtswert-(x) und Hochwert-(y) Raster zu einer cell_id -> (X, Y)-Tabelle.
#'
#' @param rw_nc,hw_nc  NetCDF mit Rechtswert (x_25832) bzw. Hochwert (y_25832).
#' @param varname_rw,varname_hw  Variablennamen (NULL = Autoerkennung).
#' @return data.frame: cell_id | X | Y   (EPSG:25832-Meter)
read_real_coords <- function(rw_nc, hw_nc, varname_rw = NULL, varname_hw = NULL) {
  rw <- read_nc_id_grid(rw_nc, varname_rw, value_name = "X")
  hw <- read_nc_id_grid(hw_nc, varname_hw, value_name = "Y")
  out <- merge(rw[, c("cell_id", "X")], hw[, c("cell_id", "Y")], by = "cell_id")
  out[order(out$cell_id), ]
}


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
  # nbrg aus MASTER_ID "NR_010_07_133397" -> Stellen 8-9 = "07" -> "NR-07"
  poly$nbrg <- paste0("NR-", substring(poly[[master_col]], 8, 9))
  if (master_col != "MASTER_ID") poly$MASTER_ID <- poly[[master_col]]
  poly[, c("MASTER_ID", "nbrg")]
}


# ---- Eine breite (Region, Lauf)-Tabelle -> georeferenziertes Monatsraster ----
#' Aus echten Koordinaten + 12 Monatsspalten ein 12-Layer-SpatRaster bauen.
#'
#' @param coords      cell_id | X | Y (EPSG:25832) der Region.
#' @param wide_run    breite Tabelle EINER Region+Lauf (cell_id + Monatsspalten).
#' @param month_cols  die 12 Monatsspaltennamen (Reihenfolge Jan..Dez).
#' @param crs         CRS-String (Default "EPSG:25832").
#' @return SpatRaster mit 12 nach month_cols benannten Layern.
nr_build_raster <- function(coords, wide_run, month_cols, crs = "EPSG:25832") {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Paket 'terra' wird benoetigt.")
  df <- merge(coords, wide_run[, c("cell_id", month_cols)], by = "cell_id")
  terra::rast(df[, c("X", "Y", month_cols)], type = "xyz", crs = crs)
}


# ---- Polygon-Mittel je MASTER_ID -> Long ------------------------------------
#' exact_extract(mean) eines Monatsrasters ueber die Region-Polygone.
#'
#' @param r           12-Layer-SpatRaster (Monate).
#' @param poly_region sf-Polygone EINER Region (Spalte MASTER_ID).
#' @param month_cols  die 12 Monatslabels (zur Monatszuordnung).
#' @param value_name  Zielspalte ("T_mean" bzw. "P_sum").
#' @return data.frame: MASTER_ID | Monat | <value_name>
nr_extract_long <- function(r, poly_region, month_cols, value_name) {
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

  ex %>%
    tidyr::pivot_longer(dplyr::all_of(clean),
                        names_to = "MonatLabel", values_to = value_name) %>%
    dplyr::mutate(Monat = match(.data$MonatLabel, month_cols)) %>%
    dplyr::select(-"MonatLabel")
}


# ---- Treiber: NR-Long-Format ueber alle Regionen + Laeufe -------------------
#' NR-Monatsdaten ins MASTER_ID-Long-Format (alle NR-Regionen + Laeufe).
#'
#' temp_df/prec_df sind die GESTAPELTEN breiten Tabellen (read_all) - es werden
#' nur die NR-Dateien verarbeitet (ueber wl_split_quelle erkannt). Pro Datei wird
#' ein georeferenziertes Raster gebaut und ueber die passenden Region-Polygone
#' gemittelt.
#'
#' @param temp_df,prec_df breite NR(+BWI)-Tabellen (cell_id|x|y|Monate|name).
#' @param coords_for   Funktion region ("NR-01") -> data.frame cell_id|X|Y.
#'                     (z.B. \\(reg) read_real_coords(rw_nc(reg), hw_nc(reg))).
#' @param polygons     sf-Polygone aus load_nr_polygons() (Spalten MASTER_ID,nbrg).
#' @param month_cols   12 Monatslabels (Default wie nc_monthly_tables).
#' @param scale        NULL = Auto (.wl_detect_scale), sonst 1/0.1.
#' @return tibble: MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
wl_long_nr_from_tables <- function(temp_df, prec_df, coords_for, polygons,
                                   month_cols = .wl_month_labels, scale = NULL) {
  `%>%` <- dplyr::`%>%`

  # name -> quelle (NR-XX) + Zeitlauf; nur NR behalten
  key <- function(df) {
    z <- sub("\\.nc$", "", sub("^[0-9]+_", "", df$name))
    data.frame(name = df$name,
               quelle   = toupper(sub("^(nr-?[0-9]{2})_.*$", "\\1", z, ignore.case = TRUE)),
               Zeitlauf = sub("^(bwi[-_]bze|nr-?[0-9]{2})_", "", z, ignore.case = TRUE),
               stringsAsFactors = FALSE)
  }
  tmeta <- key(temp_df); pmeta <- key(prec_df)
  is_nr <- grepl("^NR-", tmeta$quelle)

  fac <- if (exists(".wl_detect_scale", mode = "function"))
    .wl_detect_scale(as.matrix(temp_df[, month_cols, drop = FALSE]), scale) else
    if (is.null(scale)) 1 else scale

  ergebnisse <- list()
  for (nm in unique(temp_df$name[is_nr])) {
    region <- tmeta$quelle[match(nm, tmeta$name)]
    run    <- tmeta$Zeitlauf[match(nm, tmeta$name)]
    nm_p   <- pmeta$name[pmeta$quelle == region & pmeta$Zeitlauf == run][1]
    if (is.na(nm_p)) { warning("Kein Niederschlag zu ", nm, call. = FALSE); next }

    coords <- coords_for(region)
    poly_r <- polygons[polygons$nbrg == region, ]

    r_t <- nr_build_raster(coords, temp_df[temp_df$name == nm, ],   month_cols)
    r_p <- nr_build_raster(coords, prec_df[prec_df$name == nm_p, ], month_cols)

    t_long <- nr_extract_long(r_t, poly_r, month_cols, "T_mean")
    p_long <- nr_extract_long(r_p, poly_r, month_cols, "P_sum")

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


# ---- Beispiel (auskommentiert) ---------------------------------------------
# source("02_function/WL_Diagramme/nc_monthly_tables.R")
# source("02_function/WL_Diagramme/walther_lieth_input.R")
# source("02_function/WL_Diagramme/wl_master_id.R")
# source("02_function/WL_Diagramme/wl_master_id_nr.R")
#
# polygons <- load_nr_polygons("01_data/Grundlagen/Bodendatenbank/NR/Geodaten/GEO_NR.shp")
#
# # echte Koordinaten je NR-Region (NR-Rechtswert/Hochwert-Raster, analog 7001/7002):
# coords_for <- function(region) read_real_coords(
#   rw_nc = nr_rw_file(region),   # <- deine NR-Rechtswert-Datei je Region
#   hw_nc = nr_hw_file(region))   # <- deine NR-Hochwert-Datei je Region
#
# nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
# nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)
# temp_df <- read_all(nr_1155, nc.1155_function)
# prec_df <- read_all(nr_1157, nc.1157_function)
#
# wl_nr <- wl_long_nr_from_tables(temp_df, prec_df, coords_for, polygons)
# write_region_csvs(wl_nr, out_dir = "WL_CSV/monthly", prefix = "WL_monthly")
# # -> WL_CSV/monthly/WL_monthly_NR-01.csv ... NR-11.csv
