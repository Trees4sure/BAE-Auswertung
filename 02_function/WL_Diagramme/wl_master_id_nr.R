# =============================================================================
# NR-Pfad: Klimaraster je Nachbarschaftsregion -> Polygon-Mittel -> MASTER_ID
#
# Anders als BWI (Tabellen-Join ueber 7001/7002-Geometrie) ist NR eine RAEUMLICHE
# Aggregation - und sie laeuft STREAMEND ueber Datei-Listen:
#   pro NR-Datei: lesen -> Raster -> exact_extract(mean) ueber die Region-Polygone
#   -> nur die kleinen Polygon-Mittel (MASTER_ID x n Perioden) behalten, die
#   40000-Zellen-Tabelle sofort verwerfen. So entsteht NIE eine Riesentabelle
#   ueber alle Dateien (kein 20-Mio-Zeilen temp_df noetig).
# Ergebnis (Monat): MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
# Ergebnis (Trend): MASTER_ID | quelle | Zeitlauf | Jahr | Kalenderjahr | T_year | P_year
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
load_nr_polygons <- function(shp_path, master_col = "MASTER_ID"){
  if(!requireNamespace("sf", quietly = TRUE)) stop("Paket 'sf' wird benoetigt.")
  poly <- sf::st_read(shp_path, quiet = TRUE)
  if(!master_col %in% names(poly))
    stop("Spalte '", master_col, "' fehlt in ", basename(shp_path), ".")
  # nbrg aus MASTER_ID: Stellen 6-7 = "nr", Stellen 8-9 = Regionsnummer 01..11
  # (z.B. "...nr07..." -> "07" -> "NR-07").
  poly$nbrg <- paste0("NR-", substring(poly[[master_col]], 8, 9))
  if(master_col != "MASTER_ID") poly$MASTER_ID <- poly[[master_col]]
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
                            crs = "EPSG:25832"){
  if(!requireNamespace("terra", quietly = TRUE)) stop("Paket 'terra' wird benoetigt.")
  for(cc in c(x_col, y_col, value_cols))
    if(!cc %in% names(wide_run))
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
nr_extract_long <- function(r, poly_region, value_name, period_name = "Monat"){
  if(!requireNamespace("exactextractr", quietly = TRUE))
    stop("Paket 'exactextractr' wird benoetigt.")
  if(!requireNamespace("tidyr", quietly = TRUE)) stop("Paket 'tidyr' wird benoetigt.")
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


# ---- intern: Datei -> (Region, Lauf) ----------------------------------------
.nr_stem   <- function(f){ sub("\\.nc$", "", sub("^[0-9]+_", "", basename(f))) }
.nr_region <- function(f){
  sprintf("NR-%02d", suppressWarnings(as.integer(
    sub("^nr-?0*([0-9]+)_.*$", "\\1", .nr_stem(f), ignore.case = TRUE))))
}
.nr_run    <- function(f){
  sub("^(nr-?[0-9]{2}|bwi[-_]bze)_", "", .nr_stem(f), ignore.case = TRUE)
}
.nr_only   <- function(v){ v[grepl("nr-?[0-9]{2}", basename(v), ignore.case = TRUE)] }


# ---- intern: streamender Polygon-Mittel-Extract ueber Datei-Listen ----------
#' Gemeinsamer Kern fuer Monat (1155/1157) und Trend (1049/1050): liest die
#' Dateien EINZELN, mittelt je (Region, Lauf) ueber die Region-Polygone und
#' sammelt nur die kleinen Ergebnisse. value_t/value_p = Zielspalten,
#' period_name = "Monat"/"Jahr", add_calendar = TRUE haengt 'Kalenderjahr' an.
.nr_extract_files <- function(temp_files, prec_files, polygons,
                              read_temp, read_prec,
                              value_t, value_p, period_name,
                              add_calendar = FALSE, scale = NULL,
                              run_label = NULL){
  `%>%` <- dplyr::`%>%`
  # run_label gesetzt -> Temp/Prec POSITIONAL (1:1) paaren und Zeitlauf
  # ueberschreiben. Noetig fuer NR v2/v3: der nachpredizierte Niederschlag traegt
  # "-v2"/"-v3", die Temp nur den Basis-Run -> die Paarung ueber gleiche Run-Namen
  # greift sonst nicht.
  positional <- !is.null(run_label)

  temp_files <- .nr_only(temp_files); prec_files <- .nr_only(prec_files)
  if(!length(temp_files)) stop("Keine NR-Temp-Dateien uebergeben.")
  if(positional && length(prec_files) != length(temp_files))
    stop("run_label: temp_files und prec_files muessen gleich lang sein (1:1).")

  # Sicherheitsnetz: Datei-Regionen muessen sich mit den Polygon-Regionen (nbrg)
  # ueberschneiden - sonst lieber lauter Abbruch als stille Leer-CSVs.
  file_regs <- sort(unique(vapply(temp_files, .nr_region, "")))
  poly_regs <- sort(unique(polygons$nbrg))
  if(length(intersect(file_regs, poly_regs)) == 0)
    stop("Region-Mismatch: Dateien -> {", paste(file_regs, collapse = ", "),
         "}, Polygone(nbrg) -> {", paste(poly_regs, collapse = ", "),
         "}. nbrg-Ableitung in load_nr_polygons (Stellen 8-9) pruefen.")

  pkey <- if(positional) NULL else
    paste(vapply(prec_files, .nr_region, ""), vapply(prec_files, .nr_run, ""))

  ergebnisse <- vector("list", length(temp_files))
  for(i in seq_along(temp_files)){
    tf     <- temp_files[i]
    region <- .nr_region(tf)
    if(positional){
      pf  <- prec_files[i]
      run <- if(length(run_label) == 1L) run_label else run_label[i]
    } else {
      run <- .nr_run(tf)
      pf  <- prec_files[match(paste(region, run), pkey)]
    }
    if(is.na(pf)){ warning("Kein Niederschlag zu ", basename(tf), call. = FALSE); next }

    poly_r <- polygons[polygons$nbrg == region, ]
    if(nrow(poly_r) == 0){
      warning("Keine Polygone fuer ", region, ". Uebersprungen.", call. = FALSE); next
    }

    # EINE Datei lesen, mitteln, Zellen sofort verwerfen (speicherschonend)
    t_wide <- read_temp(tf, assign_global = FALSE)
    p_wide <- read_prec(pf, assign_global = FALSE)
    vc_t   <- setdiff(names(t_wide), c("cell_id", "x", "y", "name"))
    vc_p   <- setdiff(names(p_wide), c("cell_id", "x", "y", "name"))

    fac <- if(exists(".wl_detect_scale", mode = "function")){
      .wl_detect_scale(as.matrix(t_wide[, vc_t, drop = FALSE]), scale)
    } else if(is.null(scale)) 1 else scale

    t_long <- nr_extract_long(nr_build_raster(t_wide, vc_t), poly_r,
                              value_t, period_name = period_name)
    p_long <- nr_extract_long(nr_build_raster(p_wide, vc_p), poly_r,
                              value_p, period_name = period_name)

    res <- dplyr::inner_join(t_long, p_long, by = c("MASTER_ID", period_name)) %>%
      dplyr::mutate(quelle = region, Zeitlauf = run)
    res[[value_t]] <- res[[value_t]] * fac
    res[[value_p]] <- res[[value_p]] * fac
    if(isTRUE(add_calendar)){
      # Kalenderjahr aus den Jahres-Spaltennamen (Position -> Jahr); nur echte Jahre
      kj <- suppressWarnings(as.integer(vc_t)); kj[kj < 1900] <- NA_integer_
      res$Kalenderjahr <- kj[res[[period_name]]]
    }
    ergebnisse[[i]] <- res
    message(sprintf("[%s] %s: %d MASTER_ID x %d %s.", region, run,
                    dplyr::n_distinct(res$MASTER_ID), length(vc_t), period_name))
  }

  dplyr::bind_rows(ergebnisse)
}


# ---- Treiber: NR-MONAT (1155/1157) -> MASTER_ID-Long ------------------------
#' NR-Monatsdaten ins MASTER_ID-Long-Format (alle NR-Regionen + Laeufe).
#'
#' Streamt ueber die Datei-Listen (kein gestapeltes temp_df noetig). Paart Temp-
#' und Niederschlagsdatei je (Region, Lauf) am Dateinamen.
#'
#' @param temp_files,prec_files NR-Dateilisten (1155 bzw. 1157).
#' @param polygons   sf-Polygone aus load_nr_polygons() (Spalten MASTER_ID,nbrg).
#' @param read_temp,read_prec  Lesefunktionen (Default nc.1155_/nc.1157_function).
#' @param scale      NULL = Auto (.wl_detect_scale je Datei), sonst 1/0.1.
#' @return tibble: MASTER_ID | quelle | Zeitlauf | Monat | T_mean | P_sum
wl_month_nr_from_files <- function(temp_files, prec_files, polygons,
                                   read_temp = nc.1155_function,
                                   read_prec = nc.1157_function,
                                   scale = NULL, run_label = NULL){
  `%>%` <- dplyr::`%>%`
  out <- .nr_extract_files(temp_files, prec_files, polygons, read_temp, read_prec,
                           value_t = "T_mean", value_p = "P_sum",
                           period_name = "Monat", add_calendar = FALSE,
                           scale = scale, run_label = run_label)
  if(!nrow(out)) return(out)
  out %>%
    dplyr::select("MASTER_ID", "quelle", "Zeitlauf", "Monat", "T_mean", "P_sum") %>%
    dplyr::arrange(.data$quelle, .data$Zeitlauf, .data$MASTER_ID, .data$Monat)
}


# ---- Treiber: NR-TREND (Jahre, 1049/1050) -> MASTER_ID-Long -----------------
#' NR-Jahresdaten ins MASTER_ID-Trend-Long-Format (alle NR-Regionen + Laeufe).
#'
#' Wie wl_month_nr_from_files(), aber fuer die JAHRES-Produkte. Laeufe haben
#' unterschiedlich viele Jahre (21/29/30) - durch das streamende Lesen je Datei
#' ist das unproblematisch. Kalenderjahr kommt aus den Jahres-Spaltennamen.
#'
#' @param temp_files,prec_files NR-Dateilisten (1049 bzw. 1050).
#' @param polygons   sf-Polygone aus load_nr_polygons() (Spalten MASTER_ID,nbrg).
#' @param read_temp,read_prec  Lesefunktionen (Default nc.1049_/nc.1050_function).
#' @param scale      NULL = Auto (.wl_detect_scale je Datei), sonst 1/0.1.
#' @return tibble: MASTER_ID|quelle|Zeitlauf|Jahr|Kalenderjahr|T_year|P_year
wl_trend_nr_from_files <- function(temp_files, prec_files, polygons,
                                   read_temp = nc.1049_function,
                                   read_prec = nc.1050_function,
                                   scale = NULL, run_label = NULL){
  `%>%` <- dplyr::`%>%`
  out <- .nr_extract_files(temp_files, prec_files, polygons, read_temp, read_prec,
                           value_t = "T_year", value_p = "P_year",
                           period_name = "Jahr", add_calendar = TRUE,
                           scale = scale, run_label = run_label)
  if(!nrow(out)) return(out)
  out %>%
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
# # Datei-LISTEN reichen - die Treiber lesen streamend (kein Riesen-temp_df):
# nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
# nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)
# wl_nr <- wl_month_nr_from_files(nr_1155, nr_1157, polygons)
# write_region_csvs(wl_nr, out_dir = "WL_CSV/monthly", prefix = "WL_monthly")
# # -> WL_CSV/monthly/WL_monthly_NR-01.csv ... NR-11.csv
#
# # --- Trend (Jahre 1049/1050) analog:
# tr_nr <- wl_trend_nr_from_files(nr_1049, nr_1050, polygons)
# write_region_csvs(tr_nr, out_dir = "WL_CSV/trend", prefix = "WL_trend")
# # -> WL_CSV/trend/WL_trend_NR-01.csv ... NR-11.csv
