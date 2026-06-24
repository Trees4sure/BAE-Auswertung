# =============================================================================
# 6. Walther-Lieth- / WaLi-Trend-Diagramme  -  Daten laden & Diagramme bauen
#
# Ablauf:
#   6.1  Pakete + Funktionen sourcen
#   6.2  Pfade (einmal zentral)
#   6.3  Monats-Klima (1155/1157) -> Monats-WL-Long-Format
#   6.4  Jahres-Klima-Rasters (1049/1050) fuer BWI + NR laden
#   6.5  BWI-Geometrie + Schluesseltabelle
#   6.6  Lauf-Mittelwerte (MAT/MAP) -> Long-Format -> RDS
#   6.7  Diagramme: Monats-WL, WaLi-Trend, Klimalauf-Vergleich
# =============================================================================


## 6.1  Pakete + Funktionen ---------------------------------------------------
library(terra)
library(dplyr)
library(tidyr)

wl_dir <- "02_function/WL_Diagramme"
invisible(lapply(c(
  "nc_monthly_tables.R",      # nc.115x/nc.10xx-Leser + Bruecken (ncdf4-Pfad)
  "walther_lieth_input.R",    # .wl_detect_scale(), build_walther_lieth_input()
  "plot_walther_lieth.R",     # Monats-WL-Plot
  "wali_trend.R",             # WaLi-Trend (Jahresverlauf)
  "walther_lieth_helpers.R",  # geteilte Skalierung/Theme/Palette
  "walther_lieth_compare.R",  # Monats-WL-Vergleich
  "wali_trend_compare.R",     # WaLi-Trend-Vergleich
  "recommendation_strip.R",   # Empfehlungs-Leiste
  "wl_master_id.R",           # MASTER_ID-Anbindung + write_region_csvs (BWI)
  "wl_master_id_nr.R"         # NR-Pfad: Polygon-Mittel -> MASTER_ID (NR)
), function(f) source(file.path(wl_dir, f))))


## 6.2  Pfade -----------------------------------------------------------------
data_raw  <- "../../../data/data_raw"
dir_extra <- file.path(data_raw, "extra_downloads")   # Monats-/Jahres-Klimatologien
dir_klima <- file.path(data_raw, "Klimaparameter")    # MRS BWI/NR Jahres-Rasters


## 6.3  Monats-Klima (1155 Temp / 1157 Niederschlag) -> WL-Long-Format --------
# Pattern am Dateinamen-Anfang verankern (^<id>_), damit sich z.B. keine
# 1155-Datei in die 1157-Liste mischt. Alle Dateien je Parameter einlesen und
# stapeln; jede Datei meldet sich per message() mit ihrem 'name'.
liste    <- function(id) list.files(file.path(dir_extra, id),
                                    pattern = paste0("^", id, "_.*\\.nc$"),
                                    full.names = TRUE, recursive = TRUE)
read_all <- function(files, fun)
  do.call(rbind, lapply(files, function(f) fun(f, assign_global = FALSE)))

# BWI laeuft ueber den Tabellen-Pfad (synthetische .nc-Koordinaten -> MASTER_ID
# spaeter via geom_bwi/7001-7002). NR NICHT hier stapeln - das gaebe eine
# Riesentabelle (~20 Mio. Zeilen); NR laeuft streamend in 6.8.
bwi_1155 <- grep("bwi[-_]bze", liste("1155"), value = TRUE, ignore.case = TRUE)
bwi_1157 <- grep("bwi[-_]bze", liste("1157"), value = TRUE, ignore.case = TRUE)
temp_df  <- read_all(bwi_1155, nc.1155_function)
prec_df  <- read_all(bwi_1157, nc.1157_function)
wl_month <- wl_long_from_tables(temp_df, prec_df)   # id|Zeitlauf|Monat|T_mean|P_sum (+x,y)

# Schnellcheck: ein Punkt, erster Lauf (Kopf Hoehe/Lon/Lat noch "?" -
# Metadaten werden in 6.5 angehaengt). Optional nur EINEN Lauf vorfiltern:
#   wl_one <- wl_long_from_tables(temp_df, prec_df, runs = "OBS_DWD_1991-2020")
plot_walther_lieth_from_long(wl_month, id_val = 1, run = wl_month$Zeitlauf[1])


## 6.4  Jahres-Klima-Rasters fuer BWI + NR (1049 = MAT, 1050 = MAP) ------------
KS_model_vars       <- c(1049, 1050)
KS_model_vars.names <- c("MAT", "MAP")
var_filter          <- paste(KS_model_vars, collapse = "|")

# alle .nc unter Klimaparameter, ESRI-Sidecars (.aux.xml) raus
nc_files.nc <- list.files(dir_klima, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
nc_files.nc <- nc_files.nc[!grepl("\\.aux\\.xml$", nc_files.nc)]

# -- BWI: je Klimalauf ein Multi-Layer-Raster (Layer = Jahre x MAT/MAP) --------
nc_BWI_dir    <- list.files(dir_klima, pattern = "BWI_BZE", full.names = TRUE)
run_names     <- list.files(nc_BWI_dir)[20:57]                 # Laufnamen (Ordner)
bwi_var_files <- grep(var_filter, grep("BWI_BZE", nc_files.nc, value = TRUE), value = TRUE)

bwi_files_split <- setNames(
  lapply(run_names, function(r) grep(r, bwi_var_files, value = TRUE)), run_names)
nc.grep.variables_BWI_KS <- setNames(lapply(bwi_files_split, terra::rast), run_names)

# -- NR: Nachbarschaftsregionen NR-01 .. NR-11 --------------------------------
nr_seq       <- sprintf("NR-%02d", 1:11)
nr_var_files <- grep(var_filter, grep("NR-", nc_files.nc, value = TRUE), value = TRUE)
nr_files_split <- setNames(
  lapply(nr_seq, function(r) grep(r, nr_var_files, value = TRUE)), nr_seq)
nc_NR_select_data <- lapply(nr_files_split, terra::rast)

# Hinweis: v2/v3-Laeufe (ECECMO, MPICLM) muessen lt. Notiz noch separat
# nachprediziert werden -> ggf. hier mit grep("_v2|_v3", invert = TRUE) filtern.


## 6.5  BWI-Geometrie (eine Zeile je Rasterzelle) + Schluesseltabelle ----------
# Zusatz-Rasters mit Punkt-Metadaten (Code im Dateinamen):
#   7001 Rechtswert(Lon) | 7002 Hochwert(Lat) | 7004 id_04 |
#   7005 Hoehe           | 8002 BWI-Punkt-id (Traktecke)
bwi_extra <- list.files(nc_BWI_dir, pattern = "nc", full.names = TRUE)
rd <- function(code) terra::as.data.frame(terra::rast(grep(code, bwi_extra, value = TRUE)))

nc.BWI.rw.df    <- rd("7001")   # Lon
nc.BWI.hw.df    <- rd("7002")   # Lat
nc.BWI.id_04.df <- rd("7004")
nc.BWI.el.df    <- rd("7005")   # Hoehe
nc.BWI.id.df    <- rd("8002")   # BWI-Punkt-id

# Geometrie fuer die WL-/Trend-Eingaben (ohne id_04)
geom_bwi <- cbind(nc.BWI.rw.df, nc.BWI.hw.df, nc.BWI.el.df, nc.BWI.id.df) %>%
  dplyr::rename(Lon = x_25832, Lat = y_25832, altitude = elevation_250m, id = id)

# Hoehe/Lon/Lat an wl_month haengen (reihenfolge-sicher ueber die 8002-id-Werte)
# -> Plot-Kopf zeigt jetzt Hoehe/Laenge/Breite statt "?".
bwi_id_nc <- grep("8002", bwi_extra, value = TRUE)
wl_month  <- attach_bwi_geometry(wl_month, bwi_id_nc, geom_bwi)
plot_walther_lieth_from_long(wl_month, id_val = 1, run = wl_month$Zeitlauf[1])

# Boden-/Klima-Schluesseltabelle (fuer die spaetere Empfehlungs-Anbindung)
MRS_Bod_Klima_Schl <- read.csv2(file.path(dir_klima, "BWI-BZE_Klima_Boden_Join.csv")) %>%
  dplyr::mutate(unique_plo_SCHL = paste0(traktnummer, "_", traktecke))


## 6.6  Lauf-Mittelwerte (MAT/MAP) -> Long-Format -> RDS -----------------------
# Jahres-Layer je Lauf zu einem Mittelwert je Variable aggregieren. Achtung:
# einige Laeufe haben nur 21 bzw. 29 Jahre -> var_index entsprechend setzen.
n_years_for <- function(nm) {
  if (nm %in% c("RCP85_MPIWRF_1970-1990", "RCP85_HADWRF_1970-1990")) 21L
  else if (nm == "RCP85_HADWRF_2071-2099")                           29L
  else                                                               30L
}

nc.grep.variables_mean_BWI_KS <- Map(function(r, nm) {
  idx <- rep(KS_model_vars.names, each = n_years_for(nm))
  m   <- terra::tapp(r, index = idx, fun = mean, na.rm = TRUE)
  names(m) <- KS_model_vars.names
  m
}, nc.grep.variables_BWI_KS, names(nc.grep.variables_BWI_KS))

# Mittelwerte (alle Laeufe) als breites df: Spalten "<Lauf>.MAT" / "<Lauf>.MAP".
# (as.data.frame auf der Raster-LISTE bewusst beibehalten - liefert genau diese
#  punktierten Spaltennamen, auf die die Pivot-Regex unten aufsetzt.)
nc.grep.variables_df_KS <- terra::as.data.frame(nc.grep.variables_mean_BWI_KS, xy = FALSE)

nc.cbind.variables_df_KS <- cbind(
  nc.BWI.rw.df, nc.BWI.hw.df, nc.BWI.el.df, nc.BWI.id.df, nc.BWI.id_04.df,
  nc.grep.variables_df_KS
) %>%
  dplyr::rename(Lon = x_25832, Lat = y_25832, altitude = elevation_250m, id = id)

# Regex aus den tatsaechlichen Variablennamen bauen (frueher fest "MAT|FVegD|
# Kwb_Fveg|Rad_4.9" -> liess MAP stillschweigend wegfallen).
var_regex <- paste0("^(.*)\\.(", paste(KS_model_vars.names, collapse = "|"), ")$")

nc.grep.variables_df_KS_long <- nc.cbind.variables_df_KS %>%
  tidyr::pivot_longer(
    cols          = -c(Lon, Lat, id, altitude, id_7004),
    names_to      = c("Zeitlauf", "Variable"),
    names_pattern = var_regex,
    values_to     = "Value"
  ) %>%
  dplyr::mutate(Zeitlauf = gsub("\\.", "-", Zeitlauf)) %>%
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

saveRDS(nc.grep.variables_df_KS_long, "BWI_Klimadaten_long_RCP45v2.RDS")


## 6.7  Diagramme -------------------------------------------------------------
runs_auswahl <- c("OBS_DWD_1961-1990", "RCP85_MPICLM_2071-2100")

# -- WaLi-Trend (Jahresverlauf 1049/1050) aus den BWI-Rastern -----------------
# Fuer Klick-Diagramme IMMER runs=/ids= filtern (sonst >100 Mio. Zeilen).
ts <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi,
                             runs = runs_auswahl, ids = 1)
nrow(ts)   # erwartet ~30 je Lauf

plot_wali_trend_from_long(ts, id_val = 1, run = runs_auswahl[1])
compare_wali_trend(ts, id_val = 1, runs = runs_auswahl, mode = "both")


## 6.8  NR-Pfad: Polygon-Mittel je MASTER_ID -> Region-CSVs --------------------
# NR-.nc sind bereits korrekt georeferenziert (EPSG:25832, echte Meter) -> die
# x/y aus .nc_layer_table reichen; je Datei wird ein Raster gebaut und ueber die
# StoKa/Boden-Polygone (GEO_NR.shp) gemittelt. quelle = "NR-01".."NR-11".
geo_nr_shp <- file.path(data_raw, "Grundlagen/Bodendatenbank/NR/Geodaten/GEO_NR.shp")
polygons   <- load_nr_polygons(geo_nr_shp)   # Spalten MASTER_ID, nbrg

# -- Monat (1155/1157): streamend ueber die NR-Dateilisten (dir_extra).
nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)
wl_nr_month <- wl_month_nr_from_files(nr_1155, nr_1157, polygons)
write_region_csvs(wl_nr_month, out_dir = "WL_CSV/monthly", prefix = "WL_monthly")

# -- Trend (1049/1050): NR-Jahres-Rasters unter dir_klima (nr_var_files aus 6.4),
#    "alt"-Varianten ausgeschlossen. Laeufe mit 21/29/30 Jahren sind ok.
nr_var_files_use <- grep("alt", nr_var_files, value = TRUE, invert = TRUE)
nr_1049 <- grep("1049", nr_var_files_use, value = TRUE)
nr_1050 <- grep("1050", nr_var_files_use, value = TRUE)

wl_nr_trend <- wl_trend_nr_from_files(nr_1049, nr_1050, polygons)
write_region_csvs(wl_nr_trend, out_dir = "WL_CSV/trend", prefix = "WL_trend")
# -> WL_CSV/{monthly,trend}/WL_*_NR-01.csv ... NR-11.csv


# =============================================================================
# Optional / Demos (mit Testdaten) -- bei Bedarf einkommentieren
# =============================================================================
# ## Monats-WL-Vergleich + Empfehlungs-Leiste (Test-CSVs)
# wl   <- read.csv(file.path(wl_dir, "testdata/shift_monthly_test.csv"))
# rec  <- read.csv(file.path(wl_dir, "testdata/recommendation_test.csv"))
# runs <- c("Referenz_1991-2020", "RCP45_2071-2100", "RCP85_2071-2100")
# compare_walther_lieth(wl, 70041234, runs = runs, mode = "both")
# combine_climate_recommendation(
#   plot_walther_lieth_facets(wl, 70041234, runs = runs), rec, 70041234, runs = runs)
#
# ## Komplettdemo
# source(file.path(wl_dir, "testdata/demo_compare.R"))
