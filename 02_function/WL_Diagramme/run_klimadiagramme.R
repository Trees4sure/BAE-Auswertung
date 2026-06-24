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
  "wali_timeline.R",          # WaLi-Zeitstrahl 1961-2100 (Szenario-Vergleich)
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

# Ausgabe-Basis (im Projekt). Layout: <out_base>/<Region>/<Lauf>.csv
# -> 12 Ordner (BWI-BZE, NR-01..NR-11), darin je Lauf eine CSV.
out_base       <- "03_parameters/WL_diagrams"          # Monats-WL (App)
out_base_trend <- "03_parameters/WL_diagrams_trend"    # WaLi-Trend (Jahre)
geo_nr_shp <- file.path("01_data", "Grundlagen/Bodendatenbank/NR/Geodaten/GEO_NR.shp")


## 6.3  Monats-Klima (1155 Temp / 1157 Niederschlag) -> WL-Long-Format --------
# Pattern am Dateinamen-Anfang verankern (^<id>_), damit sich z.B. keine
# 1155-Datei in die 1157-Liste mischt. Alle Dateien je Parameter einlesen und
# stapeln; jede Datei meldet sich per message() mit ihrem 'name'.
liste    <- function(id) list.files(file.path(dir_extra, id),
                                    pattern = paste0("^", id, "_.*\\.nc$"),
                                    full.names = TRUE, recursive = TRUE)
read_all <- function(files, fun){
  do.call(rbind, lapply(files, function(f) fun(f, assign_global = FALSE)))}

# BWI laeuft ueber den Tabellen-Pfad (synthetische .nc-Koordinaten -> MASTER_ID
# spaeter via geom_bwi/7001-7002). NR NICHT hier stapeln - das gaebe eine
# Riesentabelle (~20 Mio. Zeilen); NR laeuft streamend in 6.8.
bwi_1155 <- grep("bwi[-_]bze", liste("1155"), value = TRUE, ignore.case = TRUE)
bwi_1157 <- grep("bwi[-_]bze", liste("1157"), value = TRUE, ignore.case = TRUE)

# v2/v3-Laeufe (RCP45) gesondert behandeln: aus dem Hauptlauf ausschliessen und
# separat aufheben (muessen lt. Notiz noch nachprediziert werden; betrifft auch
# den Niederschlag). ACHTUNG Muster "_v[23]" - NICHT "[_v23]": letzteres ist eine
# Zeichenklasse und matcht das '_' in JEDEM Dateinamen.
bwi_1157_v23 <- grep("_v[23]", bwi_1157, value = TRUE)
bwi_1157     <- grep("_v[23]", bwi_1157, value = TRUE, invert = TRUE)

temp_df  <- read_all(bwi_1155, nc.1155_function)
prec_df  <- read_all(bwi_1157, nc.1157_function)
wl_month <- wl_long_from_tables(temp_df, prec_df)   # id|quelle|Zeitlauf|Monat|T_mean|P_sum (+x,y)
wl_month %>% glimpse

# Zeitlauf ist jetzt BEREINIGT (ohne "bwi-bze_") -> Laeufe direkt benennbar.
# Optional nur EINEN Lauf erzeugen (spart die Millionen Zeilen der anderen):
#   wl_month_one <- wl_long_from_tables(temp_df, prec_df, runs = "OBS_DWD_1991-2020")

# Schnellcheck: ein Punkt, FESTER Lauf-Name (nicht wl_month$Zeitlauf[1] - bei
# Millionen Zeilen muesste man den Index erst raten). 

# Kopf-Metadaten folgen 6.5 !!
plot_walther_lieth_from_long(wl_month, id_val = 1, run = "OBS_DWD_1991-2020")


## 6.4  Jahres-Klima-Rasters fuer BWI + NR (1049 = MAT, 1050 = MAP) ------------
KS_model_vars       <- c(1049, 1050)
KS_model_vars.names <- c("MAT", "MAP")
var_filter          <- paste(KS_model_vars, collapse = "|")

# alle .nc unter Klimaparameter, ESRI-Sidecars (.aux.xml) raus
nc_files.nc <- list.files(dir_klima, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
nc_files.nc <- nc_files.nc[!grepl("\\.aux\\.xml$", nc_files.nc)]

# v2/v3 (RCP45: ECECMO=v2, MPICLM=v3) sind die NACHPREDIZIERTEN Niederschlaege.
# Statt zu filtern werden sie als EIGENE Laeufe gefuehrt: pro Lauf wird die Temp
# (1049, nur v1) mit JEDER vorhandenen 1050-Version zu einem eigenen Listen-
# Element gekoppelt. Original -> "<run>", korrigiert -> "<run>_v2"/"_v3". So
# bleibt das (fehlerbehaftete) Original-1050 erhalten - mit ihm wurde ja weiter
# gerechnet - und v2/v3 liegen direkt vergleichbar daneben. Jeder Lauf 30/30.
expand_precip_versions <- function(files, run) {
  temp   <- grep("1049", files, value = TRUE)
  precip <- grep("1050", files, value = TRUE)
  # BWI legt "_v2" ab, NR "-v2" -> [_-] deckt beide Schreibweisen ab.
  suffix <- ifelse(grepl("[_-]v[23]", precip), sub(".*([_-]v[23]).*", "\\1", precip), "")
  setNames(lapply(precip, function(p) c(temp, p)), paste0(run, suffix))
}

# -- BWI: je Klimalauf ein Multi-Layer-Raster (Layer = Jahre x MAT/MAP) --------
nc_BWI_dir    <- list.files(dir_klima, pattern = "BWI_BZE", full.names = TRUE)
run_names     <- list.files(nc_BWI_dir)[20:57]                 # Laufnamen (Ordner)
bwi_var_files <- grep(var_filter, grep("BWI_BZE", nc_files.nc, value = TRUE), value = TRUE)

bwi_files_split <- setNames(
  lapply(run_names, function(r) grep(r, bwi_var_files, value = TRUE)), run_names)
bwi_files_split <- do.call(c, unname(
  Map(expand_precip_versions, bwi_files_split, names(bwi_files_split))))
nc.grep.variables_BWI_KS <- lapply(bwi_files_split, terra::rast)

# -- NR: Nachbarschaftsregionen NR-01 .. NR-11 --------------------------------
# nr_var_files bleibt OHNE v2/v3 (so nutzt es der NR-Trend in 6.8); die 6.4-
# Rasterliste wird dagegen aus ALLEN Versionen (mit Suffix) aufgebaut.
nr_seq          <- sprintf("NR-%02d", 1:11)
nr_var_files    <- grep(var_filter, grep("NR-", nc_files.nc, value = TRUE), value = TRUE)
nr_var_files_v2 <- grep("[_-]v[23]", nr_var_files, value = TRUE)                 # nachprediziert
nr_var_files    <- grep("[_-]v[23]", nr_var_files, value = TRUE, invert = TRUE)  # ohne v2/v3 -> 6.8

nr_files_split <- setNames(
  lapply(nr_seq, function(r)
    grep(r, c(nr_var_files, nr_var_files_v2), value = TRUE)), nr_seq)
nr_files_split <- do.call(c, unname(
  Map(expand_precip_versions, nr_files_split, names(nr_files_split))))
nc_NR_select_data <- lapply(nr_files_split, terra::rast)


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

#  Test
# wl_month_join <- left_join(wl_month, MRS_Bod_Klima_Schl, by = c("id" = "id_bwi_bze"))
# plot_walther_lieth_from_long(wl_month_join, id_val = 1, run = "OBS_DWD_1991-2020")


## 6.6  Lauf-Mittelwerte (MAT/MAP) -> Long-Format -> RDS -----------------------
# Jahres-Layer je Lauf zu einem Mittelwert je Variable aggregieren. Achtung:
# einige Laeufe haben nur 21 bzw. 29 Jahre -> var_index entsprechend setzen.
n_years_for <- function(nm) {
  nm <- sub("[_-]v[23]$", "", nm)     # Versions-Suffix ignorieren (_v2/_v3, -v2/-v3)
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

if (!dir.exists(out_base)) dir.create(out_base, recursive = TRUE)
saveRDS(nc.grep.variables_df_KS_long,
        file.path(out_base, "BWI_Klimadaten_MATMAP.RDS"))


## 6.7  Diagramme -------------------------------------------------------------
runs_auswahl <- c("OBS_DWD_1961-1990", "RCP85_MPICLM_2071-2100")

# -- WaLi-Trend (Jahresverlauf 1049/1050) aus den BWI-Rastern -----------------
# Fuer Klick-Diagramme IMMER runs=/ids= filtern (sonst >100 Mio. Zeilen).
ts <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi,
                             runs = runs_auswahl, ids = 1)
nrow(ts)   # erwartet ~30 je Lauf

plot_wali_trend_from_long(ts, id_val = 1, run = runs_auswahl[1])
compare_wali_trend(ts, id_val = 1, runs = runs_auswahl)   # facets (ohne Delta-Leiste)

# -- Zeitstrahl 1961-2100: ALLE Laeufe EINES Punktes ueberlagert (Szenario-Pfade)
# Achtung ids=1 setzen (sonst alle Punkte x Jahre x Laeufe). Differenzen direkt
# sichtbar -> ersetzt die separate Mittel-Shift-Leiste.
ts_all <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi, ids = 1)
plot_wali_timeline(ts_all, id_val = 1)
plot_wali_timeline(ts_all, id_val = 1, prec_mode = "diff",
                   ref_run = "OBS_DWD_1961-1990")

# ---- HIER HIER HIER HIER ----
cat("ts_all <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi, ids = 1)
Error in wali_trend_one_run(rast_list[[run]], run, geom = geom, scale = scale,  :
                              Lauf RCP45_ECECMO_1961-1990: unterschiedlich viele Temp-/Niederschlags-Layer (30 vs. 60).
                            Called from: wali_trend_one_run(rast_list[[run]], run, geom = geom, scale = scale,
                                                            ids = ids)")


## 6.8  NR-Pfad: Polygon-Mittel je MASTER_ID -> Region-CSVs --------------------
# NR-.nc sind bereits korrekt georeferenziert (EPSG:25832, echte Meter) -> die
# x/y aus .nc_layer_table reichen; je Datei wird ein Raster gebaut und ueber die
# StoKa/Boden-Polygone (GEO_NR.shp) gemittelt. quelle = "NR-01".."NR-11".
# (geo_nr_shp ist in 6.2 gesetzt.)
polygons   <- load_nr_polygons(geo_nr_shp)   # Spalten MASTER_ID, nbrg

# Geo + Hoehe je MASTER_ID fuer den WL-Kopf (die NR-Klima-.nc tragen das nicht):
#   Lon/Lat  = Polygon-Zentroid (EPSG:25832, Rechts-/Hochwert wie BWI x_/y_25832)
#   altitude = mittlere DGM-Hoehe je MASTER_ID (vorberechnet in DGM_NR.csv;
#              fwrite -> ";"-getrennt, Punkt-Dezimal -> fread, NICHT read.csv2)
dgm_nr_path <- "DGM_NR.csv"                   # ggf. Pfad anpassen
ctr   <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(polygons)))
nr_xy <- aggregate(cbind(Lon = ctr[, 1], Lat = ctr[, 2]),
                   by = list(MASTER_ID = as.character(polygons$MASTER_ID)), FUN = mean)
dgm_nr <- data.table::fread(dgm_nr_path, sep = ";")
nr_geo <- merge(
  nr_xy,
  data.frame(MASTER_ID = as.character(dgm_nr$MASTER_ID), altitude = dgm_nr$Elevation),
  by = "MASTER_ID", all.x = TRUE)            # MASTER_ID | Lon | Lat | altitude

# -- Monat (1155/1157): pro (Region, Lauf) STREAMEND statt alles auf einmal.
#    Je 1155-Datei den 1157-Partner am Datei-Stamm (Region_Lauf) ziehen, nur
#    diesen einen Lauf mitteln und sofort als <out_base>/<Region>/<Lauf>.csv
#    ablegen -> kurze Ladezeit/wenig RAM, Zwischenstaende stehen direkt auf Platte.
nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)

nr_stem    <- function(f) sub("\\.nc$", "", sub("^[0-9]+_", "", basename(f)))  # Region_Lauf
nr_1157_by <- setNames(nr_1157, nr_stem(nr_1157))            # Stamm -> 1157-Datei (exakt)

for (tf in nr_1155) {
  pf <- nr_1157_by[[nr_stem(tf)]]                            # passender Niederschlag
  if (is.null(pf)) { warning("kein 1157 zu ", basename(tf), call. = FALSE); next }
  m <- wl_month_nr_from_files(tf, pf, polygons)              # nur dieser (Region, Lauf)
  m$MASTER_ID <- as.character(m$MASTER_ID)
  m <- dplyr::left_join(m, nr_geo, by = "MASTER_ID")         # + Lon/Lat/altitude
  write_run_csvs(m, out_dir = out_base)                      # -> out_base/NR-01/<Lauf>.csv
}

# Deskriptives Regions-Diagramm direkt aus der fertigen CSV (Mittel ueber alle
# MASTER_ID der Region) - liest die CSV, nicht die .nc:
#   wl_region_diagram(file.path(out_base, "NR-08", "OBS_DWD_1991-2020.csv"))

# -- Trend (1049/1050): analog STREAMEND je (Region, Lauf). "alt"-Varianten raus;
#    nr_var_files (aus 6.4) ist bereits v2/v3-frei. Laeufe mit 21/29/30 Jahren ok.
nr_var_files_use <- grep("alt", nr_var_files, value = TRUE, invert = TRUE)
nr_1049 <- grep("1049", nr_var_files_use, value = TRUE)
nr_1050 <- grep("1050", nr_var_files_use, value = TRUE)

nr_1050_by <- setNames(nr_1050, nr_stem(nr_1050))
for (tf in nr_1049) {
  pf <- nr_1050_by[[nr_stem(tf)]]
  if (is.null(pf)) { warning("kein 1050 zu ", basename(tf), call. = FALSE); next }
  tr <- wl_trend_nr_from_files(tf, pf, polygons)
  tr$MASTER_ID <- as.character(tr$MASTER_ID)
  tr <- dplyr::left_join(tr, nr_geo, by = "MASTER_ID")       # + Lon/Lat/altitude
  write_run_csvs(tr, out_dir = out_base_trend)               # -> out_base_trend/NR-01/<Lauf>.csv
}


## 6.9  BWI -> MASTER_ID je Region/Lauf (abgelegte App-Daten) ------------------
# Ziel: EINMAL vorrechnen + ablegen; die App liest nur die passende Lauf-CSV und
# ruft plot_walther_lieth_from_long(..., id_col = "MASTER_ID"). Dazu cell_id ->
# MASTER_ID anhaengen und je MASTER_ID mitteln (mehrere Zellen je MASTER_ID ->
# ein Wert), damit je MASTER_ID genau 12 Monatszeilen bleiben. wl_month traegt
# aus 6.5 schon quelle="BWI" + altitude/Lon/Lat.
# ACHTUNG: Spaltennamen der Join-CSV ggf. anpassen (id_col=/master_col=).
bwi_lookup <- build_bwi_master_lookup(
  id_nc_file = grep("8002", bwi_extra, value = TRUE),
  join_csv   = file.path(dir_klima, "BWI-BZE_Klima_Boden_Join.csv"))

wl_bwi_master <- wl_month %>%
  attach_master_id(bwi_lookup) %>%     # cell_id -> MASTER_ID
  wl_aggregate_master() %>%            # Mittel je MASTER_ID (+ Metadaten)
  dplyr::mutate(quelle = "BWI-BZE")    # Ordnername wie gewuenscht

write_run_csvs(wl_bwi_master, out_dir = out_base)
# -> 03_parameters/WL_diagrams/BWI-BZE/OBS_DWD_1991-2020.csv ... (je Lauf)
# App-Aufruf (Beispiel):
#   bwi <- read.csv2(file.path(out_base, "BWI-BZE/OBS_DWD_1991-2020.csv"))
#   plot_walther_lieth_from_long(bwi, id_val = <MASTER_ID>,
#                                run = "OBS_DWD_1991-2020", id_col = "MASTER_ID")


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
