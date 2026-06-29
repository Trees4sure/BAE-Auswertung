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

# Geo + Hoehe + DGM-Lage je MASTER_ID fuer WL-Kopf und Empfehlungs-Leiste (die
# NR-Klima-.nc tragen das nicht):
#   Lon/Lat  = Polygon-Zentroid (EPSG:25832, Rechts-/Hochwert wie BWI x_/y_25832)
#   altitude = mittlere DGM-Hoehe (Elevation) je MASTER_ID; zusaetzlich Aspect/
#              Slope/Exposition/Hangseite fuer die spaetere Empfehlung. Alles aus
#              DGM_NR.csv (fwrite -> ";"-getrennt, Punkt-Dezimal -> fread, NICHT
#              read.csv2).
dgm_nr_path <- file.path("01_data", "Grundlagen/Geodaten/DGM_NR.csv")
ctr   <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(polygons)))
nr_xy <- aggregate(cbind(Lon = ctr[, 1], Lat = ctr[, 2]),
                   by = list(MASTER_ID = as.character(polygons$MASTER_ID)), FUN = mean)
dgm_nr <- data.table::fread(dgm_nr_path, sep = ";")
# Elevation -> altitude, restliche DGM-Spalten (falls vorhanden) durchreichen.
dgm_map  <- c(altitude = "Elevation", Aspect = "Aspect", Slope = "Slope",
              Exposition = "Exposition", Hangseite = "Hangseite")
dgm_have <- dgm_map[dgm_map %in% names(dgm_nr)]
dgm_sel  <- data.frame(MASTER_ID = as.character(dgm_nr$MASTER_ID),
                       setNames(lapply(dgm_have, function(cc) dgm_nr[[cc]]), names(dgm_have)),
                       check.names = FALSE, stringsAsFactors = FALSE)
nr_geo <- merge(nr_xy, dgm_sel, by = "MASTER_ID", all.x = TRUE)  # MASTER_ID|Lon|Lat|altitude|...

# Datei-Stamm (Region_Lauf, ohne ID-Praefix/.nc) + Lauf-Name (ohne Region-Praefix,
# inkl. -v2/-v3-Suffix).
nr_stem <- function(f) sub("\\.nc$", "", sub("^[0-9]+_", "", basename(f)))
nr_run  <- function(f) sub("^nr-?[0-9]{2}_", "", nr_stem(f), ignore.case = TRUE)

# -- Monat (1155/1157): pro (Region, Lauf) STREAMEND statt alles auf einmal.
#    Je 1155-Datei den 1157-Partner am Datei-Stamm (Region_Lauf) ziehen, nur
#    diesen einen Lauf mitteln und sofort als <out_base>/<Region>/<Lauf>.csv
#    ablegen -> kurze Ladezeit/wenig RAM, Zwischenstaende stehen direkt auf Platte.
nr_1155 <- grep("nr-?[0-9]{2}", liste("1155"), value = TRUE, ignore.case = TRUE)
nr_1157 <- grep("nr-?[0-9]{2}", liste("1157"), value = TRUE, ignore.case = TRUE)

nr_1157_by <- setNames(nr_1157, nr_stem(nr_1157))            # Stamm -> 1157-Datei (exakt)

for (tf in nr_1155) {
  pf <- nr_1157_by[[nr_stem(tf)]]                            # passender Niederschlag
  if (is.null(pf)) { warning("kein 1157 zu ", basename(tf), call. = FALSE); next }
  m <- wl_month_nr_from_files(tf, pf, polygons)              # nur dieser (Region, Lauf)
  m$MASTER_ID <- as.character(m$MASTER_ID)
  m <- dplyr::left_join(m, nr_geo, by = "MASTER_ID")         # + Lon/Lat/altitude
  write_run_csvs(m, out_dir = out_base)                      # -> out_base/NR-01/<Lauf>.csv
}

# NR v2/v3 (Niederschlag nachprediziert, "-v2"/"-v3"; Temp nur als Basis): die
# Basis-Temp mit dem v2/v3-Niederschlag paaren (run_label) und als EIGENER Lauf
# "<Run>-v2"/"-v3" zusaetzlich ablegen - das Original bleibt erhalten.
nr_1155_by <- setNames(nr_1155, nr_stem(nr_1155))
for (pf in grep("[_-]v[23]", nr_1157, value = TRUE)) {
  tf <- nr_1155_by[[ sub("[_-]v[23]$", "", nr_stem(pf)) ]]  # Basis-Temp (ohne Suffix)
  if (is.null(tf)) { warning("keine Basis-Temp zu ", basename(pf), call. = FALSE); next }
  m <- wl_month_nr_from_files(tf, pf, polygons, run_label = nr_run(pf))  # Lauf "...-v2"
  m$MASTER_ID <- as.character(m$MASTER_ID)
  m <- dplyr::left_join(m, nr_geo, by = "MASTER_ID")
  write_run_csvs(m, out_dir = out_base)                      # -> out_base/NR-XX/<Run>-v2.csv
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

# NR-Trend v2/v3 (aus nr_var_files_v2 in 6.4): Basis-1049 mit v2/v3-1050 paaren
# und als eigener Lauf "<Run>-v2"/"-v3" ablegen.
nr_1049_by <- setNames(nr_1049, nr_stem(nr_1049))
nr_1050_v  <- grep("1050", grep("alt", nr_var_files_v2, value = TRUE, invert = TRUE), value = TRUE)
for (pf in nr_1050_v) {
  tf <- nr_1049_by[[ sub("[_-]v[23]$", "", nr_stem(pf)) ]]
  if (is.null(tf)) { warning("keine Basis-Temp(1049) zu ", basename(pf), call. = FALSE); next }
  tr <- wl_trend_nr_from_files(tf, pf, polygons, run_label = nr_run(pf))
  tr$MASTER_ID <- as.character(tr$MASTER_ID)
  tr <- dplyr::left_join(tr, nr_geo, by = "MASTER_ID")
  write_run_csvs(tr, out_dir = out_base_trend)
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

# Lauf fuer Lauf statt 42 Mio. Zeilen in EINEM group_by: pro Zeitlauf filtern,
# auf MASTER_ID verdichten, sofort als BWI-BZE/<Lauf>.csv ablegen (kleiner RAM,
# Zwischenstaende auf Platte). wl_month traegt aus 6.5 schon altitude/Lon/Lat.
for (rn in unique(wl_month$Zeitlauf)) {
  wl_month[wl_month$Zeitlauf == rn, , drop = FALSE] %>%
    attach_master_id(bwi_lookup) %>%   # cell_id -> MASTER_ID
    wl_aggregate_master() %>%          # Mittel je MASTER_ID (+ Metadaten)
    dplyr::mutate(quelle = "BWI-BZE") %>%
    write_run_csvs(out_dir = out_base) # -> out_base/BWI-BZE/<Lauf>.csv
}
# -> 03_parameters/WL_diagrams/BWI-BZE/OBS_DWD_1991-2020.csv ... (je Lauf)
# App-Aufruf (Beispiel):
#   bwi <- data.table::fread(file.path(out_base, "BWI-BZE/OBS_DWD_1991-2020.csv"))
#   plot_walther_lieth_from_long(bwi, id_val = <MASTER_ID>,
#                                run = "OBS_DWD_1991-2020", id_col = "MASTER_ID")


## 6.10  NR Walther-Lieth-PNGs je MASTER_ID ueber ALLE Laeufe -----------------
# Aus den fertigen Region-CSVs (out_base/<Region>/<Lauf>.csv): je MASTER_ID und
# je Lauf EIN WL-Diagramm als PNG. Jede CSV = ein Lauf -> Dateistamm == Zeitlauf.
# Lon/Lat liefert der Wrapper bei Bedarf aus X_Centroid/Y_Centroid (stokpolyshp).
wl_region  <- "NR-08"
wl_ids     <- c("NR_130_08_6189", "NR_130_08_66519")
wl_png_dir <- file.path("04_results", "WL_diagrams", wl_region)   # Zielordner
dir.create(wl_png_dir, recursive = TRUE, showWarnings = FALSE)

# Alle Lauf-CSVs der Region einsammeln (38 erwartet).
wl_csvs <- list.files(file.path(out_base, wl_region),
                      pattern = "\\.csv$", full.names = TRUE)

for (csv in wl_csvs) {
  run <- sub("\\.csv$", "", basename(csv))   # Lauf = Dateistamm = Zeitlauf
  # fread statt read.csv2: die CSVs haben PUNKT-Dezimal -> read.csv2 (Komma)
  # liefe Character zurueck. as.data.frame, damit die Basis-Indizierung im
  # Wrapper (df[cond, , drop=FALSE]) sauber bleibt.
  d   <- as.data.frame(data.table::fread(csv))
  for (id in wl_ids) {
    # Fehlt eine ID/Kombination in einer CSV, nur warnen statt den Loop abbrechen.
    p <- tryCatch(
      plot_walther_lieth_from_long(d, id_val = id, run = run,
                                   id_col = "MASTER_ID"),
      error = function(e) { warning(id, " / ", run, ": ", conditionMessage(e),
                                    call. = FALSE); NULL })
    if (is.null(p)) next
    ggplot2::ggsave(
      filename = file.path(wl_png_dir, paste0(id, "_", run, ".png")),
      plot = p, width = 8, height = 6, dpi = 300)   # 2400x1800 px
  }
}
# -> 2 MASTER_IDs x 38 Laeufe = 76 PNG in 04_results/WL_diagrams/NR-08/


## 6.11  NR Walther-Lieth-VERGLEICHE je MASTER_ID (Referenz vs. Modell-Laeufe) -
# Wie 6.10, aber als VERGLEICH: oben die Klimadiagramme nebeneinander
# (Referenz | 2021-2050 | 2071-2100), darunter die Differenz Zukunft - Referenz.
# Referenz ist OBS_DWD_1991-2020; je Modell (Szenario+GCM) dessen Zukunftslaeufe.
#
# BEWUSST FLACH: alles in den Schleifen, beide Diagramme als direkte ggplot()-
# Aufrufe (Achsen/Farben/Layer hier im Skript aenderbar, kein Helfer dazwischen).
# Die Region-CSVs liegen je Lauf einzeln vor (Spalte MASTER_ID, Dateistamm =
# Zeitlauf); pro Modell werden die noetigen CSVs eingelesen und gestapelt.

library(ggplot2)
library(patchwork)

wl_ref_run <- "OBS_DWD_1991-2020"           # Referenzlauf (links/oben)
wl_monlab  <- c("J","F","M","A","M","J","J","A","S","O","N","D")
wl_cmp_dir <- file.path("04_results", "WL_compare", wl_region)
dir.create(wl_cmp_dir, recursive = TRUE, showWarnings = FALSE)

# --- Laufnamen in Periode (JJJJ-JJJJ) und Modell (Name ohne Periode) zerlegen --
# [0-9]{4}, NICHT (19|20)..: sonst faellt das Endjahr 2100 (in 2071-2100) weg.
all_runs <- sub("\\.csv$", "", basename(wl_csvs))             # Lauf = Dateistamm
period   <- sub(".*([0-9]{4}-[0-9]{4}).*", "\\1", all_runs)   # "2071-2100"
start    <- substr(period, 1, 4)                              # "2071"
model    <- sub("[0-9]{4}-[0-9]{4}", "", all_runs)           # Name ohne Periode
model    <- sub("^_|_$", "", gsub("[_-]+", "_", model))       # "RCP85_MPIWRF"

# Zukunft = Anfangsjahr 2021 (nah) oder 2071 (fern). Modelle mit >=1 Zukunftslauf.
is_future     <- start %in% c("2021", "2071")
future_models <- unique(model[is_future])

# --- je Modell und ID ein Vergleichsdiagramm ---------------------------------
for (mod in future_models) {

  # Laeufe dieses Modells: Referenz + dessen Zukunftslaeufe, nach Periode sortiert.
  sel  <- model == mod & is_future
  futs <- all_runs[sel][order(period[sel])]   # z.B. ..._2021-2050, ..._2071-2100
  runs <- c(wl_ref_run, futs)

  # benoetigte CSVs einlesen, auf die Wunsch-IDs reduzieren, zu EINER Tabelle.
  dat <- data.frame()
  for (r in runs) {
    f <- file.path(out_base, wl_region, paste0(r, ".csv"))
    if (!file.exists(f)) { warning("fehlt: ", f, call. = FALSE); next }
    d <- as.data.frame(data.table::fread(f))
    d <- d[d$MASTER_ID %in% wl_ids, c("MASTER_ID", "Monat", "T_mean", "P_sum")]
    d$Zeitlauf <- r
    dat <- rbind(dat, d)
  }
  dat$Zeitlauf <- factor(dat$Zeitlauf, levels = runs)   # Referenz bleibt links

  for (id in wl_ids) {
    pt <- dat[dat$MASTER_ID == id, ]
    if (nrow(pt) == 0) { warning(id, " fehlt fuer ", mod, call. = FALSE); next }

    # ---- oben: Klimadiagramme nebeneinander (ein Panel je Lauf) --------------
    # Walther-Lieth-Kopplung: Niederschlag auf die Temperaturachse, 1 degC = 2 mm.
    pt$P_temp <- pt$P_sum / 2
    p_oben <- ggplot(pt, aes(Monat)) +
      geom_ribbon(aes(ymin = pmin(T_mean, P_temp), ymax = pmax(T_mean, P_temp)),
                  fill = "#9ecae1", alpha = 0.6) +
      geom_line(aes(y = T_mean), colour = "#c0392b", linewidth = 0.8) +   # Temp rot
      geom_line(aes(y = P_temp), colour = "#2c5fa8", linewidth = 0.8) +   # Nied. blau
      facet_wrap(~ Zeitlauf, nrow = 1) +
      scale_x_continuous(breaks = 1:12, labels = wl_monlab) +
      scale_y_continuous("Temperatur [\u00b0C]",
        sec.axis = sec_axis(~ . * 2, name = "Niederschlag [mm]")) +
      labs(title = paste0("Walther-Lieth-Vergleich \u00b7 ", id), x = "Monat") +
      theme_minimal()

    # ---- unten: Differenz Zukunft - Referenz (je Zukunftslauf ein Panel) -----
    ref <- pt[pt$Zeitlauf == wl_ref_run, c("Monat", "T_mean", "P_sum")]
    del <- data.frame()
    for (r in futs) {
      cur <- pt[pt$Zeitlauf == r, c("Monat", "T_mean", "P_sum")]
      if (nrow(cur) < 12 || nrow(ref) < 12) next       # unvollstaendig -> weg
      e <- data.frame(Monat = ref$Monat,
                      dT = cur$T_mean - ref$T_mean,
                      dP = cur$P_sum  - ref$P_sum,
                      Zeitlauf = r)
      del <- rbind(del, e)
    }
    if (nrow(del) == 0) { warning(id, " / ", mod, ": kein Delta moeglich.",
                                  call. = FALSE); next }
    del$Zeitlauf <- factor(del$Zeitlauf, levels = futs)
    del$Richtung <- ifelse(del$dP < 0, "trockener", "feuchter")

    p_unten <- ggplot(del, aes(Monat)) +
      geom_col(aes(y = dP / 2, fill = Richtung), alpha = 0.5) +   # dP auf Temp-Achse
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_line(aes(y = dT), colour = "#c0392b", linewidth = 0.8) +
      geom_point(aes(y = dT), colour = "#c0392b", size = 1.3) +
      facet_wrap(~ Zeitlauf, nrow = 1) +
      scale_fill_manual(values = c(feuchter = "#9ecae1", trockener = "#d8b365"),
                        name = NULL) +
      scale_x_continuous(breaks = 1:12, labels = wl_monlab) +
      scale_y_continuous("\u0394Temperatur [\u00b0C]",
        sec.axis = sec_axis(~ . * 2, name = "\u0394Niederschlag [mm]")) +
      labs(title = paste0("\u0394 zur Referenz \u00b7 ", id), x = "Monat") +
      theme_minimal()

    # ---- beide stapeln und speichern ----------------------------------------
    p <- p_oben / p_unten + plot_layout(heights = c(2, 1.4))
    ggsave(file.path(wl_cmp_dir, paste0(id, "_", mod, ".png")),
           p, width = 11, height = 8, dpi = 200)
  }
}
# -> je MASTER_ID und Modell eine PNG <id>_<Modell>.png in
#    04_results/WL_compare/NR-08/  (oben WL-Vergleich, unten Differenz)


## 6.12  Gestaffelter WL-Vergleich je MASTER_ID (ALLE Laeufe ueberlagert) ------
# Ein Panel je gewaehlter MASTER_ID: alle Lauf-CSVs der Region uebereinander.
# Unten das Temperatur-Buendel (rot), oben das Niederschlags-Buendel (blau, als
# P/2 auf der Temperaturachse -> WL-Kopplung 1 degC : 2 mm). Dazu der De-Martonne-
# Index 12*P/(T+10) je Lauf/Monat: Mittel ueber alle Laeufe als gruene
# Mittellinie + min/max-Huelle.
#
# Punkte ueber MID_auswahl waehlen; je Punkt ein eigener ggplot im Loop.
# ggplot hat nur EINE Zweitachse (hier: Niederschlag mm). Der De-Martonne wird
# darum per einfachem Linear-Massstab (a/b) in die Mitte gelegt und mit echten
# Indexwerten als Labels beschriftet - kein dritter Achsen-Strang noetig.

library(ggplot2)

MID_auswahl <- c("NR_130_08_6189", "NR_130_08_66519")   # gewuenschte MASTER_IDs
wl_stf_dir  <- file.path("04_results", "WL_staffel", wl_region)
dir.create(wl_stf_dir, recursive = TRUE, showWarnings = FALSE)

# ALLE Lauf-CSVs der Region (wl_csvs aus 6.10) einlesen und auf die gewaehlten
# IDs stapeln. Spalten je CSV: MASTER_ID | Monat | T_mean | P_sum; Lauf = Stamm.
stf <- data.frame()
for (csv in wl_csvs) {
  run <- sub("\\.csv$", "", basename(csv))
  d   <- as.data.frame(data.table::fread(csv))
  d   <- d[d$MASTER_ID %in% MID_auswahl, c("MASTER_ID", "Monat", "T_mean", "P_sum")]
  if (nrow(d) == 0) next
  d$Zeitlauf <- run
  stf <- rbind(stf, d)
}

stf_monlab <- c("J","F","M","A","M","J","J","A","S","O","N","D")

for (mid in MID_auswahl) {
  pt <- stf[stf$MASTER_ID == mid, ]
  if (nrow(pt) == 0) { warning(mid, ": keine Daten.", call. = FALSE); next }
  pt$P_temp <- pt$P_sum / 2                       # Niederschlag auf die Temp-Achse

  # Szenario-Familie aus dem Laufnamen (OBS_DWD / RCP45 / RCP85) -> Linientyp der
  # Mittelkurven + Beschriftung am rechten Rand (zeigt, wo welches Szenario liegt).
  pt$Familie <- ifelse(grepl("^OBS",   pt$Zeitlauf), "OBS_DWD",
                ifelse(grepl("^RCP45", pt$Zeitlauf), "RCP45",
                ifelse(grepl("^RCP85", pt$Zeitlauf), "RCP85", "andere")))

  # Mittelkurven JE FAMILIE (fett) je Monat, fuer T und P.
  fam     <- aggregate(cbind(T_mean, P_temp) ~ Familie + Monat, pt, mean)
  fam_lab <- fam[fam$Monat == 12, ]               # Labels am Dezember-Ende

  # De-Martonne je Lauf/Monat -> Mittel + min/max ueber alle Laeufe je Monat.
  pt$dm <- 12 * pt$P_sum / (pt$T_mean + 10)
  dm <- aggregate(dm ~ Monat, pt, function(x) c(m = mean(x), lo = min(x), hi = max(x)))
  dm <- data.frame(Monat = dm$Monat, m = dm$dm[, "m"],
                   lo = dm$dm[, "lo"], hi = dm$dm[, "hi"])
  # De-Martonne in die Plot-Mitte legen: Index [0, max] -> Achsen-Band [a, b].
  a <- 8; b <- 34
  to_axis <- function(x) a + (b - a) * x / max(dm$hi)
  dm$m_y <- to_axis(dm$m); dm$lo_y <- to_axis(dm$lo); dm$hi_y <- to_axis(dm$hi)

  p <- ggplot() +
    geom_hline(yintercept = 50, colour = "grey80", linetype = "dashed") +  # WL-Bruchlinie
    # duenne Buendel: rot = Temperatur, blau = Niederschlag (P/2)
    geom_line(data = pt, aes(Monat, T_mean, group = Zeitlauf),
              colour = "#c0392b", alpha = 0.10, linewidth = 0.5) +
    geom_line(data = pt, aes(Monat, P_temp, group = Zeitlauf),
              colour = "#2c5fa8", alpha = 0.10, linewidth = 0.5) +
    # De-Martonne: min/max-Huelle + Mittellinie
    geom_ribbon(data = dm, aes(Monat, ymin = lo_y, ymax = hi_y),
                fill = "#2e7d32", alpha = 0.13) +
    geom_line(data = dm, aes(Monat, m_y), colour = "#2e7d32", linewidth = 1) +
    # Familien-Mittel: Farbe = Variable (rot/blau), Linientyp = Szenario-Familie
    geom_line(data = fam, aes(Monat, T_mean, linetype = Familie),
              colour = "#c0392b", linewidth = 1.1) +
    geom_line(data = fam, aes(Monat, P_temp, linetype = Familie),
              colour = "#2c5fa8", linewidth = 1.1) +
    # rechts daneben: welche Familie wo liegt (am T-Mittel-Ende)
    geom_text(data = fam_lab, aes(Monat, T_mean, label = Familie),
              colour = "#c0392b", hjust = -0.1, size = 2.9) +
    scale_linetype_manual(values = c(OBS_DWD = "solid", RCP45 = "dashed",
                                     RCP85 = "dotted", andere = "12"),
                          name = "Szenario") +
    scale_x_continuous(breaks = 1:12, labels = stf_monlab,
                       expand = expansion(mult = c(0.02, 0.18))) +
    scale_y_continuous("Temperatur [\u00b0C]",
      sec.axis = sec_axis(~ . * 2, name = "Niederschlag [mm]")) +
    coord_cartesian(clip = "off") +
    labs(title = paste0("Gestaffelter WL-Vergleich \u00b7 ", mid),
         subtitle = paste0(length(unique(pt$Zeitlauf)),
           " Klimal\u00e4ufe  \u00b7  rot = Temperatur, blau = Niederschlag  \u00b7  ",
           "gr\u00fcn: De-Martonne 12P/(T+10), Mittel + min/max"),
         x = "Monat") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      axis.title.y.left  = element_text(colour = "#c0392b"),
      axis.text.y.left   = element_text(colour = "#c0392b"),
      axis.title.y.right = element_text(colour = "#2c5fa8"),
      axis.text.y.right  = element_text(colour = "#2c5fa8"),
      legend.position    = "bottom")

  ggsave(file.path(wl_stf_dir, paste0(mid, "_staffel.png")),
         p, width = 10, height = 6.5, dpi = 200)
}
# -> je gewaehlter MASTER_ID eine PNG <MASTER_ID>_staffel.png in
#    04_results/WL_staffel/NR-08/


## 6.13  DEBUG: Temperatur/Niederschlag-Verwechslung pruefen ------------------
# Die NR-CSVs zeigen T_mean = Monatsniederschlag (Mittel = Jahressumme/12) -> in
# die "1155"-Datei wird Niederschlag gelesen. Hier EINE 1155- und EINE 1157-Datei
# DIREKT einlesen und Variable + Wertebereich zeigen. Temperatur muss im Winter
# unter 0 gehen (Min < 0, Max < 30 degC); Niederschlag ist immer positiv (~20-120
# mm). Ist der 1155-Bereich komplett positiv und ~40-60, steckt dort Niederschlag.
tf1 <- nr_1155[1]; pf1 <- nr_1157[1]
t_chk <- nc.1155_function(tf1, assign_global = FALSE)   # message() nennt die Variable!
p_chk <- nc.1157_function(pf1, assign_global = FALSE)
vt <- setdiff(names(t_chk), c("cell_id", "x", "y", "name"))
vp <- setdiff(names(p_chk), c("cell_id", "x", "y", "name"))
cat("1155:", basename(tf1), "-> Range",
    paste(round(range(unlist(t_chk[vt]), na.rm = TRUE), 1), collapse = " .. "), "\n")
cat("1157:", basename(pf1), "-> Range",
    paste(round(range(unlist(p_chk[vp]), na.rm = TRUE), 1), collapse = " .. "), "\n")
# Erwartung: 1155 z.B. "-5 .. 25" (Temp), 1157 z.B. "15 .. 130" (Nied.).
# Steht im 1155-Range etwas wie "30 .. 70", ist die 1155-Datei/Variable falsch
# -> Quelle der NR-1155-Dateien bzw. nc.1155_function(varname=) korrigieren und
#    Abschnitt 6.8 (NR-Monat) neu laufen lassen.


## 6.14  Regions-Mittel-WL je Lauf fuer EINE Region (NR-08) -------------------
# Ein WL-Diagramm je Lauf, gemittelt ueber ALLE MASTER_ID der Region. wl_region_
# diagram() liest die fertige Region/Lauf-CSV (out_base/<Region>/<Lauf>.csv),
# mittelt T_mean/P_sum (und altitude/Lon/Lat) ueber alle MASTER_ID und zeichnet.
# (Ersetzt den verlorenen Aufruf plot_walther_lieth_from_long(wl_csvs, id_val =
#  "NR-08", ...) - der ging nicht: wl_csvs sind Pfade und "NR-08" ist keine ID.)
library(ggplot2)

wl_region  <- "NR-08"
wl_reg_dir <- file.path("04_results", "WL_region", wl_region)
dir.create(wl_reg_dir, recursive = TRUE, showWarnings = FALSE)

wl_csvs <- list.files(file.path(out_base, wl_region), pattern = "\\.csv$",
                      full.names = TRUE)

for (csv in wl_csvs) {
  run <- sub("\\.csv$", "", basename(csv))      # Lauf = Dateistamm
  p <- tryCatch(wl_region_diagram(csv),
                error = function(e) { warning(run, ": ", conditionMessage(e),
                                              call. = FALSE); NULL })
  if (is.null(p)) next
  ggsave(file.path(wl_reg_dir, paste0(wl_region, "_", run, ".png")),
         p, width = 8, height = 6, dpi = 200)
}
# -> je Lauf eine PNG NR-08_<Lauf>.png in 04_results/WL_region/NR-08/
#    (Einzeldiagramme JE MASTER_ID liefert weiterhin Abschnitt 6.10.)


## 6.15  Klima-Wolken-Diagramm (MAT vs. MAP) ueber ALLE BWI-BZE-Punkte --------
# Streudiagramm der Klima-Nische ganz Deutschlands: x = MAT (1049, Jahresmittel-
# temperatur), y = MAP (1050, Jahresniederschlag), ein Punkt je BWI-BZE-Standort
# fuer den Lauf OBS_DWD_1991-2020. Drei Ebenen, von hinten nach vorne:
#   - alle DE-Punkte               -> dunkelgrau
#   - ein Bundesland (BL)           -> hellgrau   (frei waehlbar, hier "MV")
#   - zwei ausgewaehlte MASTER_IDs  -> rot        (frei waehlbar)
#
# BEWUSST FLACH: MAT/MAP kommen fertig aus 6.6 (RDS, dort aus 1049/1050 gemittelt).
# Das Bundesland (BL) steckt bereits in der MASTER_ID (s.u.) - es wird also weder
# eine Tabelle noch ein Shapefile gebraucht. MASTER_ID kommt aus der Boden-Join-
# CSV (clim$id IST id_bwi_bze). Der Plot ist EIN direkter ggplot()-Aufruf.
library(ggplot2)

# -- (a) MAT/MAP je Punkt + Lauf laden (in 6.6 erzeugt und als RDS abgelegt) ---
clim <- readRDS(file.path(out_base, "BWI_Klimadaten_MATMAP.RDS"))
# Spalten: Lon | Lat | altitude | id | id_7004 | Zeitlauf | MAT | MAP
# clim$id == id_bwi_bze -> Schluessel zur Boden-/MASTER_ID-Tabelle.

run_cloud <- "OBS_DWD_1991-2020"
clim <- clim[clim$Zeitlauf == run_cloud &
             is.finite(clim$MAT) & is.finite(clim$MAP), ]   # nur dieser Lauf

# -- (b) MASTER_ID je Punkt anhaengen (Boden-Join-CSV) ------------------------
# id_bwi_bze -> master_id_boden. clim$id IST id_bwi_bze, darum direkt darauf
# joinen (build_bwi_master_lookup liest dieselbe CSV, keyt aber auf cell_id - das
# passt hier nicht zu unserem id).
boden <- read.csv2(file.path(dir_klima, "BWI-BZE_Klima_Boden_Join.csv"),
                   stringsAsFactors = FALSE)
boden_sel <- data.frame(
  id_bwi_bze = as.integer(boden$id_bwi_bze),
  MASTER_ID  = as.character(boden$master_id_boden),
  stringsAsFactors = FALSE)
boden_sel <- boden_sel[grepl("\\S", boden_sel$MASTER_ID), ]    # ohne Boden raus
clim <- dplyr::left_join(clim, boden_sel, by = c("id" = "id_bwi_bze"))

# -- (c) Bundesland (BL) direkt aus der MASTER_ID ableiten --------------------
# Die MASTER_ID traegt den BL-Schluessel als Zahl = amtlicher Laenderschluessel*10:
#   BWI_090_334_4 -> Token "090" = 90  -> 90/10 = 9  -> Bayern
#   BZE_80220     -> "80"        = 80  -> 80/10 = 8  -> Baden-Wuerttemberg
#   NR_130_08_... -> "130"       = 130 -> 130/10= 13 -> Mecklenburg-Vorpommern
# BWI/NR tragen den Code als eigenen "_"-Token; BZE klebt ihn vorn an die Nummer
# -> dort den laengsten gueltigen Prefix nehmen (3- vor 2-stellig). (Einzige
# Rest-Unschaerfe: BZE "10xxx" koennte SL(100) statt SH(10) sein - wenige Punkte.)
valid3 <- c("100","110","120","130","140","150","160")
valid2 <- c("10","20","30","40","50","60","70","80","90")

after <- sub("^[A-Za-z]+_", "", clim$MASTER_ID)         # Prefix BWI_/BZE_/NR_ weg
tok   <- ifelse(grepl("_", after), sub("_.*$", "", after),   # BWI/NR: erster Token
                sub("[^0-9].*$", "", after))                 # BZE: fuehrende Ziffern
p3 <- substr(tok, 1, 3); p2 <- substr(tok, 1, 2)
code <- ifelse(grepl("_", after), suppressWarnings(as.integer(tok)),
               ifelse(p3 %in% valid3, as.integer(p3),
                      ifelse(p2 %in% valid2, as.integer(p2), NA_integer_)))
key  <- code / 10                                       # amtlicher Laenderschluessel

# Schluessel -> Kuerzel (Kodierung wie in der Vorlage: NRW/SA, Stadtstaaten
# zugeschlagen: Hamburg->SH, Bremen->NI, Berlin->BB).
bl_lookup <- c("1"="SH","2"="SH","3"="NI","4"="NI","5"="NRW","6"="HE","7"="RP",
               "8"="BW","9"="BY","10"="SL","11"="BB","12"="BB","13"="MV",
               "14"="SN","15"="SA","16"="TH")
clim$BL <- unname(bl_lookup[as.character(key)])

cloud <- clim[is.finite(clim$MAT) & is.finite(clim$MAP), ]

# -- (d) Auswahl: Bundesland (hellgrau) + zwei MASTER_IDs (rot) ----------------
bl_pick  <- "MV"                                   # beliebiges Bundesland
mid_pick <- c("BZE_80220", "BZE_90850")            # zwei beliebige MASTER_IDs

cloud_bl  <- cloud[!is.na(cloud$BL) & cloud$BL == bl_pick, ]
# je MASTER_ID EIN roter Punkt (mehrere Zellen je MASTER_ID -> Mittel):
cloud_sel <- aggregate(cbind(MAT, MAP) ~ MASTER_ID,
                       cloud[cloud$MASTER_ID %in% mid_pick, ], mean)
if (nrow(cloud_sel) == 0)
  warning("keine der MASTER_IDs (", paste(mid_pick, collapse = ", "),
          ") im Lauf ", run_cloud, ".", call. = FALSE)

# -- (e) Plot: direkter ggplot, drei Punkt-Ebenen von hinten nach vorne --------
p_cloud <- ggplot() +
  geom_point(data = cloud,     aes(MAT, MAP),
             colour = "grey30", size = 0.5, alpha = 0.35) +     # alle DE-Punkte
  geom_point(data = cloud_bl,  aes(MAT, MAP),
             colour = "grey75", size = 0.9, alpha = 0.9) +      # Bundesland
  geom_point(data = cloud_sel, aes(MAT, MAP),
             colour = "white", fill = "#c0392b",
             shape = 21, size = 3, stroke = 0.8) +              # Auswahl rot
  geom_text(data = cloud_sel, aes(MAT, MAP, label = MASTER_ID),
            colour = "#c0392b", size = 3, vjust = -1) +
  labs(title    = "Klima-Wolken-Diagramm \u00b7 MAT vs. MAP (BWI-BZE)",
       subtitle = paste0(run_cloud, "  \u00b7  alle DE (dunkelgrau)  \u00b7  ",
                         bl_pick, " (hellgrau)  \u00b7  Auswahl (rot)"),
       x = "MAT [\u00b0C]",
       y = "MAP [mm]") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

print(p_cloud)

wl_cloud_dir <- file.path("04_results", "WL_cloud")
dir.create(wl_cloud_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(wl_cloud_dir, paste0("MATMAP_", bl_pick, "_", run_cloud, ".png")),
       p_cloud, width = 8, height = 6, dpi = 200)
# -> 04_results/WL_cloud/MATMAP_MV_OBS_DWD_1991-2020.png
#
# Beliebig anderes Bundesland / andere IDs: bl_pick / mid_pick oben aendern.
# Facettierung nach BL (eine Kachel je Bundesland) wie in der Vorlage: einfach
#   + facet_wrap(~ BL) an p_cloud anhaengen (cloud vorher auf !is.na(BL) filtern).


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
