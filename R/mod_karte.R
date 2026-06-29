# R/mod_karte.R ----
# Geodaten, Klimalauf-Parsing, Farbpaletten, BAE-Stufenzuordnung.
# Setzt voraus: LOCAL_DIR, result_dir, geo_dir aus config.R

# ---- 0. Globale Variablen (Vorbelegung) ----
# Globale Variablen vorbelegen – werden durch init_karte_daten() befuellt.
# Verhindert "object not found"-Fehler falls init noch nicht gelaufen.
if (!exists("BWI_GEO"))          BWI_GEO          <- NULL
if (!exists("DE_GRENZE"))        DE_GRENZE         <- NULL
if (!exists("klima_meta"))       klima_meta        <- NULL
if (!exists("csv_files"))        csv_files         <- NULL
if (!exists("szenario_choices")) szenario_choices  <- character(0)
if (!exists("modell_choices"))   modell_choices    <- character(0)
if (!exists("zeitraum_choices")) zeitraum_choices  <- character(0)
if (!exists("baumart_choices"))  baumart_choices   <- character(0)
if (!exists("tv_choices"))       tv_choices        <- character(0)
if (!exists("tv_bezeichnung"))   tv_bezeichnung    <- character(0)
if (!exists("NR_GEO_ALL"))       NR_GEO_ALL        <- NULL
if (!exists("NR_UMKREIS_ALL"))   NR_UMKREIS_ALL    <- NULL
if (!exists("nr_choices"))       nr_choices        <- character(0)

# ---- 1. Klimalauf-Parsing ----
# Schema: BAE_{Szenario}_{Modell}_{Zeitraum}.csv

parse_klimalauf <- function(files) {
  bn       <- sub("^BAE_", "", sub("\\.csv$", "", basename(files)))
  zeitraum <- stringr::str_extract(bn, "\\d{4}-\\d{4}")
  rest     <- stringr::str_remove(bn, "_?\\d{4}-\\d{4}$")
  parts    <- stringr::str_split_fixed(rest, "_", n = 2)
  szenario <- parts[, 1]
  modell   <- ifelse(nchar(parts[, 2]) == 0, "(OBS)", parts[, 2])
  data.frame(file     = files,
             Szenario = szenario,
             Modell   = modell,
             Zeitraum = zeitraum,
             stringsAsFactors = FALSE)
}

# ---- 2. Farbpaletten ----

kat_palette <- c(
  "sehr empfohlen"        = "#1A9850",
  "empfohlen"             = "#A6D96A",
  "m\u00e4\u00dfig empfohlen" = "#FEE08B",
  "wenig empfohlen"       = "#FDAE61",
  "nicht empfohlen"       = "#A50026",
  "pBv"                   = "#404040",
  "Keine Datengrundlage"  = "#B0B0B0"
)

baumart_farben_basis <- c(
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
  "#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf",
  "#aec7e8","#ffbb78"
)

# ---- 3. BAE-Stufenzuordnung ----

map_stufe <- function(val, st) {
  maps <- list(
    "BAE_3ST" = c("1"="sehr empfohlen", "2"="m\u00e4\u00dfig empfohlen",
                  "3"="nicht empfohlen"),
    "BAE_4ST" = c("1"="sehr empfohlen", "2"="empfohlen",
                  "3"="m\u00e4\u00dfig empfohlen", "4"="nicht empfohlen"),
    "BAE_5ST" = c("1"="sehr empfohlen", "2"="empfohlen",
                  "3"="m\u00e4\u00dfig empfohlen", "4"="wenig empfohlen",
                  "5"="nicht empfohlen"),
    "BAE_7ST" = c("1"="sehr empfohlen", "2"="sehr empfohlen",
                  "3"="empfohlen",       "4"="m\u00e4\u00dfig empfohlen",
                  "5"="wenig empfohlen", "6"="nicht empfohlen",
                  "7"="nicht empfohlen")
  )
  m <- maps[[toupper(st)]]
  if (is.null(m)) return(rep("Keine Datengrundlage", length(val)))
  v <- as.character(val)
  dplyr::case_when(
    v %in% names(m) ~ unname(m[v]),
    v == "pBv"      ~ "pBv",
    TRUE            ~ "Keine Datengrundlage"
  )
}

# ---- 4. Geodaten laden (init_karte_daten) ----

# Spaltennamen eines sf-Objekts auf Grossbuchstaben umstellen, ohne die
# sf_column-Zuordnung zu verlieren (sonst "internal error: can't find sf
# column" bei dplyr::select/rename auf dem Ergebnis).
uppercase_sf_names <- function(sf_raw) {
  geom_col <- attr(sf_raw, "sf_column")
  names(sf_raw) <- toupper(names(sf_raw))
  attr(sf_raw, "sf_column") <- toupper(geom_col)
  sf_raw
}

init_karte_daten <- function(geo_dir    = NULL,
                             result_dir = NULL,
                             local_dir  = NULL) {
  # Fallbacks auf globale Variablen wenn keine Parameter uebergeben
  geo_dir    <- geo_dir    %||% get0("geo_dir",    envir = .GlobalEnv)
  result_dir <- result_dir %||% get0("result_dir", envir = .GlobalEnv)
  local_dir  <- local_dir  %||% get0("LOCAL_DIR",  envir = .GlobalEnv) %||% "../../"
  
  if (is.null(geo_dir) || is.null(result_dir))
    stop("init_karte_daten: geo_dir und result_dir muessen angegeben werden.")
  
  ## ---- 4.1 BWI-Geodaten ----
  bwi_csv <- file.path(geo_dir, "BWI-BZE_Klima_Boden_Join.csv")
  BWI_BZE_GEO <<- data.table::fread(bwi_csv)
  BWI_GEO <<- sf::st_as_sf(BWI_BZE_GEO,
                           coords = c("x_25832","y_25832"),
                           crs    = "EPSG:25832") %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::select(MASTER_ID = master_id_boden, id_bwi_bze)
  
  ## ---- 4.2 Deutschland-Grenze (ggplot-Export) ----
  de_shp    <- list.files(file.path(geo_dir, "DE_L\u00e4ndergrenzen"),
                          pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  DE_GRENZE <<- sf::st_read(de_shp[1], quiet = TRUE) %>% sf::st_transform(crs = 4326)
  
  ## ---- 4.3 BWI-CSV-Metadaten ----
  csv_files        <<- list.files(file.path(result_dir, "BAE/BWI"),
                                  pattern = "\\.csv$", full.names = TRUE)
  klima_meta       <<- parse_klimalauf(csv_files)
  szenario_choices <<- sort(unique(klima_meta$Szenario))
  modell_choices   <<- sort(unique(klima_meta$Modell))
  zeitraum_choices <<- sort(unique(klima_meta$Zeitraum))
  
  sample_df       <- data.table::fread(csv_files[1])
  baumart_choices <<- sort(unique(sample_df$Baumart))
  
  tv_bezeichnung <<- c(
    "TV1: NW-FVA"   = "1", "TV2: LFOA-MV"  = "2",
    "TV3: LFE-BB"   = "3", "TV4: LWF-BY"   = "4",
    "TV5: FVA-BW"   = "5", "TV6: FAWF-RLP" = "6",
    "TV7: FFK-SN"   = "7", "TV8: WH-NRW"   = "8",
    "TV9: FFK-TH"   = "9"
  )
  tv_in_daten  <- sort(unique(as.character(sample_df$TV)))
  tv_choices  <<- tv_bezeichnung[tv_bezeichnung %in% tv_in_daten] #1:9
  
  message("BWI_GEO: ", nrow(BWI_GEO), " | CSV: ", nrow(klima_meta),
          " | Szenarien: ", paste(szenario_choices, collapse = ", "))
  
  ## ---- 4.4 NR-Geodaten (GEO_NR.shp) ----
  # Zwei Shapefiles:
  #  - GEO_NR.shp: ein Polygon je MASTER_ID (Bodendatenbank/NR/Geodaten),
  #    Basis fuer NR_GEO_ALL (Kartenpunkte + MASTER_ID-Join zu Ergebnisse_BAE/BZT)
  #  - 11_nachbarschaftsregionen_polygon_radius_25km.shp: 25-km-Umkreis je
  #    Nachbarschaftsregion (Spalte "id" = 1..11), Geodaten/Nachbarschaftsregionen.
  #    Dient als Kartenvorschau ("Umkreis") UND - falls GEO_NR.shp keine eigene
  #    NR-Spalte hat - als Spatial-Join-Grundlage fuer NR_ID.
  
  nr_geo_dir <- file.path(local_dir,
                          "01_data/Grundlagen/Bodendatenbank/NR/Geodaten")
  NR_GEO_ALL <<- tryCatch({
    shp <- list.files(nr_geo_dir, pattern = "\\.shp$",
                      full.names = TRUE, recursive = TRUE)
    if (length(shp) == 0) stop("Kein NR-Shapefile gefunden in ", nr_geo_dir)
    geo_nr_idx <- grep("^GEO_NR\\.shp$", basename(shp), ignore.case = TRUE)
    shp_file   <- if (length(geo_nr_idx) > 0) shp[geo_nr_idx[1]] else shp[1]
    message("NR-Geodaten-Datei: ", shp_file)
    sf_raw <- sf::st_read(shp_file, quiet = TRUE)
    # Defekte Geometrien (z.B. doppelte Vertices) VOR dem Transform nach 4326
    # reparieren - im projizierten Quell-CRS arbeitet GEOS planar und tolerant,
    # waehrend s2 auf 4326 bei degenerierten Kanten hart abbricht.
    n_invalid <- sum(!sf::st_is_valid(sf_raw))
    if (n_invalid > 0) {
      message("  ", n_invalid, " ungueltige Geometrie(n) repariert (st_make_valid).")
      sf_raw <- sf::st_make_valid(sf_raw)
      gt <- as.character(sf::st_geometry_type(sf_raw))
      if (any(grepl("POLYGON", gt))) {
        if (any(gt == "GEOMETRYCOLLECTION"))
          sf_raw <- suppressWarnings(sf::st_collection_extract(sf_raw, "POLYGON"))
        gt   <- as.character(sf::st_geometry_type(sf_raw))
        drop <- !grepl("POLYGON", gt)
        if (any(drop)) {
          message("  ", sum(drop),
                  " nicht-polygonale Reste nach st_make_valid verworfen.")
          sf_raw <- sf_raw[!drop, ]
        }
        sf_raw <- sf::st_cast(sf_raw, "MULTIPOLYGON", warn = FALSE)
      }
    }
    sf_raw <- sf::st_transform(sf_raw, crs = 4326)
    sf_raw <- uppercase_sf_names(sf_raw)
    id_col <- grep("^MASTER_ID$|^ID$", names(sf_raw), value = TRUE)[1]
    if (!is.na(id_col) && id_col != "MASTER_ID")
      sf_raw <- dplyr::rename(sf_raw, MASTER_ID = dplyr::all_of(id_col))
    xcol <- grep("X_?CENT|^X$|^LON", names(sf_raw), value = TRUE)[1]
    ycol <- grep("Y_?CENT|^Y$|^LAT", names(sf_raw), value = TRUE)[1]
    if (!is.na(xcol) && xcol != "X_CENTROID")
      sf_raw <- dplyr::rename(sf_raw, X_CENTROID = dplyr::all_of(xcol))
    if (!is.na(ycol) && ycol != "Y_CENTROID")
      sf_raw <- dplyr::rename(sf_raw, Y_CENTROID = dplyr::all_of(ycol))
    message("NR-Shapefile-Spalten: ", paste(names(sf_raw), collapse = ", "))
    sf_raw
  }, error = function(e) {
    message("NR-Geodaten nicht geladen: ", e$message); NULL })
  
  
  ## ---- 4.5 NR-Umkreise (25-km-Radius) ----
  nr_umkreis_dir <- file.path(local_dir,
                              "01_data/Grundlagen/Geodaten/Nachbarschaftsregionen")
  NR_UMKREIS_ALL <<- tryCatch({
    shp <- list.files(nr_umkreis_dir, pattern = "\\.shp$",
                      full.names = TRUE, recursive = TRUE)
    if (length(shp) == 0) stop("Kein Umkreis-Shapefile gefunden in ", nr_umkreis_dir)
    # Ordner enthaelt zwei Shapefiles: ..._centroid.shp (POINT) und
    # ..._polygon_radius_25km.shp (POLYGON). shp[1] waere alphabetisch der
    # Centroid -> addPolygons() stuerzt ueber to_ring.default (POINT) ab.
    # Daher gezielt das Polygon-Shapefile waehlen.
    poly_idx <- grep("polygon", basename(shp), ignore.case = TRUE)
    shp_file <- if (length(poly_idx) > 0) shp[poly_idx[1]] else shp[1]
    message("NR-Umkreis-Datei: ", shp_file)
    sf_raw <- sf::st_read(shp_file, quiet = TRUE)
    n_invalid <- sum(!sf::st_is_valid(sf_raw))
    if (n_invalid > 0) {
      message("  ", n_invalid, " ungueltige Umkreis-Geometrie(n) repariert (st_make_valid).")
      sf_raw <- sf::st_make_valid(sf_raw)
    }
    sf_raw <- sf::st_transform(sf_raw, crs = 4326)
    sf_raw <- uppercase_sf_names(sf_raw)
    id_col <- grep("^ID$", names(sf_raw), value = TRUE)[1]
    if (is.na(id_col))
      stop("Spalte 'id' nicht im Umkreis-Shapefile gefunden (Spalten: ",
           paste(names(sf_raw), collapse = ", "), ")")
    sf_raw$NR_ID <- sprintf("NR%02d", as.integer(sf_raw[[id_col]]))
    sf_raw <- dplyr::select(sf_raw, NR_ID)
    message("NR-Umkreise geladen: ", paste(sort(sf_raw$NR_ID), collapse = ", "))
    sf_raw
  }, error = function(e) {
    message("NR-Umkreise nicht geladen: ", e$message); NULL })
  
  ## ---- 4.6 NR_ID je GEO_NR-Polygon (aus MASTER_ID 8-9) ----
  # ableiten (bestaetigte Regel: paste0("NR-", substring(MASTER_ID, 8, 9))).
  # Intern als "NR08"-Token gefuehrt (passt zum Selector nr_choices);
  # der Bindestrich-Token "NR-08" wird erst beim Dateizugriff gebildet.
  if (!is.null(NR_GEO_ALL)) {
    nr_num <- suppressWarnings(
      as.integer(substring(as.character(NR_GEO_ALL$MASTER_ID), 8, 9)))
    NR_GEO_ALL$NR_ID <<- ifelse(is.na(nr_num), NA_character_,
                                sprintf("NR%02d", nr_num))
    n_ohne <- sum(is.na(NR_GEO_ALL$NR_ID))
    if (n_ohne > 0)
      message("  ", n_ohne, " von ", nrow(NR_GEO_ALL),
              " Polygonen ohne ableitbare NR_ID (MASTER_ID-Stellen 8-9 leer).")
    nr_tab <- table(NR_GEO_ALL$NR_ID)
    message("NR-Verteilung (aus MASTER_ID 8-9): ",
            paste(names(nr_tab), as.integer(nr_tab),
                  sep = "=", collapse = ", "))
  }
  
  nr_choices <<- paste0("NR", sprintf("%02d", 1:11))
  message("NR-Geodaten: ",
          if (!is.null(NR_GEO_ALL)) nrow(NR_GEO_ALL) else "nicht geladen",
          " | NR-Umkreise: ",
          if (!is.null(NR_UMKREIS_ALL)) nrow(NR_UMKREIS_ALL) else "nicht geladen")
}