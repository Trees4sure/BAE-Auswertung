# =============================================================================
# Klima-Wolken-Diagramm (MAP vs. MAT) als parametrisierte Funktion
#
# Cloud_diagram_function() zeichnet die deutschlandweite Klima-Punktwolke fuer
# EINEN Referenzlauf (alle DE-Punkte dunkelgrau, ein Bundesland hellgrau) und
# hebt die ausgewaehlte MASTER_ID-Station hervor (farbiger Punkt = "Kaliss" o.ae.).
# Optional ein/mehrere Vergleichslauf(e): die erwartete Stationslage im Vergleichs-
# lauf wird gezeichnet und mit einer gestrichelten Linie von der Referenz-Lage
# verbunden -> "Mittelpunktverschiebung". Abschaltbar mit show_shift = FALSE.
#
# Achsen: x = MAP (Niederschlag), y = MAT (Temperatur). Subtitle = Delta der
# Station (Vergleich - Referenz). Legenden-/Text-Labels ohne Modellname. Unten
# links die Referenz-Mittelwerte (DE, Bundesland, Station) als Zahlen; die
# Vergleichswerte stehen farbig rechts neben ihren Punkten.
#
# MAT/MAP kommen aus 6.6 (BWI_Klimadaten_MATMAP.RDS, dort aus 1049/1050 gemittelt).
# BL ueber den NUTS1-Lookup (id -> NUTS_NAME), MASTER_ID aus der Boden-Join-CSV;
# beide sind je Punkt (id) lauf-unabhaengig -> EINMAL anreichern + Cache-RDS.
#
# Pakete: dplyr, ggplot2. sf nur fuer das einmalige Erzeugen von nuts_id.RDS
# (s. Block am Dateiende), nicht im Normalbetrieb.
# =============================================================================


#' @param Klimalauf.choose  Referenzlauf (Wolke), Default OBS_DWD_1991-2020.
#' @param Klimalauf_compare Optionale(r) Vergleichslauf/-laeufe (Vektor; NULL = keiner).
#' @param MASTER_ID.choose  Eine (oder mehrere) MASTER_ID(s) fuer die Station.
#' @param BL_choose         Bundesland-Kuerzel fuer die hellgraue Ebene.
#' @param New_label         Label der Station in Legende/Text (NULL = "Station").
#' @param show_shift        TRUE = Verschiebungs-Linie Referenz->Vergleich zeichnen.
#' @param point_size        Groesse der Stations-Punkte.
#' @param cloud_size        Groesse der Wolken-Punkte (alle DE / Bundesland).
#' @param rds_path,nuts_rds,boden_csv,cache_rds,rebuild_cache,save_dir  s. Kommentare.
#' @return ggplot-Objekt.
Cloud_diagram_function <- function(
    Klimalauf.choose  = "OBS_DWD_1991-2020",
    Klimalauf_compare = NULL,
    MASTER_ID.choose  = "BWI_130_36567_4",
    BL_choose         = "MV",
    New_label         = NULL,
    show_shift        = TRUE,
    point_size        = 5,
    cloud_size        = 0.8,
    rds_path   = file.path(out_base, "BWI_Klimadaten_MATMAP.RDS"),
    nuts_rds   = file.path("01_data", "Grundlagen/Geodaten",
                           "DE_L\u00e4ndergrenzen/nuts250_1231", "nuts_id.RDS"),
    boden_csv  = file.path(dir_klima, "BWI-BZE_Klima_Boden_Join.csv"),
    cache_rds  = file.path(out_base, "BWI_Klimadaten_MATMAP_BL_MID.RDS"),
    rebuild_cache = FALSE,
    save_dir   = NULL) {

  library(ggplot2)
  library(dplyr)

  # ==========================================================================
  # (a) Daten laden + EINMAL mit BL (NUTS1) und MASTER_ID anreichern (Cache)
  # ==========================================================================
  if (!rebuild_cache && !is.null(cache_rds) && file.exists(cache_rds)) {
    clim <- readRDS(cache_rds)                       # schon angereichert
  } else {
    clim <- readRDS(rds_path)        # Lon|Lat|altitude|id|id_7004|Zeitlauf|MAT|MAP

    # BL ueber den NUTS1-Lookup (id -> NUTS_NAME); lauf-unabhaengig -> auf id joinen.
    if (!file.exists(nuts_rds))
      stop("nuts_rds nicht gefunden: '", nuts_rds, "'. Einmal mit dem Block am ",
           "Dateiende von cloud_diagram.R erzeugen.")
    id_nuts <- readRDS(nuts_rds)
    clim <- left_join(clim, id_nuts[, c("id", "NUTS_NAME")], by = "id")

    # NUTS_NAME -> BL (NRW/SA; Stadtstaaten zugeschlagen: Berlin->BB, Bremen->NI,
    # Hamburg->SH).
    clim$BL <- case_when(
      clim$NUTS_NAME == "Bayern"                 ~ "BY",
      clim$NUTS_NAME == "Baden-W\u00fcrttemberg" ~ "BW",
      clim$NUTS_NAME == "Rheinland-Pfalz"        ~ "RP",
      clim$NUTS_NAME == "Hessen"                 ~ "HE",
      clim$NUTS_NAME == "Nordrhein-Westfalen"    ~ "NRW",
      clim$NUTS_NAME == "Th\u00fcringen"         ~ "TH",
      clim$NUTS_NAME == "Niedersachsen"          ~ "NI",
      clim$NUTS_NAME == "Sachsen-Anhalt"         ~ "SA",
      clim$NUTS_NAME == "Berlin"                 ~ "BB",
      clim$NUTS_NAME == "Brandenburg"            ~ "BB",
      clim$NUTS_NAME == "Bremen"                 ~ "NI",
      clim$NUTS_NAME == "Hamburg"                ~ "SH",
      clim$NUTS_NAME == "Schleswig-Holstein"     ~ "SH",
      clim$NUTS_NAME == "Mecklenburg-Vorpommern" ~ "MV",
      clim$NUTS_NAME == "Sachsen"                ~ "SN",
      clim$NUTS_NAME == "Saarland"               ~ "SL",
      TRUE                                       ~ NA_character_)

    # MASTER_ID aus der Boden-Join-CSV (id_bwi_bze -> master_id_boden), auf id.
    boden     <- utils::read.csv2(boden_csv, stringsAsFactors = FALSE)
    boden_sel <- data.frame(id        = as.integer(boden$id_bwi_bze),
                            MASTER_ID = as.character(boden$master_id_boden))
    boden_sel <- boden_sel[grepl("\\S", boden_sel$MASTER_ID), ]
    clim <- left_join(clim, boden_sel, by = "id")

    if (!is.null(cache_rds)) saveRDS(clim, cache_rds)
  }

  clim <- clim[is.finite(clim$MAT) & is.finite(clim$MAP), ]   # nur gueltige Werte

  # ==========================================================================
  # (b) Kurznamen + Wolken-Tabellen (Referenzlauf)
  # ==========================================================================
  ref      <- Klimalauf.choose
  cmp_runs <- if (is.null(Klimalauf_compare)) character(0) else as.character(Klimalauf_compare)
  runs     <- c(ref, cmp_runs)
  typ_stat <- if (!is.null(New_label)) New_label else "Station"
  lab_de   <- "DE"
  lab_bl   <- BL_choose

  cloud    <- clim[clim$Zeitlauf == ref, ]                      # alle DE (Referenzlauf)
  cloud_bl <- cloud[!is.na(cloud$BL) & cloud$BL == BL_choose, ] # ein Bundesland
  if (nrow(cloud) == 0)    stop("Referenzlauf '", ref, "' nicht in den Daten.")
  if (nrow(cloud_bl) == 0) warning("Keine Punkte fuer Bundesland '", BL_choose, "'.", call. = FALSE)

  # Kurze Lauf-Namen ohne Modell, z.B. "RCP85_MPICLM_2071-2100" -> "RCP85: 2071-2100"
  run_kurz <- ifelse(grepl("[0-9]{4}-[0-9]{4}", runs),
                     paste0(sub("_.*$", "", runs), ": ",
                            sub(".*([0-9]{4}-[0-9]{4}).*", "\\1", runs)), runs)
  names(run_kurz) <- runs

  # ==========================================================================
  # (c) Stationsmittel je Lauf (NUR die Station - kein DE-Mittel-Punkt)
  #     Legenden-Klasse: Referenz = Stationsname ("Kaliss"), Vergleich = Laufname.
  # ==========================================================================
  st <- clim %>%
    filter(Zeitlauf %in% runs, MASTER_ID %in% MASTER_ID.choose) %>%
    group_by(Lauf = Zeitlauf) %>%
    summarise(MAP = mean(MAP), MAT = mean(MAT), .groups = "drop")
  if (nrow(st) == 0)
    stop("MASTER_ID '", paste(MASTER_ID.choose, collapse = ", "), "' nicht gefunden.")
  st    <- st[order(match(st$Lauf, runs)), ]                    # Referenz zuerst
  st$Klasse <- ifelse(st$Lauf == ref, typ_stat, run_kurz[st$Lauf])

  # ==========================================================================
  # (d) Verschiebungs-Pfeil: Referenz-Station -> Vergleichs-Station
  # ==========================================================================
  st_ref <- st[st$Lauf == ref, ]
  segs   <- data.frame(MAP = numeric(), MAT = numeric(),
                       MAP_ref = numeric(), MAT_ref = numeric(), Klasse = character())
  if (show_shift && nrow(st_ref) == 1) {
    sc <- st[st$Lauf != ref, ]
    if (nrow(sc) > 0)
      segs <- data.frame(MAP = sc$MAP, MAT = sc$MAT,
                         MAP_ref = st_ref$MAP, MAT_ref = st_ref$MAT, Klasse = sc$Klasse)
  }

  # ==========================================================================
  # (e) Farben (eine je Klasse) + Reihenfolge der Legende
  # ==========================================================================
  run_pal <- c("#2166ac", "#b2182b", "#e08214", "#1b7837", "#762a83", "#5e3c99")
  farben  <- c(setNames("grey30", lab_de),
               setNames("grey75", lab_bl),
               setNames(run_pal[seq_len(nrow(st))], st$Klasse))
  klassen <- names(farben)                                      # DE, MV, Kaliss, <cmp>...

  st$Klasse  <- factor(st$Klasse,  levels = klassen)
  if (nrow(segs) > 0) segs$Klasse <- factor(segs$Klasse, levels = klassen)

  # ==========================================================================
  # (f) Text-Block unten links: NUR DE, Bundesland, Station (Referenz).
  #     Die Vergleichslauf-Werte stehen farbig RECHTS neben ihren Punkten (s. (g)).
  # ==========================================================================
  txt_de  <- sprintf("DE: %.1f \u00b0C, %d mm", mean(cloud$MAT), round(mean(cloud$MAP)))
  txt_bl  <- sprintf("%s: %.1f \u00b0C, %d mm", lab_bl, mean(cloud_bl$MAT), round(mean(cloud_bl$MAP)))
  txt_ref <- if (nrow(st_ref) == 1) sprintf("%s: %.1f \u00b0C, %d mm", typ_stat, st_ref$MAT, round(st_ref$MAP))
  mean_txt <- paste(c(txt_de, txt_bl, txt_ref), collapse = "\n")

  # Subtitle = Delta der STATION (Vergleich - Referenz), aus den Stationsmitteln (st)
  sub_txt <- NULL
  if (length(cmp_runs) > 0 && nrow(st_ref) == 1) {
    d <- st[match(cmp_runs, st$Lauf), ]
    sub_txt <- paste0("\u0394 ", typ_stat, ": ", paste(sprintf(
      "%s: %+.1f \u00b0C, %+d mm", run_kurz[cmp_runs], d$MAT - st_ref$MAT, round(d$MAP - st_ref$MAP)),
      collapse = "   |   "))
  }

  # ==========================================================================
  # (g) Plot: ein durchgehender ggplot()-Aufruf, jede Ebene eine Zeile
  # ==========================================================================
  p <- ggplot() +
    geom_point(data = cloud,    aes(MAP, MAT, colour = lab_de), size = cloud_size) +
    geom_point(data = cloud_bl, aes(MAP, MAT, colour = lab_bl), size = cloud_size) +
    geom_segment(data = segs, aes(MAP_ref, MAT_ref, xend = MAP, yend = MAT, colour = Klasse),
                 linetype = "dashed", linewidth = 0.7) +              # ohne Pfeilspitze
    geom_point(data = st, aes(MAP, MAT, colour = Klasse), size = point_size) +
    geom_text(data = st[st$Lauf != ref, ],                           # Vergleichswerte rechts neben den Punkten
              aes(MAP, MAT, colour = Klasse,
                  label = sprintf("%.1f \u00b0C, %d mm", MAT, round(MAP))),
              hjust = -0.15, size = 4, fontface = "bold", show.legend = FALSE) +
    annotate("text", x = -Inf, y = -Inf, label = mean_txt,
             colour = "black", size = 4, hjust = 0, vjust = -0.5) +
    scale_colour_manual(name = NULL, values = farben, breaks = klassen, limits = klassen) +
    guides(colour = guide_legend(override.aes = list(size = 4, linetype = 0))) +
    coord_cartesian(clip = "off") +
    labs(title    = "Klimaraum: Jahresniederschlag (MAP) vs. Jahresmitteltemperatur (MAT)",
         subtitle = sub_txt,
         x        = "Jahresniederschlag MAP [mm]",
         y        = "Jahresmitteltemperatur MAT [\u00b0C]") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())

  # ==========================================================================
  # (h) optional speichern
  # ==========================================================================
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    # Kompakter Dateiname (<= 25 Zeichen inkl. .png): cloud_<Station-kurz>_<Szenarien>
    # z.B. "cloud_BWI_130*_4585.png" (Station auf 1.+2. Token gekuerzt, Vergleichs-
    # szenarien nur als Ziffern: RCP45/RCP85 -> "4585").
    mid_kurz <- sub("^([A-Za-z]+_[0-9]+).*$", "\\1", MASTER_ID.choose[1])
    if (mid_kurz != MASTER_ID.choose[1]) mid_kurz <- paste0(mid_kurz, "*")
    scen     <- gsub("\\D", "", sub("_.*$", "", cmp_runs))           # "45","85"
    cmp_kurz <- if (length(cmp_runs)) paste0("_", paste(scen, collapse = "")) else ""
    stem     <- substr(paste0("cloud_", mid_kurz, cmp_kurz), 1, 20)  # harte Kappung
    ggsave(file.path(save_dir, paste0(stem, ".png")), p, width = 8, height = 6, dpi = 200)
  }

  p
}


# =============================================================================
# EINMALIG: NUTS1-Lookup id -> NUTS_NAME erzeugen (braucht sf)
# Danach liest Cloud_diagram_function() nur noch nuts_id.RDS.
# =============================================================================
# library(sf)
# clim <- readRDS(file.path(out_base, "BWI_Klimadaten_MATMAP.RDS"))
# clim <- clim[clim$Zeitlauf == "OBS_DWD_1991-2020", ]   # ein Lauf deckt alle id ab
#
# nuts1_shp <- file.path("01_data", "Grundlagen/Geodaten",
#                        "DE_L\u00e4ndergrenzen/nuts250_1231", "250_NUTS1.shp")
# nuts1 <- sf::st_transform(sf::st_read(nuts1_shp, quiet = TRUE), 25832)
#
# clim_sf <- sf::st_as_sf(clim, coords = c("Lon", "Lat"), crs = 25832, remove = FALSE)
# clim_sf <- sf::st_join(clim_sf, nuts1["NUTS_NAME"], join = sf::st_within)
# id_nuts <- unique(sf::st_drop_geometry(clim_sf)[, c("id", "NUTS_NAME")])
# saveRDS(id_nuts, file.path("01_data", "Grundlagen/Geodaten",
#                            "DE_L\u00e4ndergrenzen/nuts250_1231", "nuts_id.RDS"))
