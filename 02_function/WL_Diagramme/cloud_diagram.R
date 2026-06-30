# =============================================================================
# Klima-Wolken-Diagramm (MAP vs. MAT) als parametrisierte Funktion
#
# Cloud_diagram_function() zeichnet die deutschlandweite Klima-Punktwolke fuer
# EINEN Referenzlauf (alle DE-Punkte dunkelgrau, ein Bundesland hellgrau) und
# darueber die Klima-MITTELPUNKTE je Lauf:
#   - DE-Mittel (Dreieck) und die MASTER_ID-Station (Kreis), je Lauf eine Farbe.
# Optional ein/mehrere Vergleichslauf(e): deren Mittelpunkte werden gezeichnet und
# mit einem Pfeil von der Referenz-Lage verbunden -> "Mittelpunktverschiebung".
# Der Pfeil laesst sich mit show_shift = FALSE abschalten.
#
# Achsen: x = MAP (Niederschlag), y = MAT (Temperatur). Subtitle = Delta der
# DE-Mittelwerte (Vergleich - Referenz). Legenden-Labels ohne Modellname.
# Oben links steht zusaetzlich ein Text-Block mit den Mittelwerten (DE,
# Bundesland, je Lauf/Typ) als Zahlen.
#
# MAT/MAP kommen aus 6.6 (BWI_Klimadaten_MATMAP.RDS, dort aus 1049/1050 gemittelt).
# BL ueber den NUTS1-Lookup (id -> NUTS_NAME), MASTER_ID aus der Boden-Join-CSV;
# beide sind je Punkt (id) lauf-unabhaengig -> EINMAL anreichern + Cache-RDS.
#
# Pakete: dplyr, ggplot2 (grid fuer den Pfeil). sf nur fuer das einmalige
# Erzeugen von nuts_id.RDS (s. Block am Dateiende), nicht im Normalbetrieb.
# =============================================================================


#' @param Klimalauf.choose  Referenzlauf (Wolke), Default OBS_DWD_1991-2020.
#' @param Klimalauf_compare Optionale(r) Vergleichslauf/-laeufe (Vektor; NULL = keiner).
#' @param MASTER_ID.choose  Eine (oder mehrere) MASTER_ID(s) fuer die Station.
#' @param BL_choose         Bundesland-Kuerzel fuer die hellgraue Ebene.
#' @param New_label         Label fuer die Station in der Form-Legende (NULL = "Station").
#' @param show_shift        TRUE = Verschiebungs-Pfeile Referenz->Vergleich zeichnen.
#' @param point_size        Groesse der Mittelpunkt-Symbole (Dreieck/Kreis).
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

  # Ab hier nur gueltige Klimawerte.
  clim <- clim[is.finite(clim$MAT) & is.finite(clim$MAP), ]

  # ==========================================================================
  # (b) Ein paar Kurznamen + die drei Wolken-Tabellen direkt (gut anschaubar)
  # ==========================================================================
  ref      <- Klimalauf.choose
  runs     <- c(ref, as.character(Klimalauf_compare))   # NULL faellt einfach weg
  typ_stat <- if (!is.null(New_label)) New_label else "Station"
  lab_de   <- "alle DE-Punkte"
  lab_bl   <- paste0("Bundesland ", BL_choose)

  cloud    <- clim[clim$Zeitlauf == ref, ]                       # alle DE (Referenzlauf)
  cloud_bl <- cloud[!is.na(cloud$BL) & cloud$BL == BL_choose, ]  # ein Bundesland
  if (nrow(cloud) == 0)    stop("Referenzlauf '", ref, "' nicht in den Daten.")
  if (nrow(cloud_bl) == 0) warning("Keine Punkte fuer Bundesland '", BL_choose, "'.", call. = FALSE)

  # ==========================================================================
  # (c) Mittelpunkte je Lauf: DE-Mittel + Stationsmittel (dplyr statt Schleife)
  # ==========================================================================
  runs_df <- filter(clim, Zeitlauf %in% runs)

  mids_de <- runs_df %>%
    group_by(Lauf = Zeitlauf) %>%
    summarise(MAP = mean(MAP), MAT = mean(MAT), .groups = "drop") %>%
    mutate(Typ = "DE-Mittel")

  mids_st <- runs_df %>%
    filter(MASTER_ID %in% MASTER_ID.choose) %>%
    group_by(Lauf = Zeitlauf) %>%
    summarise(MAP = mean(MAP), MAT = mean(MAT), .groups = "drop") %>%
    mutate(Typ = typ_stat)

  mids <- bind_rows(mids_de, mids_st)

  # ==========================================================================
  # (d) Verschiebungs-Pfeile: Referenz-Mittel -> Vergleichs-Mittel (ein join)
  # ==========================================================================
  ref_mids <- mids %>% filter(Lauf == ref)  %>% select(Typ, MAP_ref = MAP, MAT_ref = MAT)
  segs     <- mids %>% filter(Lauf != ref)  %>% left_join(ref_mids, by = "Typ")
  if (!show_shift) segs <- segs[0, ]         # leeres df -> geom_segment zeichnet nichts

  # ==========================================================================
  # (e) Kurze Lauf-Namen (z.B. "RCP85: 2071-2100") + eine Farbe je Lauf
  # ==========================================================================
  run_kurz <- ifelse(grepl("[0-9]{4}-[0-9]{4}", runs),
                     paste0(sub("_.*$", "", runs), ": ",
                            sub(".*([0-9]{4}-[0-9]{4}).*", "\\1", runs)),
                     runs)
  names(run_kurz) <- runs

  run_pal <- c("#2166ac", "#b2182b", "#e08214", "#1b7837", "#762a83", "#5e3c99")
  farben  <- c(setNames("grey30", lab_de),
               setNames("grey75", lab_bl),
               setNames(run_pal[seq_along(runs)], runs))

  # ==========================================================================
  # (f) Text-Block (Mittelwerte als Zahlen) + Subtitle (Delta DE-Mittel)
  # ==========================================================================
  txt_de  <- sprintf("%s: %.1f \u00b0C, %d mm", lab_de, mean(cloud$MAT), round(mean(cloud$MAP)))
  txt_bl  <- sprintf("%s: %.1f \u00b0C, %d mm", lab_bl, mean(cloud_bl$MAT), round(mean(cloud_bl$MAP)))
  txt_mid <- sprintf("%s (%s): %.1f \u00b0C, %d mm",
                     run_kurz[mids$Lauf], mids$Typ, mids$MAT, round(mids$MAP))
  mean_txt <- paste(c(txt_de, txt_bl, txt_mid), collapse = "\n")

  ref_de <- mids %>% filter(Lauf == ref,  Typ == "DE-Mittel")
  cmp_de <- mids %>% filter(Lauf != ref,  Typ == "DE-Mittel")
  sub_txt <- NULL
  if (nrow(cmp_de) > 0)
    sub_txt <- paste0("\u0394 DE-Mittel: ", paste(sprintf(
      "%s: %+.1f \u00b0C, %+d mm", run_kurz[cmp_de$Lauf],
      cmp_de$MAT - ref_de$MAT, round(cmp_de$MAP - ref_de$MAP)), collapse = "   |   "))

  # ==========================================================================
  # (g) Plot: ein durchgehender ggplot()-Aufruf, jede Ebene eine Zeile
  # ==========================================================================
  p <- ggplot() +
    geom_point(data = cloud,    aes(MAP, MAT, colour = lab_de), size = cloud_size) +
    geom_point(data = cloud_bl, aes(MAP, MAT, colour = lab_bl), size = cloud_size) +
    geom_segment(data = segs, aes(MAP_ref, MAT_ref, xend = MAP, yend = MAT, colour = Lauf),
                 linetype = "dashed", linewidth = 0.7,
                 arrow = grid::arrow(length = grid::unit(0.2, "cm"))) +
    geom_point(data = mids, aes(MAP, MAT, colour = Lauf, shape = Typ),
               size = point_size, stroke = 1) +
    annotate("text", x = -Inf, y = Inf, label = mean_txt,
             colour = "black", size = 4, hjust = -0.05, vjust = 1.1) +
    scale_colour_manual(name = NULL, values = farben, breaks = names(farben),
                        labels = c(lab_de, lab_bl, run_kurz)) +
    scale_shape_manual(name = NULL,
                       values = setNames(c(17, 19), c("DE-Mittel", typ_stat))) +
    guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 4, linetype = 0)),
           shape  = guide_legend(order = 2, override.aes = list(size = 4, colour = "black"))) +
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
    tag     <- gsub("[^A-Za-z0-9]+", "_", MASTER_ID.choose[1])
    cmp_tag <- if (length(runs) > 1) paste0("_vs_", paste(runs[-1], collapse = "_")) else ""
    ggsave(file.path(save_dir, paste0("MATMAP_", BL_choose, "_", tag, "_", ref, cmp_tag, ".png")),
           p, width = 8, height = 6, dpi = 200)
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
