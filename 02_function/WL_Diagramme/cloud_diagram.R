# =============================================================================
# Klima-Wolken-Diagramm (MAT vs. MAP) als parametrisierte Funktion
#
# Cloud_diagram_function() zeichnet die deutschlandweite Klima-Punktwolke fuer
# EINEN Klimalauf und hebt hervor:
#   - alle DE-Punkte            -> dunkelgrau
#   - ein Bundesland (BL)        -> hellgrau
#   - eine/mehrere MASTER_ID(s)  -> rot (Station) + Beschriftung
#   - Referenz-Klimaraum         -> blaue 95%-Perzentil-Box + Mittel (Dreieck)
# Optional ein Vergleichslauf (Klimalauf_compare):
#   - darkred 95%-Perzentil-Box + Mittel + Beschriftung UEBER der Box
#   - erwartete Lage der MASTER_ID im Vergleichslauf (darkred Punkt) + Pfeil von
#     der Referenz-Lage dorthin
#   - Delta-Text (\u00d8 MAT/MAP Vergleich - Referenz)
#
# MAT/MAP kommen aus 6.6 (BWI_Klimadaten_MATMAP.RDS, dort aus 1049/1050 gemittelt).
# BL wird RAEUMLICH ueber die NUTS1-Landesgrenzen bestimmt (Lookup id -> NUTS_NAME,
# einmal vorgerechnet als nuts_id.RDS), MASTER_ID aus der Boden-Join-CSV. Beide
# sind je Punkt (id) lauf-unabhaengig -> sie werden EINMAL an alle Laeufe gehaengt
# und als Cache-RDS abgelegt; weitere Aufrufe lesen nur noch + filtern.
#
# Pakete: dplyr, ggplot2 (grid fuer den Pfeil; base). sf nur fuer das einmalige
# Erzeugen von nuts_id.RDS (s. Block am Dateiende), nicht im Normalbetrieb.
# =============================================================================


#' @param Klimalauf.choose  Referenzlauf (x/y der Wolke), Default OBS_DWD_1991-2020.
#' @param Klimalauf_compare Optionaler Vergleichslauf (NULL = keiner).
#' @param MASTER_ID.choose  Eine (oder mehrere) MASTER_ID(s) fuer die rote Station.
#' @param BL_choose         Bundesland-Kuerzel fuer die hellgraue Ebene.
#' @param New_label         Anderes Label fuer die Station (NULL = MASTER_ID).
#' @param rds_path          MAT/MAP-RDS aus 6.6.
#' @param nuts_rds          Lookup id -> NUTS_NAME (einmal vorgerechnet).
#' @param boden_csv         BWI-BZE_Klima_Boden_Join.csv (id_bwi_bze -> MASTER_ID).
#' @param cache_rds         Angereicherter Cache (BL + MASTER_ID an allen Laeufen).
#' @param rebuild_cache     TRUE = Cache neu bauen, auch wenn vorhanden.
#' @param save_dir          Ordner fuer die PNG-Ausgabe (NULL = nicht speichern).
#' @return ggplot-Objekt.
Cloud_diagram_function <- function(
    Klimalauf.choose  = "OBS_DWD_1991-2020",
    Klimalauf_compare = NULL,
    MASTER_ID.choose  = "BWI_130_36567_4",
    BL_choose         = "MV",
    New_label         = NULL,
    rds_path   = file.path(out_base, "BWI_Klimadaten_MATMAP.RDS"),
    nuts_rds   = file.path("01_data", "Grundlagen/Geodaten",
                           "DE_L\u00e4ndergrenzen/nuts250_1231", "nuts_id.RDS"),
    boden_csv  = file.path(dir_klima, "BWI-BZE_Klima_Boden_Join.csv"),
    cache_rds  = file.path(out_base, "BWI_Klimadaten_MATMAP_BL_MID.RDS"),
    rebuild_cache = FALSE,
    save_dir   = NULL) {

  library(ggplot2)

  # -- (a) Daten laden + EINMAL mit BL (NUTS1) und MASTER_ID anreichern --------
  if (!rebuild_cache && !is.null(cache_rds) && file.exists(cache_rds)) {
    clim <- readRDS(cache_rds)                       # schon angereichert
  } else {
    clim <- readRDS(rds_path)
    # Spalten: Lon | Lat | altitude | id | id_7004 | Zeitlauf | MAT | MAP
    # clim$id == id_bwi_bze.

    # BL ueber den NUTS1-Lookup (id -> NUTS_NAME). Lauf-unabhaengig -> auf id joinen.
    if (!file.exists(nuts_rds))
      stop("nuts_rds nicht gefunden: '", nuts_rds, "'. Einmal mit dem Block am ",
           "Dateiende von cloud_diagram.R erzeugen.")
    id_nuts <- readRDS(nuts_rds)
    if (!all(c("id", "NUTS_NAME") %in% names(id_nuts)))
      stop("nuts_rds braucht die Spalten 'id' und 'NUTS_NAME'.")
    clim <- dplyr::left_join(clim, id_nuts[, c("id", "NUTS_NAME")], by = "id")

    # NUTS_NAME -> BL (Kodierung wie in der Vorlage: NRW/SA, Stadtstaaten
    # zugeschlagen: Berlin->BB, Bremen->NI, Hamburg->SH).
    clim$BL <- dplyr::case_when(
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
    boden <- utils::read.csv2(boden_csv, stringsAsFactors = FALSE)
    boden_sel <- data.frame(
      id        = as.integer(boden$id_bwi_bze),
      MASTER_ID = as.character(boden$master_id_boden),
      stringsAsFactors = FALSE)
    boden_sel <- boden_sel[grepl("\\S", boden_sel$MASTER_ID), ]   # ohne Boden raus
    clim <- dplyr::left_join(clim, boden_sel, by = "id")

    if (!is.null(cache_rds)) saveRDS(clim, cache_rds)
  }

  # -- (b) Referenzlauf + (optional) Vergleichslauf herausziehen ---------------
  fin   <- function(d) d[is.finite(d$MAT) & is.finite(d$MAP), , drop = FALSE]
  cloud <- fin(clim[clim$Zeitlauf == Klimalauf.choose, , drop = FALSE])
  if (nrow(cloud) == 0)
    stop("Referenzlauf '", Klimalauf.choose, "' nicht in den Daten.")

  bl_pick  <- BL_choose
  mid_pick <- MASTER_ID.choose
  new_label <- if (!is.null(New_label)) New_label else paste(mid_pick, collapse = ", ")

  cloud_bl <- cloud[!is.na(cloud$BL) & cloud$BL == bl_pick, , drop = FALSE]
  if (nrow(cloud_bl) == 0)
    warning("Keine Punkte fuer Bundesland '", bl_pick, "'.", call. = FALSE)

  # je MASTER_ID EIN roter Punkt (mehrere Zellen je MASTER_ID -> Mittel)
  cloud_sel <- aggregate(cbind(MAT, MAP) ~ MASTER_ID,
                         cloud[cloud$MASTER_ID %in% mid_pick, , drop = FALSE], mean)
  if (nrow(cloud_sel) == 0)
    stop("MASTER_ID '", paste(mid_pick, collapse = ", "),
         "' nicht im Lauf ", Klimalauf.choose, ".")
  cloud_sel$lab <- if (nrow(cloud_sel) == 1) new_label else cloud_sel$MASTER_ID

  # -- (c) Referenz-Klimaraum: 95%-Box (2.5/97.5) + Mittel ---------------------
  mat_pct <- quantile(cloud$MAT, c(0.025, 0.975), na.rm = TRUE)
  map_pct <- quantile(cloud$MAP, c(0.025, 0.975), na.rm = TRUE)

  txt <- paste0(
    "DE: ",     round(mean(cloud$MAT), 1),    "\u00b0C, ", round(mean(cloud$MAP)),    " mm\n",
    bl_pick, ": ", round(mean(cloud_bl$MAT), 1), "\u00b0C, ", round(mean(cloud_bl$MAP)), " mm\n",
    paste0(cloud_sel$lab, ": ", round(cloud_sel$MAT, 1), "\u00b0C, ",
           round(cloud_sel$MAP), " mm", collapse = "\n"))

  # -- (d) Plot von hinten nach vorne ------------------------------------------
  p <- ggplot() +
    geom_point(data = cloud,    aes(MAT, MAP),
               colour = "grey30", size = 0.75, alpha = 0.35) +   # alle DE
    geom_point(data = cloud_bl, aes(MAT, MAP),
               colour = "grey75", size = 0.75, alpha = 0.9) +    # Bundesland
    # Referenz-Klimaraum (blau)
    annotate("rect", xmin = mat_pct[1], xmax = mat_pct[2],
             ymin = map_pct[1], ymax = map_pct[2],
             fill = NA, colour = "blue", linewidth = 1) +
    annotate("point", x = mean(cloud$MAT, na.rm = TRUE),
             y = mean(cloud$MAP, na.rm = TRUE),
             colour = "blue", size = 4, shape = 17) +
    annotate("text", x = mean(mat_pct), y = map_pct[2], label = Klimalauf.choose,
             colour = "blue", size = 3.5, vjust = -0.6, fontface = "bold") +
    # ausgewaehlte Station (Referenz, rot)
    geom_point(data = cloud_sel, aes(MAT, MAP),
               colour = "white", fill = "#c0392b",
               shape = 21, size = 3, stroke = 0.8) +
    geom_text(data = cloud_sel, aes(MAT, MAP, label = lab),
              colour = "#c0392b", size = 5, vjust = 2) +
    # Kennzahlen-Block oben links (datenrelativ verankert)
    annotate("text", x = -Inf, y = Inf, label = txt,
             colour = "black", size = 4, hjust = -0.05, vjust = 1.1)

  # -- (e) Vergleichslauf (optional): Box + Beschriftung + erwartete Lage ------
  if (!is.null(Klimalauf_compare)) {
    cmp <- fin(clim[clim$Zeitlauf == Klimalauf_compare, , drop = FALSE])
    if (nrow(cmp) == 0) {
      warning("Vergleichslauf '", Klimalauf_compare,
              "' nicht in den Daten - Box uebersprungen.", call. = FALSE)
    } else {
      mat_pct_c <- quantile(cmp$MAT, c(0.025, 0.975), na.rm = TRUE)
      map_pct_c <- quantile(cmp$MAP, c(0.025, 0.975), na.rm = TRUE)
      dMAT <- mean(cmp$MAT, na.rm = TRUE) - mean(cloud$MAT, na.rm = TRUE)
      dMAP <- mean(cmp$MAP, na.rm = TRUE) - mean(cloud$MAP, na.rm = TRUE)

      # erwartete Lage der Station(en) im Vergleichslauf (Mittel je MASTER_ID)
      sel_c <- aggregate(cbind(MAT, MAP) ~ MASTER_ID,
                         cmp[cmp$MASTER_ID %in% mid_pick, , drop = FALSE], mean)

      p <- p +
        annotate("rect", xmin = mat_pct_c[1], xmax = mat_pct_c[2],
                 ymin = map_pct_c[1], ymax = map_pct_c[2],
                 fill = NA, colour = "darkred", linewidth = 1) +
        annotate("point", x = mean(cmp$MAT, na.rm = TRUE),
                 y = mean(cmp$MAP, na.rm = TRUE),
                 colour = "darkred", size = 4, shape = 17) +
        # Bezeichnung UEBER dem darkred-Kasten
        annotate("text", x = mean(mat_pct_c), y = map_pct_c[2], label = Klimalauf_compare,
                 colour = "darkred", size = 3.5, vjust = -0.6, fontface = "bold") +
        # Delta-Text oben rechts
        annotate("text", x = Inf, y = Inf,
                 label = paste0("\u0394 zu ", Klimalauf_compare, ": MAT ",
                                round(dMAT, 1), "\u00b0C, MAP ", round(dMAP), " mm"),
                 colour = "darkred", size = 4, hjust = 1.1, vjust = 1.1)

      if (nrow(sel_c) > 0) {
        sel_c$lab <- if (nrow(sel_c) == 1) new_label else sel_c$MASTER_ID
        # Pfeil von der Referenz-Lage zur erwarteten Zukunfts-Lage je MASTER_ID
        seg <- merge(cloud_sel[, c("MASTER_ID", "MAT", "MAP")],
                     sel_c[, c("MASTER_ID", "MAT", "MAP")],
                     by = "MASTER_ID", suffixes = c("_ref", "_cmp"))
        p <- p +
          geom_segment(data = seg,
                       aes(x = MAT_ref, y = MAP_ref, xend = MAT_cmp, yend = MAP_cmp),
                       colour = "darkred", linewidth = 0.6, linetype = "dashed",
                       arrow = grid::arrow(length = grid::unit(0.18, "cm"))) +
          geom_point(data = sel_c, aes(MAT, MAP),
                     colour = "white", fill = "darkred",
                     shape = 21, size = 3, stroke = 0.8) +
          geom_text(data = sel_c, aes(MAT, MAP, label = lab),
                    colour = "darkred", size = 4, vjust = 2)
      }
    }
  }

  # -- (f) Achsen / Stil --------------------------------------------------------
  p <- p +
    coord_cartesian(clip = "off") +
    labs(title    = "Vergleich Jahres-Temperatur (MAT) und Niederschlag (MAP)",
         subtitle = paste0(Klimalauf.choose, "  \u00b7  alle DE (dunkelgrau)  \u00b7  ",
                           bl_pick, " (hellgrau)  \u00b7  Auswahl (rot)"),
         x = "MAT [\u00b0C]", y = "MAP [mm]") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # -- (g) optional speichern ---------------------------------------------------
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    tag <- gsub("[^A-Za-z0-9]+", "_", mid_pick[1])
    fn  <- paste0("MATMAP_", bl_pick, "_", tag, "_", Klimalauf.choose,
                  if (!is.null(Klimalauf_compare)) paste0("_vs_", Klimalauf_compare) else "",
                  ".png")
    ggsave(file.path(save_dir, fn), p, width = 8, height = 6, dpi = 200)
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
