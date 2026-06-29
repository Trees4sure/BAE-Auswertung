# =============================================================================
# Klima-Wolken-Diagramm (MAP vs. MAT) als parametrisierte Funktion
#
# Cloud_diagram_function() zeichnet die deutschlandweite Klima-Punktwolke fuer
# EINEN Referenzlauf:
#   - alle DE-Punkte    -> dunkelgrau
#   - ein Bundesland (BL) -> hellgrau
# und darueber die MITTELPUNKTE (Klima-Schwerpunkte) als Punkte:
#   - DE-Mittel (Dreieck) und die ausgewaehlte MASTER_ID-Station (Kreis),
#     je Lauf in einer eigenen Farbe.
# Optional ein/mehrere Vergleichslauf(e) (Klimalauf_compare als Vektor): deren
# DE-Mittel + Stationslage werden ebenfalls gezeichnet und mit einem Pfeil von
# der Referenz-Lage verbunden -> sichtbare "Mittelpunktverschiebung".
#
# Keine Boxen, keine Text-Bloecke - die Zuordnung laeuft ueber eine normale
# Legende (Farbe = Lauf bzw. Punkt-Klasse, Form = DE-Mittel/Station).
# Achsen: x = MAP (Niederschlag), y = MAT (Temperatur).
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

  # -- (b) Referenzlauf herausziehen + Ebenen-Daten ----------------------------
  fin   <- function(d) d[is.finite(d$MAT) & is.finite(d$MAP), , drop = FALSE]
  cloud <- fin(clim[clim$Zeitlauf == Klimalauf.choose, , drop = FALSE])
  if (nrow(cloud) == 0)
    stop("Referenzlauf '", Klimalauf.choose, "' nicht in den Daten.")

  bl_pick  <- BL_choose
  mid_pick <- MASTER_ID.choose
  typ_stat <- if (!is.null(New_label)) New_label else "Station"

  cloud_bl <- cloud[!is.na(cloud$BL) & cloud$BL == bl_pick, , drop = FALSE]
  if (nrow(cloud_bl) == 0)
    warning("Keine Punkte fuer Bundesland '", bl_pick, "'.", call. = FALSE)

  cat_all <- "alle DE-Punkte"
  cat_bl  <- paste0("Bundesland ", bl_pick)

  # -- (c) Mittelpunkte je Lauf: DE-Mittel + Stationslage ----------------------
  # helper: Klima-Schwerpunkte eines Laufs (DE-Mittel + Mittel je MASTER_ID)
  mids_of <- function(d, lauf) {
    de <- data.frame(MAP = mean(d$MAP, na.rm = TRUE), MAT = mean(d$MAT, na.rm = TRUE),
                     MASTER_ID = NA_character_, Lauf = lauf, Typ = "DE-Mittel")
    sub <- d[d$MASTER_ID %in% mid_pick, , drop = FALSE]
    st <- if (nrow(sub) == 0) NULL else {
      a <- aggregate(cbind(MAP, MAT) ~ MASTER_ID, sub, mean)
      data.frame(MAP = a$MAP, MAT = a$MAT, MASTER_ID = a$MASTER_ID,
                 Lauf = lauf, Typ = typ_stat)
    }
    rbind(de, st)
  }

  ref   <- Klimalauf.choose
  mids  <- mids_of(cloud, ref)

  cmp_runs <- if (is.null(Klimalauf_compare)) character(0) else as.character(Klimalauf_compare)
  segs <- data.frame()                      # Pfeile Referenz -> Vergleich
  for (r in cmp_runs) {
    cmp <- fin(clim[clim$Zeitlauf == r, , drop = FALSE])
    if (nrow(cmp) == 0) {
      warning("Vergleichslauf '", r, "' nicht in den Daten - uebersprungen.",
              call. = FALSE); next
    }
    m_r  <- mids_of(cmp, r)
    mids <- rbind(mids, m_r)
    # Pfeil je Typ (DE-Mittel + je MASTER_ID) von der Referenz-Lage zur Vergleichs-Lage
    a <- merge(mids[mids$Lauf == ref, c("Typ", "MASTER_ID", "MAP", "MAT")],
               m_r[, c("Typ", "MASTER_ID", "MAP", "MAT")],
               by = c("Typ", "MASTER_ID"), suffixes = c("", "_end"))
    if (nrow(a) > 0)
      segs <- rbind(segs, data.frame(MAP = a$MAP, MAT = a$MAT,
                                     xend = a$MAP_end, yend = a$MAT_end, Lauf = r))
  }

  # -- (d) Farb-/Form-Skalen ----------------------------------------------------
  run_levels <- c(ref, cmp_runs)
  run_pal    <- c("#2166ac", "#b2182b", "#e08214", "#1b7837", "#762a83", "#5e3c99")
  run_cols   <- stats::setNames(run_pal[seq_along(run_levels)], run_levels)
  col_vals   <- c(stats::setNames("grey30", cat_all),
                  stats::setNames("grey75", cat_bl), run_cols)

  mids$Lauf <- factor(mids$Lauf, levels = run_levels)
  if (nrow(segs) > 0) segs$Lauf <- factor(segs$Lauf, levels = run_levels)

  # -- (e) Plot: Wolke + Mittelpunkte + Verschiebungs-Pfeile -------------------
  p <- ggplot() +
    geom_point(data = cloud,    aes(MAP, MAT, colour = cat_all), size = 0.75) +
    geom_point(data = cloud_bl, aes(MAP, MAT, colour = cat_bl),  size = 0.75)
  if (nrow(segs) > 0)
    p <- p + geom_segment(data = segs,
                          aes(x = MAP, y = MAT, xend = xend, yend = yend, colour = Lauf),
                          linewidth = 0.6, linetype = "dashed",
                          arrow = grid::arrow(length = grid::unit(0.18, "cm")))
  p <- p +
    geom_point(data = mids, aes(MAP, MAT, colour = Lauf, shape = Typ),
               size = 3.5, stroke = 1) +
    scale_colour_manual(name = NULL, values = col_vals,
                        limits = names(col_vals), breaks = names(col_vals)) +
    scale_shape_manual(name = NULL,
                       values = stats::setNames(c(17, 19), c("DE-Mittel", typ_stat))) +
    guides(colour = guide_legend(order = 1,
                                 override.aes = list(shape = 16, size = 3, linetype = 0)),
           shape  = guide_legend(order = 2,
                                 override.aes = list(size = 3, colour = "black"))) +
    labs(title = "Klimaraum MAP vs. MAT \u2013 Mittelpunktverschiebung",
         subtitle = paste(run_levels, collapse = "  \u2192  "),
         x = "Jahresniederschlag MAP [mm]",
         y = "Jahresmitteltemperatur MAT [\u00b0C]") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())

  # -- (f) optional speichern ---------------------------------------------------
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    tag <- gsub("[^A-Za-z0-9]+", "_", mid_pick[1])
    cmp_tag <- if (length(cmp_runs)) paste0("_vs_", paste(cmp_runs, collapse = "_")) else ""
    fn  <- paste0("MATMAP_", bl_pick, "_", tag, "_", Klimalauf.choose, cmp_tag, ".png")
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
