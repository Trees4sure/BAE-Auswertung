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
    if (!all(c("id", "NUTS_NAME") %in% names(id_nuts)))
      stop("nuts_rds braucht die Spalten 'id' und 'NUTS_NAME'.")
    clim <- dplyr::left_join(clim, id_nuts[, c("id", "NUTS_NAME")], by = "id")

    # NUTS_NAME -> BL (NRW/SA; Stadtstaaten zugeschlagen: Berlin->BB, Bremen->NI,
    # Hamburg->SH).
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
    boden_sel <- data.frame(id        = as.integer(boden$id_bwi_bze),
                            MASTER_ID = as.character(boden$master_id_boden),
                            stringsAsFactors = FALSE)
    boden_sel <- boden_sel[grepl("\\S", boden_sel$MASTER_ID), ]
    clim <- dplyr::left_join(clim, boden_sel, by = "id")

    if (!is.null(cache_rds)) saveRDS(clim, cache_rds)
  }

  # ==========================================================================
  # (b) Wolken-Ebenen: alle DE-Punkte + ein Bundesland (Referenzlauf)
  # ==========================================================================
  cloud <- clim[clim$Zeitlauf == Klimalauf.choose &
                is.finite(clim$MAT) & is.finite(clim$MAP), ]
  if (nrow(cloud) == 0) stop("Referenzlauf '", Klimalauf.choose, "' nicht in den Daten.")

  bl_pick  <- BL_choose
  mid_pick <- MASTER_ID.choose
  typ_stat <- if (!is.null(New_label)) New_label else "Station"

  cloud_bl <- cloud[!is.na(cloud$BL) & cloud$BL == bl_pick, ]
  if (nrow(cloud_bl) == 0)
    warning("Keine Punkte fuer Bundesland '", bl_pick, "'.", call. = FALSE)

  lab_de <- "alle DE-Punkte"
  lab_bl <- paste0("Bundesland ", bl_pick)

  # ==========================================================================
  # (c) Mittelpunkte je Lauf: DE-Mittel + Stationsmittel (eine Zeile je Lauf)
  # ==========================================================================
  runs <- c(Klimalauf.choose, if (is.null(Klimalauf_compare)) NULL else as.character(Klimalauf_compare))

  mids <- data.frame()
  for (r in runs) {
    d <- clim[clim$Zeitlauf == r & is.finite(clim$MAT) & is.finite(clim$MAP), ]
    if (nrow(d) == 0) { warning("Lauf '", r, "' nicht in den Daten.", call. = FALSE); next }

    # DE-weites Mittel (ein Punkt je Lauf)
    mids <- rbind(mids, data.frame(Lauf = r, Typ = "DE-Mittel",
                                   MAP = mean(d$MAP), MAT = mean(d$MAT)))
    # Stationsmittel ueber die gewaehlten MASTER_IDs (ein Punkt je Lauf)
    ds <- d[d$MASTER_ID %in% mid_pick, ]
    if (nrow(ds) > 0)
      mids <- rbind(mids, data.frame(Lauf = r, Typ = typ_stat,
                                     MAP = mean(ds$MAP), MAT = mean(ds$MAT)))
  }

  # ==========================================================================
  # (d) Verschiebungs-Pfeile: Referenz-Mittel -> Vergleichs-Mittel (je Typ)
  # ==========================================================================
  cmp_runs <- setdiff(runs, Klimalauf.choose)
  segs <- data.frame()
  if (show_shift) for (r in cmp_runs) for (ty in unique(mids$Typ)) {
    a <- mids[mids$Lauf == Klimalauf.choose & mids$Typ == ty, ]
    b <- mids[mids$Lauf == r               & mids$Typ == ty, ]
    if (nrow(a) == 1 && nrow(b) == 1)
      segs <- rbind(segs, data.frame(Lauf = r, MAP = a$MAP, MAT = a$MAT,
                                     xend = b$MAP, yend = b$MAT))
  }

  # ==========================================================================
  # (e) Farben + Legenden-Labels (Lauf-Namen ohne Modell, z.B. "RCP85: 2071-2100")
  # ==========================================================================
  pretty_run <- function(x) {
    szen <- sub("_.*$", "", x)                                # erstes Token = Szenario
    jahr <- sub(".*?([0-9]{4}-[0-9]{4}).*", "\\1", x)         # Periode
    ifelse(grepl("[0-9]{4}-[0-9]{4}", x), paste0(szen, ": ", jahr), x)
  }

  run_levels <- unique(mids$Lauf)                             # Referenz zuerst
  run_pal    <- c("#2166ac", "#b2182b", "#e08214", "#1b7837", "#762a83", "#5e3c99")
  run_cols   <- stats::setNames(run_pal[seq_along(run_levels)], run_levels)

  col_levels <- c(lab_de, lab_bl, run_levels)
  col_values <- c(lab_de = "grey30", lab_bl = "grey75", run_cols)
  names(col_values)[1:2] <- c(lab_de, lab_bl)
  col_labels <- c(lab_de, lab_bl, pretty_run(run_levels))     # nur die Laeufe kuerzen

  mids$Lauf <- factor(mids$Lauf, levels = run_levels)
  if (nrow(segs) > 0) segs$Lauf <- factor(segs$Lauf, levels = run_levels)

  # Subtitle = Delta der DE-Mittelwerte (Vergleich - Referenz)
  ref_de  <- mids[mids$Lauf == Klimalauf.choose & mids$Typ == "DE-Mittel", ]
  d_parts <- character(0)
  for (r in cmp_runs) {
    cd <- mids[mids$Lauf == r & mids$Typ == "DE-Mittel", ]
    if (nrow(cd) == 1)
      d_parts <- c(d_parts, sprintf("%s: %+.1f \u00b0C, %+d mm", pretty_run(r),
                                    cd$MAT - ref_de$MAT, round(cd$MAP - ref_de$MAP)))
  }
  sub_txt <- if (length(d_parts)) paste0("\u0394 DE-Mittel: ", paste(d_parts, collapse = "   |   ")) else NULL

  # ==========================================================================
  # (f) Plot Layer fuer Layer (jede Zeile einzeln an-/abschaltbar)
  # ==========================================================================
  p <- ggplot()
  p <- p + geom_point(data = cloud,    aes(MAP, MAT, colour = lab_de), size = cloud_size)
  p <- p + geom_point(data = cloud_bl, aes(MAP, MAT, colour = lab_bl), size = cloud_size)
  if (nrow(segs) > 0)
    p <- p + geom_segment(data = segs, aes(MAP, MAT, xend = xend, yend = yend, colour = Lauf),
                          linetype = "dashed", linewidth = 0.7,
                          arrow = grid::arrow(length = grid::unit(0.2, "cm")))
  p <- p + geom_point(data = mids, aes(MAP, MAT, colour = Lauf, shape = Typ),
                      size = point_size, stroke = 1)

  p <- p + scale_colour_manual(name = NULL, values = col_values,
                               breaks = col_levels, limits = col_levels, labels = col_labels)
  p <- p + scale_shape_manual(name = NULL,
                              values = stats::setNames(c(17, 19), c("DE-Mittel", typ_stat)))
  p <- p + guides(
    colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 4, linetype = 0)),
    shape  = guide_legend(order = 2, override.aes = list(size = 4, colour = "black")))

  p <- p + labs(title = "Klimaraum: Jahresniederschlag (MAP) vs. Jahresmitteltemperatur (MAT)",
                subtitle = sub_txt,
                x = "Jahresniederschlag MAP [mm]",
                y = "Jahresmitteltemperatur MAT [\u00b0C]")
  p <- p + theme_minimal(base_size = 12)
  p <- p + theme(panel.grid.minor = element_blank())

  # ==========================================================================
  # (g) optional speichern
  # ==========================================================================
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    tag <- gsub("[^A-Za-z0-9]+", "_", mid_pick[1])
    cmp_tag <- if (length(cmp_runs)) paste0("_vs_", paste(cmp_runs, collapse = "_")) else ""
    ggsave(file.path(save_dir, paste0("MATMAP_", bl_pick, "_", tag, "_",
                                      Klimalauf.choose, cmp_tag, ".png")),
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
