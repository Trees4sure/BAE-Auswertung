# ============================================================================
# heatmap_bae_zukunft_standalone.R
# ----------------------------------------------------------------------------
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App). Erzeugt pro
# Bewertungsstufe ZWEI Darstellungen der BAE-Heatmap für eine MASTER_ID:
#
#   layouts = "grid"       -> volles facet_grid (Szenario × Zeitraum),
#                             mit grauem Rahmen um jeden Kasten.
#                             Datei: Heatmap_Grid_<st>_<MID>_<Modell>.png
#
#   layouts = "patchwork"  -> kompakte Patchwork-Grafik:
#                             oben  OBS/Referenz × Vergangenheits-Zeiträume,
#                             unten RCP-Szenarien × Zukunfts-Zeiträume.
#                             Keine leeren Kästen -> platzsparend.
#                             Datei: Heatmap_Patch_<st>_<MID>_<Modell>.png
#
# Standard: beide werden erzeugt (layouts = c("grid","patchwork")).
#
# Weitere Features:
#   - RCP45/RCP85-Varianten (v2/v3) erhalten eigene Zeilen (Szen_label)
#   - Zeilen-Labels frei umbenennbar (szen_rename)
#   - Modell (MPICLM, ECECMO, DWD, …) im Untertitel UND Dateinamen
#   - grauer Rahmen an/aus über 'rahmen'
#
# Grundlage: heatmap_bae_function() (Farbpalette, Stufen-Mappings, Kachel-Stil).
#
# Erwartete Spalten in `data`:
#   MASTER_ID, Baumart, TV, Klimalauf, BAE_3ST, BAE_4ST, BAE_5ST
# `Klimalauf` z. B. "OBS_DWD_1991-2020", "RCP45_MPICLM_2071-2100",
#                   "RCP45-v3_MPICLM_2021-2050", "RCP85_MPICLM_2071-2100_v2".
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
# für layouts="patchwork" zusätzlich: patchwork  (install.packages("patchwork"))

heatmap_bae_zukunft_function <- function(data,
                                         master_id,
                                         stufen         = c("3st", "4st", "5st"),
                                         layouts        = c("grid", "patchwork"),
                                         rcp_zukunft_ab = 2021,          # RCP: Startjahr >= dieser Wert bleibt
                                         obs_alle       = TRUE,          # OBS: alle Zeiträume zeigen
                                         szen_rename    = character(0),  # Zeilen-Labels umbenennen, s.u.
                                         rahmen         = TRUE,          # grauer Rahmen um jeden Kasten
                                         out_dir        = "04_results/BAE_Auswertung/heatmap") {

  layouts <- match.arg(layouts, choices = c("grid", "patchwork"), several.ok = TRUE)
  if ("patchwork" %in% layouts && !requireNamespace("patchwork", quietly = TRUE)) {
    warning("Paket 'patchwork' fehlt -> nur 'grid' wird erzeugt. ",
            "install.packages(\"patchwork\")")
    layouts <- setdiff(layouts, "patchwork")
  }

  # ---- 0. Farbpalette + Stufen-Mappings (aus heatmap_bae_function) ----------
  custom_palette <- c(
    "sehr empfohlen"        = "#1A9850",
    "empfohlen"             = "#A6D96A",
    "mäßig empfohlen"       = "#FEE08B",
    "wenig empfohlen"       = "#FDAE61",
    "nicht empfohlen"       = "#A50026",
    "pBv"                   = "#404040",
    "Keine Datengrundlage"  = "#B0B0B0"
  )

  maps <- list(
    "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
    "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
              "4" = "nicht empfohlen"),
    "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
              "4" = "wenig empfohlen", "5" = "nicht empfohlen")
  )
  bae_col <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST")

  map_val <- function(val, mapping) {
    val <- as.character(val)
    dplyr::case_when(
      val %in% names(mapping) ~ unname(mapping[val]),
      val == "pBv"            ~ "pBv",
      TRUE                    ~ "Keine Datengrundlage"
    )
  }

  # ---- 1. Auf MASTER_ID filtern ---------------------------------------------
  d <- data %>% dplyr::filter(as.character(MASTER_ID) == as.character(master_id))
  if (nrow(d) == 0) {
    message("Keine Daten für MASTER_ID: ", master_id)
    return(invisible(NULL))
  }

  # ---- 2. Szenario / Modell / Zeitraum / Variante aus Klimalauf ableiten ----
  d <- d %>%
    dplyr::mutate(
      Klimalauf = as.character(Klimalauf),
      Zeitraum  = stringr::str_extract(Klimalauf, "\\d{4}-\\d{4}"),
      Variante  = tolower(stringr::str_extract(Klimalauf, "[vV][0-9]+")),
      Szenario  = stringr::str_remove(stringr::str_extract(Klimalauf, "^[^_]+"),
                                      "[-_]?[vV][0-9]+$"),
      Modell    = stringr::str_remove(
                    stringr::str_remove(Klimalauf, "^[^_]+_"),   # Szenario_ entfernen
                    "_?\\d{4}-\\d{4}.*$"),                       # _Zeitraum(+Variante) entfernen
      Szen_label = ifelse(is.na(Variante), Szenario,
                          paste0(Szenario, "_", Variante)),
      Startjahr  = suppressWarnings(as.integer(stringr::str_sub(Zeitraum, 1, 4)))
    )

  # ---- 2b. Zeilen-Labels optional umbenennen --------------------------------
  # szen_rename: benannter Vektor  c("<intern>" = "<Anzeige>")
  #   z.B. c("RCP45_v3" = "RCP45_real", "RCP45" = "RCP45_normal", "OBS" = "Referenz")
  if (length(szen_rename) > 0) {
    idx <- match(d$Szen_label, names(szen_rename))
    treffer <- !is.na(idx)
    d$Szen_label[treffer] <- unname(szen_rename[idx[treffer]])
  }

  ohne_zeit <- is.na(d$Zeitraum)
  if (any(ohne_zeit)) {
    message("Hinweis: ", sum(ohne_zeit), " Zeile(n) ohne erkennbaren Zeitraum ",
            "werden ignoriert (Klimalauf: ",
            paste(unique(d$Klimalauf[ohne_zeit]), collapse = ", "), ").")
    d <- d[!ohne_zeit, , drop = FALSE]
  }
  if (nrow(d) == 0) {
    message("Nach Zeitraum-Filter keine Daten mehr für: ", master_id)
    return(invisible(NULL))
  }

  # ---- 3. RCP: nur Zukunft, OBS: (optional) alles ---------------------------
  is_rcp   <- grepl("^RCP", d$Szenario, ignore.case = TRUE)
  keep_rcp <- !is.na(d$Startjahr) & d$Startjahr >= rcp_zukunft_ab
  d <- d[ (!is_rcp) | keep_rcp, , drop = FALSE]
  if (!obs_alle) {
    is_obs <- grepl("^OBS", d$Szenario, ignore.case = TRUE)
    d <- d[!is_obs | (!is.na(d$Startjahr) & d$Startjahr >= rcp_zukunft_ab), ,
           drop = FALSE]
  }
  if (nrow(d) == 0) {
    message("Nach Zukunfts-Filter keine Daten mehr für: ", master_id)
    return(invisible(NULL))
  }

  # ---- 4. Faktor-Ordnung (global) -------------------------------------------
  szen_levels <- sort(unique(d$Szen_label))
  zeit_levels <- sort(unique(d$Zeitraum))                              # chronologisch
  tv_levels   <- sort(unique(paste0("TV", d$TV)), decreasing = TRUE)   # TV1 oben … TVn unten
  ba_levels   <- sort(unique(as.character(d$Baumart)))

  d <- d %>%
    dplyr::mutate(
      Szen_label = factor(Szen_label, levels = szen_levels),
      Zeitraum   = factor(Zeitraum,   levels = zeit_levels),
      TV         = factor(paste0("TV", TV), levels = tv_levels),
      Baumart    = factor(Baumart,    levels = ba_levels),
      ist_rcp    = grepl("^RCP", Szenario, ignore.case = TRUE)
    )

  modelle     <- sort(unique(as.character(d$Modell)))
  modelle     <- modelle[!is.na(modelle) & nzchar(modelle)]
  modelle_str <- if (length(modelle)) paste(modelle, collapse = "-") else "NA"

  mid_dir <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- 5. gemeinsame Theme-Bausteine ----------------------------------------
  rahmen_theme <- if (rahmen)
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey70",
                                               linewidth = 0.6),
      panel.border     = ggplot2::element_rect(color = "grey70", fill = NA,
                                               linewidth = 0.6),
      panel.spacing    = ggplot2::unit(0.5, "lines")
    )
  else
    ggplot2::theme(panel.spacing = ggplot2::unit(0.2, "lines"))

  basis_theme <- function(x_labels = TRUE) {
    ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        strip.text      = ggplot2::element_text(face = "bold", size = 9),
        axis.text.x     = if (x_labels)
          ggplot2::element_text(angle = 0, hjust = 0.5, face = "bold", size = 9)
          else ggplot2::element_blank(),
        axis.ticks.x    = ggplot2::element_blank(),
        axis.text.y     = ggplot2::element_text(size = 9),
        panel.grid      = ggplot2::element_blank(),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text     = ggplot2::element_text(size = 10),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      ) + rahmen_theme
  }

  geom_layer <- list(
    ggplot2::geom_tile(color = "white", linewidth = 0.5),
    ggplot2::scale_fill_manual(values = custom_palette, na.value = "#B0B0B0",
                               breaks = names(custom_palette), drop = FALSE),
    ggplot2::scale_x_discrete(position = "top")
  )

  # ---- 6a. Builder: volles Grid (alte Variante) -----------------------------
  baue_grid <- function(d_st, st) {
    ggplot2::ggplot(d_st, ggplot2::aes(x = Baumart, y = TV, fill = Kategorie)) +
      geom_layer +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::labs(
        title    = paste0("BAE-Heatmap – ", master_id),
        subtitle = paste0(st, "-stufig  |  Modell: ", modelle_str,
                          "  |  OBS = Vergangenheit, RCP = Zukunft (ab ",
                          rcp_zukunft_ab, ")"),
        x = NULL, y = "TV", fill = "Empfehlung") +
      basis_theme(x_labels = TRUE)
  }

  # ---- 6b. Builder: ein Patchwork-Block (neue Variante) ---------------------
  baue_block <- function(dblock, x_labels = TRUE) {
    dblock <- dblock %>%
      dplyr::mutate(Szen_label = droplevels(Szen_label),
                    Zeitraum   = droplevels(Zeitraum))
    ggplot2::ggplot(dblock, ggplot2::aes(x = Baumart, y = TV, fill = Kategorie)) +
      geom_layer +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::scale_y_discrete(drop = FALSE) +   # alle TV-Zeilen -> Blöcke ausgerichtet
      ggplot2::labs(x = NULL, y = "TV", fill = "Empfehlung") +
      basis_theme(x_labels = x_labels)
  }

  # ---- 7. Schleife über Bewertungsstufen ------------------------------------
  plots <- list()
  for (st in stufen) {
    kat_col <- bae_col[[st]]
    if (is.null(kat_col) || !kat_col %in% names(d)) {
      message("Spalte für Stufe '", st, "' (", kat_col, ") nicht vorhanden – übersprungen.")
      next
    }

    d_st <- d %>%
      dplyr::mutate(Kategorie = factor(map_val(.data[[kat_col]], maps[[st]]),
                                       levels = names(custom_palette)))

    # --- 7a. Grid ---
    if ("grid" %in% layouts) {
      p_grid <- baue_grid(d_st, st)
      f_grid <- file.path(mid_dir, paste0("Heatmap_Grid_", st, "_", master_id, "_",
                                          modelle_str, ".png"))
      ggplot2::ggsave(f_grid, plot = p_grid, device = "png",
                      width = 5400, height = 3000, units = "px", dpi = 300)
      message("Gespeichert: ", f_grid)
      plots[[paste0(st, "_grid")]] <- p_grid
    }

    # --- 7b. Patchwork ---
    if ("patchwork" %in% layouts) {
      d_ref <- d_st %>% dplyr::filter(!ist_rcp)   # oben
      d_rcp <- d_st %>% dplyr::filter(ist_rcp)    # unten
      bloecke <- list(); hoehen <- c()
      if (nrow(d_ref) > 0) {
        bloecke <- c(bloecke, list(baue_block(d_ref, x_labels = TRUE)))
        hoehen  <- c(hoehen, length(unique(droplevels(d_ref$Szen_label))))
      }
      if (nrow(d_rcp) > 0) {
        bloecke <- c(bloecke, list(baue_block(d_rcp, x_labels = (nrow(d_ref) == 0))))
        hoehen  <- c(hoehen, length(unique(droplevels(d_rcp$Szen_label))))
      }
      if (length(bloecke) > 0) {
        p_patch <- patchwork::wrap_plots(bloecke, ncol = 1, heights = hoehen,
                                         guides = "collect") +
          patchwork::plot_annotation(
            title    = paste0("BAE-Heatmap – ", master_id),
            subtitle = paste0(st, "-stufig  |  Modell: ", modelle_str,
                              "  |  oben: Referenz/Vergangenheit,  unten: RCP-Zukunft (ab ",
                              rcp_zukunft_ab, ")")
          ) &
          ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal")

        n_zeilen  <- sum(hoehen)
        n_spalten <- max(1, length(unique(droplevels(d_rcp$Zeitraum))),
                         length(unique(droplevels(d_ref$Zeitraum))))
        f_patch <- file.path(mid_dir, paste0("Heatmap_Patch_", st, "_", master_id, "_",
                                             modelle_str, ".png"))
        ggplot2::ggsave(f_patch, plot = p_patch, device = "png",
                        width  = 1200 + n_spalten * 900,
                        height =  700 + n_zeilen  * 520,
                        units = "px", dpi = 150, limitsize = FALSE)
        message("Gespeichert: ", f_patch)
        plots[[paste0(st, "_patch")]] <- p_patch
      }
    }
  }

  invisible(plots)
}

# ============================================================================
# BEISPIEL-AUFRUF (auskommentiert)
# ----------------------------------------------------------------------------
# install.packages("patchwork")   # einmalig, falls noch nicht vorhanden
# data <- data.table::fread("meine_bae_daten.csv")
#
# # Beide Grafiken (Grid + Patchwork) je Stufe:
# heatmap_bae_zukunft_function(data, master_id = "NR_130_08_66519")
#
# # Nur eine Variante:
# heatmap_bae_zukunft_function(data, master_id = "NR_130_08_66519", layouts = "grid")
# heatmap_bae_zukunft_function(data, master_id = "NR_130_08_66519", layouts = "patchwork")
#
# # Ohne Rahmen, Labels umbenennen, nur 3-stufig:
# heatmap_bae_zukunft_function(
#   data, master_id = "NR_130_08_66519", stufen = "3st", rahmen = FALSE,
#   szen_rename = c("OBS" = "Referenz", "RCP45_v3" = "RCP45_real"))
# ============================================================================
