# ============================================================================
# heatmap_bae_zukunft_standalone.R
# ----------------------------------------------------------------------------
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App) zum Erzeugen der
# BAE-Heatmap für eine MASTER_ID als kompakte Patchwork-Grafik:
#
#   ┌──────────────────────────────────────────────┐
#   │  OBS / Referenz   ×  Vergangenheits-Zeiträume │   (oberer Block)
#   ├──────────────────────────────────────────────┤
#   │  RCP-Szenarien    ×  Zukunfts-Zeiträume       │   (unterer Block)
#   └──────────────────────────────────────────────┘
#   je Block: Baumart (x-Achse) × TV (y-Achse)
#
# Vorteil ggü. facet_grid: KEINE leeren Kästen (OBS-Zukunft / RCP-Vergangenheit
# entfallen) -> deutlich kompakter.
#
# Weitere Features:
#   - RCP45/RCP85-Varianten (v2/v3) erhalten eigene Zeilen (Szen_label)
#   - Zeilen-Labels frei umbenennbar (szen_rename)
#   - Modell (MPICLM, ECECMO, DWD, …) im Untertitel UND Dateinamen
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
# benötigt zusätzlich: patchwork  (install.packages("patchwork"))

heatmap_bae_zukunft_function <- function(data,
                                         master_id,
                                         stufen         = c("3st", "4st", "5st"),
                                         rcp_zukunft_ab = 2021,          # RCP: Startjahr >= dieser Wert bleibt
                                         obs_alle       = TRUE,          # OBS: alle Zeiträume zeigen
                                         szen_rename    = character(0),  # Zeilen-Labels umbenennen, s.u.
                                         out_dir        = "04_results/BAE_Auswertung/heatmap") {

  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Paket 'patchwork' wird benötigt:  install.packages(\"patchwork\")")

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

  # ---- 4. Faktor-Ordnung (global, damit Blöcke zueinander passen) -----------
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

  # Modell(e) für Untertitel und Dateiname
  modelle     <- sort(unique(as.character(d$Modell)))
  modelle     <- modelle[!is.na(modelle) & nzchar(modelle)]
  modelle_str <- if (length(modelle)) paste(modelle, collapse = "-") else "NA"

  mid_dir <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- 5. Hilfsfunktion: EIN Block (Referenz ODER RCP) als ggplot -----------
  baue_block <- function(dblock, x_labels = TRUE) {
    dblock <- dblock %>%
      dplyr::mutate(
        Szen_label = droplevels(Szen_label),   # nur vorhandene Zeilen
        Zeitraum   = droplevels(Zeitraum)      # nur vorhandene Spalten -> keine leeren Kästen
      )
    ggplot2::ggplot(dblock, ggplot2::aes(x = Baumart, y = TV, fill = Kategorie)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_manual(values = custom_palette, na.value = "#B0B0B0",
                                 breaks = names(custom_palette), drop = FALSE) +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::scale_y_discrete(drop = FALSE) +   # alle TV-Zeilen -> Blöcke bleiben ausgerichtet
      ggplot2::labs(x = NULL, y = "TV", fill = "Empfehlung") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        strip.text       = ggplot2::element_text(face = "bold", size = 9),
        axis.text.x      = if (x_labels)
          ggplot2::element_text(angle = 0, hjust = 0.5, face = "bold", size = 8)
          else ggplot2::element_blank(),
        axis.ticks.x     = ggplot2::element_blank(),
        axis.text.y      = ggplot2::element_text(size = 8),
        panel.grid       = ggplot2::element_blank(),
        panel.spacing    = ggplot2::unit(0.15, "lines"),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA)
      )
  }

  # ---- 6. Je Bewertungsstufe: zwei Blöcke stapeln (Patchwork) ---------------
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

    d_ref <- d_st %>% dplyr::filter(!ist_rcp)   # OBS / Referenz  -> oben
    d_rcp <- d_st %>% dplyr::filter(ist_rcp)    # RCP-Szenarien   -> unten

    bloecke <- list(); hoehen <- c()
    if (nrow(d_ref) > 0) {
      bloecke <- c(bloecke, list(baue_block(d_ref, x_labels = TRUE)))
      hoehen  <- c(hoehen, length(unique(droplevels(d_ref$Szen_label))))
    }
    if (nrow(d_rcp) > 0) {
      # x-Labels nur oben zeigen, wenn kein Referenzblock darüber steht
      bloecke <- c(bloecke, list(baue_block(d_rcp, x_labels = (nrow(d_ref) == 0))))
      hoehen  <- c(hoehen, length(unique(droplevels(d_rcp$Szen_label))))
    }
    if (length(bloecke) == 0) next

    p <- patchwork::wrap_plots(bloecke, ncol = 1, heights = hoehen,
                               guides = "collect") +
      patchwork::plot_annotation(
        title    = paste0("BAE-Heatmap – ", master_id),
        subtitle = paste0(st, "-stufig  |  Modell: ", modelle_str,
                          "  |  oben: Referenz/Vergangenheit,  unten: RCP-Zukunft (ab ",
                          rcp_zukunft_ab, ")")
      ) &
      ggplot2::theme(legend.position = "bottom",
                     legend.direction = "horizontal")

    # Höhe grob an Zeilenzahl koppeln, Breite an Spaltenzahl
    n_zeilen  <- sum(hoehen)
    n_spalten <- max(1, length(unique(droplevels(d_rcp$Zeitraum))),
                     length(unique(droplevels(d_ref$Zeitraum))))
    datei <- file.path(mid_dir,
                       paste0("Heatmap_Zukunft_", st, "_", master_id, "_",
                              modelle_str, ".png"))
    ggplot2::ggsave(datei, plot = p, device = "png",
                    width  = 1200 + n_spalten * 900,
                    height =  700 + n_zeilen  * 520,
                    units = "px", dpi = 150, limitsize = FALSE)
    message("Gespeichert: ", datei)
    plots[[st]] <- p
  }

  invisible(plots)
}

# ============================================================================
# BEISPIEL-AUFRUF (auskommentiert)
# ----------------------------------------------------------------------------
# install.packages("patchwork")   # einmalig, falls noch nicht vorhanden
# data <- data.table::fread("meine_bae_daten.csv")
#
# # Standard: oben OBS/Referenz (Vergangenheit), unten RCP (Zukunft ab 2021)
# heatmap_bae_zukunft_function(data, master_id = "NR_130_08_66519")
#
# # OBS in "Referenz" umbenennen, v3 hübscher:
# heatmap_bae_zukunft_function(
#   data, master_id = "NR_130_08_66519",
#   szen_rename = c("OBS" = "Referenz", "RCP45_v3" = "RCP45_real"))
#
# # Nur Endperiode 2071-2100 für RCP, nur 3-stufig:
# heatmap_bae_zukunft_function(data, master_id = "NR_130_08_66519",
#                              rcp_zukunft_ab = 2071, stufen = "3st")
# ============================================================================
