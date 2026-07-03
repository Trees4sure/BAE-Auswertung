# ============================================================================
# heatmap_bae_zukunft_standalone.R
# ----------------------------------------------------------------------------
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App) zum Erzeugen der
# gefacetteten BAE-Heatmap für eine MASTER_ID:
#   - Facets:   Szenario (Zeilen)  ×  Zeitraum (Spalten)
#   - je Block: Baumart (x-Achse)  ×  TV (y-Achse)
#   - OBS zeigt die Vergangenheit, RCP45/RCP85 zeigen NUR die Zukunft
#   - RCP45/RCP85-Varianten (v2/v3) erhalten eigene, beschriftete Zeilen
#
# Grundlage: heatmap_bae_function() (Farbpalette, Stufen-Mappings, Kachel-Stil).
#
# Erwartete Spalten in `data`:
#   MASTER_ID, Baumart, TV, Klimalauf, BAE_3ST, BAE_4ST, BAE_5ST
# `Klimalauf` enthält Szenario, (Modell), Zeitraum und optional eine Variante,
# z. B. "OBS_1991-2020", "RCP45_ECECMO_2071-2100",
#       "RCP45_MPICLM_2071-2100_v2", "RCP85_MPICLM_2071-2100_v3".
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

heatmap_bae_zukunft <- function(data,
                                master_id,
                                stufen         = c("3st", "4st", "5st"),
                                rcp_zukunft_ab = 2021,     # RCP: Startjahr >= dieser Wert bleibt
                                obs_alle       = TRUE,     # OBS: alle Zeiträume zeigen
                                out_dir        = "04_results/BAE_Auswertung/heatmap") {

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

  # ---- 2. Szenario / Zeitraum / Variante aus Klimalauf ableiten -------------
  # Zeitraum   : "JJJJ-JJJJ" irgendwo im String
  # Variante   : "vN" irgendwo im String (im Szenario ODER im Modell), sonst NA
  # Szenario   : erstes Token vor "_", ohne evtl. angehängte Variante
  # Szen_label : Szenario inkl. Variante -> eigene Facet-Zeile (RCP45_v2 ...)
  d <- d %>%
    dplyr::mutate(
      Klimalauf = as.character(Klimalauf),
      Zeitraum  = stringr::str_extract(Klimalauf, "\\d{4}-\\d{4}"),
      Variante  = tolower(stringr::str_extract(Klimalauf, "[vV][0-9]+")),
      Szenario  = stringr::str_remove(stringr::str_extract(Klimalauf, "^[^_]+"),
                                      "[-_]?[vV][0-9]+$"),
      Szen_label = ifelse(is.na(Variante), Szenario,
                          paste0(Szenario, "_", Variante)),
      Startjahr  = suppressWarnings(as.integer(stringr::str_sub(Zeitraum, 1, 4)))
    )

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
  is_rcp <- grepl("^RCP", d$Szenario, ignore.case = TRUE)
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

  # ---- 4. Faktor-Ordnung (Facets + Achsen) ----------------------------------
  szen_levels <- sort(unique(d$Szen_label))                 # OBS, RCP45, RCP45_v2, ...
  zeit_levels <- sort(unique(d$Zeitraum))                   # chronologisch (JJJJ-…)
  tv_levels   <- sort(unique(paste0("TV", d$TV)), decreasing = TRUE)  # TV2 oben … TV9 unten
  ba_levels   <- sort(unique(as.character(d$Baumart)))

  d <- d %>%
    dplyr::mutate(
      Szen_label = factor(Szen_label, levels = szen_levels),
      Zeitraum   = factor(Zeitraum,   levels = zeit_levels),
      TV         = factor(paste0("TV", TV), levels = tv_levels),
      Baumart    = factor(Baumart,    levels = ba_levels)
    )

  # ---- 5. Ausgabeordner -----------------------------------------------------
  mid_dir <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- 6. Je Bewertungsstufe eine gefacettete Heatmap -----------------------
  plots <- list()
  for (st in stufen) {
    kat_col <- bae_col[[st]]
    if (is.null(kat_col) || !kat_col %in% names(d)) {
      message("Spalte für Stufe '", st, "' (", kat_col, ") nicht vorhanden – übersprungen.")
      next
    }

    d_st <- d %>%
      dplyr::mutate(
        Kategorie = factor(map_val(.data[[kat_col]], maps[[st]]),
                           levels = names(custom_palette))
      )

    p <- ggplot2::ggplot(d_st, ggplot2::aes(x = Baumart, y = TV, fill = Kategorie)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_manual(values = custom_palette, na.value = "#B0B0B0",
                                 breaks = names(custom_palette), drop = FALSE) +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(
        title    = paste0("BAE-Heatmap – ", master_id),
        subtitle = paste0(st, "-stufig  |  OBS = Vergangenheit, RCP = Zukunft (ab ",
                          rcp_zukunft_ab, ")"),
        x = NULL, y = "TV", fill = "Empfehlung") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        strip.text       = ggplot2::element_text(face = "bold", size = 9),
        axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5,
                                                 face = "bold", size = 9),
        axis.text.y      = ggplot2::element_text(size = 9),
        panel.grid       = ggplot2::element_blank(),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.text      = ggplot2::element_text(size = 10),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA)
      )

    datei <- file.path(mid_dir, paste0("Heatmap_Zukunft_", st, "_", master_id, ".png"))
    ggplot2::ggsave(datei, plot = p, device = "png",
                    width = 5400, height = 3000, units = "px", dpi = 300)
    message("Gespeichert: ", datei)
    plots[[st]] <- p
  }

  invisible(plots)
}

# ============================================================================
# BEISPIEL-AUFRUF (auskommentiert)
# ----------------------------------------------------------------------------
# data <- data.table::fread("meine_bae_daten.csv")   # oder read.csv(...)
#
# # Standard: OBS = Vergangenheit, RCP45/RCP85 nur ab Startjahr 2021
# heatmap_bae_zukunft(data, master_id = "NR_130_08_6189")
#
# # Nur Endperiode 2071-2100 für RCP behalten:
# heatmap_bae_zukunft(data, master_id = "NR_130_08_6189", rcp_zukunft_ab = 2071)
#
# # Nur die 3-stufige Bewertung, eigener Ausgabeordner:
# heatmap_bae_zukunft(data, master_id = "NR_130_08_6189",
#                     stufen = "3st", out_dir = "output/heatmaps")
# ============================================================================
