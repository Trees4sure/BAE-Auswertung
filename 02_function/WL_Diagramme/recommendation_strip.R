# =============================================================================
# Baumartenempfehlungs-Leiste: macht den Wechsel der Empfehlungsstufe je
# Baumart zwischen den Klimalaeufen sichtbar (sehr empfohlen ... nicht empf.).
#
#   plot_recommendation_strip()       - farbcodierte Leiste (Art x Lauf)
#   combine_climate_recommendation()  - Klimadiagramm + Leiste stapeln
#
# Erwartet geladen: walther_lieth_helpers.R (wl_rec_*, wl_patch).
# Eingang: id | Zeitlauf | Baumart | Empfehlung
# =============================================================================

# Kurzlabels fuer die Kacheln
REC_KURZ <- c("sehr empfohlen" = "sehr",
              "empfohlen" = "empf.",
              "bedingt empfohlen" = "bedingt",
              "nicht empfohlen" = "nicht")


# ---- 1. Empfehlungs-Leiste --------------------------------------------------
# Zwei Layouts:
#   facets = FALSE (Default): Laeufe als Spalten, Baumarten als Zeilen
#       -> kompakte Matrix; Stufenwechsel = Farbwechsel entlang der Zeile.
#   facets = TRUE: ein schmales Kachel-Panel JE LAUF (facet_wrap, eine Reihe)
#       -> deckungsgleich mit den Klima-Small-Multiples, damit jede Lauf-Spalte
#          exakt unter ihrem Klimadiagramm sitzt (siehe combine_..., aligned).
plot_recommendation_strip <- function(rec_df, id_val, runs = NULL,
                                      arten = NULL, facets = FALSE) {
  d <- rec_df[rec_df$id == id_val, ]
  if (!is.null(runs))  d <- d[d$Zeitlauf %in% runs, ]
  if (!is.null(arten)) d <- d[d$Baumart %in% arten, ]
  if (nrow(d) == 0) stop("Keine Empfehlungen fuer id=", id_val, ".")

  runs <- if (is.null(runs)) unique(d$Zeitlauf) else runs
  d$Zeitlauf   <- wl_run_factor(d$Zeitlauf, runs)
  d$Empfehlung <- wl_as_rec_factor(d$Empfehlung)
  d$label      <- REC_KURZ[as.character(d$Empfehlung)]

  if (facets) {
    # x konstant -> in jedem Facet eine Kachelspalte (eine Kachel je Baumart)
    d$x <- ""
    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = Baumart, fill = Empfehlung)) +
      ggplot2::geom_tile(colour = "white", linewidth = 1) +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 2.8, colour = "grey15") +
      ggplot2::facet_wrap(~ Zeitlauf, nrow = 1) +
      ggplot2::labs(x = NULL, y = NULL)
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = Zeitlauf, y = Baumart, fill = Empfehlung)) +
      ggplot2::geom_tile(colour = "white", linewidth = 1) +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 3, colour = "grey15") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(title = paste0("Baumartenempfehlung \u00b7 Punkt ", id_val),
                    x = NULL, y = NULL)
  }

  p +
    ggplot2::scale_fill_manual(values = wl_rec_colors, drop = FALSE,
                               name = "Empfehlung") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_text(face = "bold"))
}


# ---- 2. Klimadiagramm + Empfehlungs-Leiste stapeln --------------------------
# climate_plot: ein WL-/WaLi-Trend-Vergleichsplot.
# aligned = TRUE  -> facettierte Leiste; jede Lauf-Spalte sitzt deckungsgleich
#                    unter ihrem Klimadiagramm. Nur sinnvoll, wenn climate_plot
#                    ebenfalls je Lauf facettiert ist (plot_*_facets()).
# aligned = FALSE -> kompakte Matrix-Leiste darunter (Laeufe als Spalten).
combine_climate_recommendation <- function(climate_plot, rec_df, id_val,
                                           runs = NULL, aligned = TRUE,
                                           heights = c(3, 1.4)) {
  strip <- plot_recommendation_strip(rec_df, id_val, runs = runs,
                                     facets = aligned)
  wl_patch(list(climate_plot, strip), ncol = 1, heights = heights)
}
