# =============================================================================
# Vergleich von WaLi-Trend-Diagrammen (30-Jahres-Verlauf) zwischen Klimalaeufen.
#
#   plot_wali_trend_facets() - Small Multiples (ein Verlauf je Lauf)
#   plot_wali_trend_delta()  - Mittel-Shift je Lauf gegenueber der Referenz
#   compare_wali_trend()     - Wrapper: "facets", "delta" oder "both"
#
# Erwartet geladen: walther_lieth_helpers.R (Farben, wl_* Helfer)
# Eingang: Long-Format wie aus build_wali_trend_input()
#          (id | Zeitlauf | Jahr | Kalenderjahr | T_year | P_year).
#
# Hinweis: Die Laeufe koennen verschiedene Perioden abdecken (z.B. 1991-2020
# vs. 2071-2100). Ein jahrweises Delta waere dann nicht alignbar - deshalb
# zeigt das Delta hier den MITTLEREN Shift (\u00d8 \u0394T, \u00d8 \u0394P) je Lauf vs. Referenz.
# =============================================================================


# ---- 1. Small Multiples: ein Verlauf je Lauf, gemeinsame Skalierung ---------
plot_wali_trend_facets <- function(df, id_val, runs = NULL, ncol = NULL,
                                   trend = TRUE) {
  d <- df[df$id == id_val, ]
  if (!is.null(runs)) d <- d[d$Zeitlauf %in% runs, ]
  if (nrow(d) == 0) stop("Keine Daten fuer id=", id_val, ".")
  runs <- if (is.null(runs)) unique(d$Zeitlauf) else runs

  # x = Kalenderjahr falls vorhanden, sonst Jahresindex
  jahr <- if (all(!is.na(d$Kalenderjahr))) d$Kalenderjahr else d$Jahr

  daten <- dplyr::tibble(
    Zeitlauf = wl_run_factor(d$Zeitlauf, runs),
    jahr = jahr, temp = d$T_year, prec = d$P_year
  )

  # GEMEINSAME lineare Abbildung Niederschlag -> Temperaturachse ueber alle
  # Laeufe (damit die Facets vergleichbar sind)
  t_lo <- min(c(daten$temp, 0)); t_hi <- max(daten$temp)
  p_lo <- min(daten$prec);       p_hi <- max(daten$prec)
  spanne_t <- max(t_hi - t_lo, 1)
  spanne_p <- max(p_hi - p_lo, 1)
  prec_to_temp <- function(p) (p - p_lo) / spanne_p * spanne_t + t_lo
  temp_to_prec <- function(t) (t - t_lo) / spanne_t * spanne_p + p_lo

  daten$prec_auf_temp <- prec_to_temp(daten$prec)

  p <- ggplot2::ggplot(daten, ggplot2::aes(x = jahr)) +
    ggplot2::geom_col(ggplot2::aes(y = prec_auf_temp),
                      fill = COL_PREC, alpha = 0.30, width = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = temp), colour = COL_TEMP, linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = temp), colour = COL_TEMP, size = 1.1)

  if (isTRUE(trend)) {
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(y = temp), method = "lm",
                           formula = y ~ x, se = FALSE,
                           colour = COL_TEMP, linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_smooth(ggplot2::aes(y = prec_auf_temp), method = "lm",
                           formula = y ~ x, se = FALSE,
                           colour = COL_PREC, linewidth = 0.5, linetype = "dashed")
  }

  p +
    ggplot2::facet_wrap(~ Zeitlauf, ncol = ncol, scales = "free_x") +
    ggplot2::scale_y_continuous(
      name = "Jahresmitteltemperatur [\u00b0C]",
      sec.axis = ggplot2::sec_axis(~ temp_to_prec(.), name = "Jahresniederschlag [mm]")) +
    ggplot2::labs(title = paste0("WaLi-Trend-Vergleich \u00b7 Punkt ", id_val),
                  x = "Jahr") +
    wl_compare_theme()
}


# ---- 2. Delta: mittlerer Shift je Lauf gegenueber der Referenz --------------
# Balken je Vergleichslauf: \u0394T [\u00b0C] (links/rot) und \u0394P [mm/a] (rechts/blau),
# als Mittelwert-Differenz zur Referenzperiode.
plot_wali_trend_delta <- function(df, id_val, run_ref, runs = NULL) {
  d <- df[df$id == id_val, ]
  runs <- if (is.null(runs)) unique(d$Zeitlauf) else runs
  cmps <- setdiff(runs, run_ref)

  mittel <- function(run) c(T = mean(d$T_year[d$Zeitlauf == run]),
                            P = mean(d$P_year[d$Zeitlauf == run]))
  ref <- mittel(run_ref)

  delta <- dplyr::tibble(
    Zeitlauf = wl_run_factor(cmps, cmps),
    dT = vapply(cmps, function(r) mittel(r)["T"] - ref["T"], numeric(1)),
    dP = vapply(cmps, function(r) mittel(r)["P"] - ref["P"], numeric(1))
  )

  # \u0394P auf die \u0394T-Achse skalieren (eigene, datengerechte Skala)
  faktor <- if (max(abs(delta$dT)) > 0)
    max(abs(delta$dP)) / max(abs(delta$dT)) else 1
  delta$dP_auf_temp <- delta$dP / faktor

  # lange Form fuer gruppierte Balken (Temp + Niederschlag nebeneinander)
  lang <- dplyr::bind_rows(
    dplyr::tibble(Zeitlauf = delta$Zeitlauf, groesse = "\u0394Temperatur",
                  wert = delta$dT, y = delta$dT),
    dplyr::tibble(Zeitlauf = delta$Zeitlauf, groesse = "\u0394Niederschlag",
                  wert = delta$dP, y = delta$dP_auf_temp)
  )

  ggplot2::ggplot(lang, ggplot2::aes(x = Zeitlauf, y = y, fill = groesse)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                      width = 0.6, alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(groesse == "\u0394Temperatur",
                                                   sprintf("%+.1f \u00b0C", wert),
                                                   sprintf("%+d mm", round(wert)))),
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = c("\u0394Temperatur" = COL_TEMP,
                                          "\u0394Niederschlag" = COL_PREC), name = NULL) +
    ggplot2::scale_y_continuous(
      name = "\u0394Temperatur [\u00b0C]",
      sec.axis = ggplot2::sec_axis(~ . * faktor, name = "\u0394Niederschlag [mm/a]")) +
    ggplot2::labs(title = paste0("WaLi-Trend Mittel-Shift \u00b7 Punkt ", id_val),
                  subtitle = paste0("Differenz zur Referenz: ", run_ref),
                  x = NULL) +
    wl_compare_theme()
}


# ---- 3. Wrapper -------------------------------------------------------------
# Default "facets": die Mittel-Shift-Leiste (delta) ist redundant - die Δ-Info
# liest man direkt aus dem Vergleich bzw. dem Zeitstrahl (plot_wali_timeline()).
# delta bleibt per mode="delta"/"both" verfuegbar.
compare_wali_trend <- function(df, id_val, runs = NULL,
                               mode = c("facets", "both", "delta"),
                               ref = NULL) {
  mode <- match.arg(mode)
  d_runs <- if (is.null(runs)) unique(df$Zeitlauf[df$id == id_val]) else runs
  if (is.null(ref)) ref <- d_runs[1]

  if (mode == "facets")
    return(plot_wali_trend_facets(df, id_val, runs = d_runs))
  if (mode == "delta")
    return(plot_wali_trend_delta(df, id_val, ref, runs = d_runs))

  oben  <- plot_wali_trend_facets(df, id_val, runs = d_runs)
  unten <- plot_wali_trend_delta(df, id_val, ref, runs = d_runs)
  wl_patch(list(oben, unten), ncol = 1, heights = c(2, 1.2))
}
