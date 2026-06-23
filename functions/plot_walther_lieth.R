# =============================================================================
# Walther-Lieth-Klimadiagramm zeichnen (ggplot2)
#
# Konvention:
#   - Temperatur- und Niederschlagsachse gekoppelt: 1 degC : 2 mm (10 degC=20 mm)
#   - bis 100 mm wird 1:2 dargestellt, darueber 1:20 gestaucht und gefuellt
#   - aride Periode  (Temp-Kurve ueber Niederschlag): roter Fuellbereich
#   - humide Periode (Niederschlag ueber Temp):        blauer Fuellbereich
#
# Eingang: zwei Vektoren der Laenge 12 (Jan..Dez): T_mean [degC], P_sum [mm].
# Genau die Werte aus build_walther_lieth_input() je id + Zeitlauf.
#
# Alternative mit identischer Konvention: climatol::diagwl().
# =============================================================================

#' Walther-Lieth-Klimadiagramm fuer einen Punkt/Lauf (ggplot2-Objekt).
#'
#' @param temp   numeric(12): mittlere Monatstemperatur Jan..Dez [degC]
#' @param prec   numeric(12): mittlere Monatsniederschlagssumme Jan..Dez [mm]
#' @param name   Stations-/Punktbezeichnung (Titel)
#' @param alt    Hoehe ue. NN [m] (optional, Untertitel)
#' @param lat,lon Koordinaten (optional, Untertitel)
#' @param period Messzeitraum/Lauf, z.B. "RCP45_MPIWRF_2071-2100"
#' @param mlabels Monatsbeschriftung (Default deutsche Initialen)
#' @param col_temp,col_prec Farben fuer Temperatur bzw. Niederschlag
#' @return ggplot-Objekt
plot_walther_lieth <- function(temp, prec,
                               name = "", alt = NA, lat = NA, lon = NA,
                               period = "",
                               mlabels = c("J","F","M","A","M","J",
                                           "J","A","S","O","N","D"),
                               col_temp = "#c0392b", col_prec = "#2c5fa8") {
  stopifnot(length(temp) == 12, length(prec) == 12)
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE))
    stop("Pakete 'ggplot2' und 'dplyr' werden benoetigt.")
  `%>%` <- dplyr::`%>%`

  temp <- as.numeric(temp); prec <- as.numeric(prec)

  # --- Niederschlag <-> Temperaturachse (1:2 bis 100 mm, dann 1:20) ---------
  p2t <- function(p) ifelse(p <= 100, p / 2, 50 + (p - 100) / 20)
  t2p <- function(t) ifelse(t <= 50, t * 2, 100 + (t - 50) * 20)

  ymax <- max(50, ceiling(max(c(temp, p2t(prec))) / 10) * 10)
  ymin <- min(0,  floor(min(temp) / 10) * 10)

  monthly <- dplyr::tibble(
    month = 1:12, temp = temp, prec = prec, pt = p2t(prec)
  )

  # --- Fein interpolierte Kurven + contiguous-Gruppen fuer die Fuellungen ----
  fine <- dplyr::tibble(x = seq(1, 12, length.out = 12 * 24)) %>%
    dplyr::mutate(
      temp  = stats::approx(1:12, temp, .data$x)$y,
      prec  = stats::approx(1:12, prec, .data$x)$y,
      pt    = p2t(.data$prec),
      humid = .data$pt >= .data$temp,
      # eindeutige Gruppe je zusammenhaengendem humid/arid-Abschnitt
      grp   = cumsum(.data$humid != dplyr::lag(.data$humid,
                                               default = dplyr::first(.data$humid)))
    )
  humid_df    <- dplyr::filter(fine,  .data$humid)
  arid_df     <- dplyr::filter(fine, !.data$humid)
  perhumid_df <- dplyr::filter(fine,  .data$prec > 100)

  ggplot2::ggplot() +
    # humide Flaeche (Niederschlag ueber Temperatur)
    ggplot2::geom_ribbon(
      data = humid_df,
      ggplot2::aes(x = .data$x, ymin = .data$temp, ymax = .data$pt,
                   group = .data$grp),
      fill = col_prec, alpha = 0.30) +
    # aride Flaeche (Temperatur ueber Niederschlag)
    ggplot2::geom_ribbon(
      data = arid_df,
      ggplot2::aes(x = .data$x, ymin = .data$pt, ymax = .data$temp,
                   group = .data$grp),
      fill = col_temp, alpha = 0.22) +
    # perhumid (>100 mm): Bereich oberhalb der 100-mm-Linie solide
    ggplot2::geom_ribbon(
      data = perhumid_df,
      ggplot2::aes(x = .data$x, ymin = 50, ymax = .data$pt, group = .data$grp),
      fill = col_prec, alpha = 0.55) +
    # 100-mm-/1:20-Bruchlinie
    ggplot2::geom_hline(yintercept = 50, colour = "grey80",
                        linetype = "dotted") +
    # Kurven + Punkte
    ggplot2::geom_line(data = monthly,
                       ggplot2::aes(.data$month, .data$temp),
                       colour = col_temp, linewidth = 0.9) +
    ggplot2::geom_line(data = monthly,
                       ggplot2::aes(.data$month, .data$pt),
                       colour = col_prec, linewidth = 0.9) +
    ggplot2::geom_point(data = monthly,
                        ggplot2::aes(.data$month, .data$temp),
                        colour = col_temp, size = 1.4) +
    ggplot2::geom_point(data = monthly,
                        ggplot2::aes(.data$month, .data$pt),
                        colour = col_prec, size = 1.4) +
    # Frostmonate (Mitteltemperatur < 0) als Balken am unteren Rand
    ggplot2::geom_tile(
      data = dplyr::filter(monthly, .data$temp < 0),
      ggplot2::aes(x = .data$month, y = ymin + (ymax - ymin) * 0.01),
      width = 1, height = (ymax - ymin) * 0.02, fill = "#34495e") +
    # Achsen: links Temperatur, rechts Niederschlag (sek. Achse)
    ggplot2::scale_x_continuous(breaks = 1:12, labels = mlabels,
                                expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(
      name = "Temperatur [°C]",
      limits = c(ymin, ymax),
      breaks = scales_pretty(ymin, min(ymax, 50)),
      sec.axis = ggplot2::sec_axis(
        ~ t2p(.), name = "Niederschlag [mm]",
        breaks = c(seq(0, 100, 20),
                   if (ymax > 50) seq(200, t2p(ymax), 100) else NULL))
    ) +
    ggplot2::labs(
      title = name,
      subtitle = wl_subtitle(temp, prec, alt, lat, lon, period)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.y.left  = ggplot2::element_text(colour = col_temp),
      axis.text.y.left   = ggplot2::element_text(colour = col_temp),
      axis.title.y.right = ggplot2::element_text(colour = col_prec),
      axis.text.y.right  = ggplot2::element_text(colour = col_prec),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey25")
    )
}

# Kennwert-Untertitel: Jahresmittel-T, Jahresniederschlag, Meta + Lauf
wl_subtitle <- function(temp, prec, alt, lat, lon, period) {
  parts <- c(
    if (!is.na(alt)) paste0(round(alt), " m"),
    if (!is.na(lat) && !is.na(lon))
      sprintf("(%.3f, %.3f)", lat, lon),
    sprintf("Ø %.1f °C  |  %d mm/a", mean(temp), round(sum(prec))),
    if (nzchar(period)) period
  )
  paste(parts, collapse = "   ·   ")
}

# kleiner Wrapper, damit 'scales' optional bleibt
scales_pretty <- function(lo, hi) {
  if (requireNamespace("scales", quietly = TRUE))
    scales::breaks_pretty()(c(lo, hi)) else pretty(c(lo, hi))
}

# ---- Bequemer Wrapper auf das Long-Format ----------------------------------
#' Diagramm direkt aus dem Long-Format (build_walther_lieth_input) zeichnen.
#'
#' @param df     data.frame mit Spalten id, Zeitlauf, Monat, T_mean, P_sum
#' @param id_val gewuenschte Punkt-id (Klick im Plot)
#' @param run    gewuenschter Zeitlauf
#' @param ...    weitere Argumente an plot_walther_lieth()
#' @return ggplot-Objekt
plot_walther_lieth_from_long <- function(df, id_val, run, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  sub <- df %>%
    dplyr::filter(.data$id == id_val, .data$Zeitlauf == run) %>%
    dplyr::arrange(.data$Monat)
  if (nrow(sub) != 12)
    stop("Erwarte 12 Monatszeilen, gefunden: ", nrow(sub),
         " (id=", id_val, ", Lauf=", run, ").")

  plot_walther_lieth(
    temp = sub$T_mean, prec = sub$P_sum,
    name = paste0("BWI-Punkt ", id_val),
    alt = if ("altitude" %in% names(sub)) sub$altitude[1] else NA,
    lat = if ("Lat" %in% names(sub)) sub$Lat[1] else NA,
    lon = if ("Lon" %in% names(sub)) sub$Lon[1] else NA,
    period = run, ...
  )
}
