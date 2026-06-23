# =============================================================================
# "Walther-Lieth"-artiges 30-Jahres-Verlaufsdiagramm
#
# Nutzt die JAHRES-Parameter (nicht Monate):
#   1049 = Jahresmitteltemperatur  [degC]  (ein Wert je Jahr)
#   1050 = Jahresniederschlagssumme [mm]   (ein Wert je Jahr)
#
# Pro Klimalauf liegen je ~30 Jahreswerte vor. Gezeigt wird der Verlauf ueber
# die Jahre: Temperatur (rot, linke Achse) und Niederschlag (blau, rechte
# Achse) mit linearen Trendlinien. Zwei UNABHAENGIGE Achsen, da die strikte
# 1:2-Kopplung von Walther-Lieth nur fuer Monatswerte sinnvoll ist.
#
# Erwartet, dass walther_lieth_input.R fuer .wl_detect_scale() geladen ist.
# =============================================================================


# ---- 1. Hilfsfunktion: Startjahr aus dem Laufnamen lesen --------------------
# z.B. "RCP45_MPIWRF_2071-2100" -> 2071 ; ohne Treffer NA
wl_start_year <- function(run) {
  treffer <- regmatches(run, regexpr("[0-9]{4}", run))
  if (length(treffer) == 0) NA_integer_ else as.integer(treffer)
}


# ---- 2. Einen Lauf in Long-Format bringen -----------------------------------
# r: SpatRaster mit 2*n Layern. Erste Haelfte = Temp (1049), zweite = Nied. (1050).
#    (Falls die Layernamen "1049"/"1050" bzw. "MAT"/"MAP" enthalten, wird danach
#     getrennt; sonst Annahme erste/zweite Haelfte.)
# Rueckgabe: id | Zeitlauf | Jahr | Kalenderjahr | T_year | P_year
wl_timeseries_one_run <- function(r, run, geom = NULL, scale = NULL) {
  n_layer <- terra::nlyr(r)
  layer_names <- names(r)

  # Temp- und Niederschlags-Layer bestimmen
  is_temp <- grepl("1049|MAT", layer_names)
  is_prec <- grepl("1050|MAP", layer_names)
  if (any(is_temp) && any(is_prec)) {
    idx_temp <- which(is_temp)
    idx_prec <- which(is_prec)
  } else {
    n_years  <- n_layer / 2
    idx_temp <- seq_len(n_years)
    idx_prec <- seq.int(n_years + 1, n_layer)
  }
  if (length(idx_temp) != length(idx_prec))
    stop("Lauf ", run, ": unterschiedlich viele Temp-/Niederschlags-Layer.")
  n_years <- length(idx_temp)

  # Werte je Zelle (Punkt) auslesen
  temp_vals <- terra::as.data.frame(r[[idx_temp]], xy = FALSE)
  prec_vals <- terra::as.data.frame(r[[idx_prec]], xy = FALSE)

  # Skalierung (x10 in den Rohdaten?) pruefen und anwenden
  faktor    <- .wl_detect_scale(as.matrix(temp_vals), scale)
  temp_vals <- temp_vals * faktor
  prec_vals <- prec_vals * faktor

  # Spalten einheitlich als Jahr 1..n benennen
  colnames(temp_vals) <- seq_len(n_years)
  colnames(prec_vals) <- seq_len(n_years)

  # Punkt-id ergaenzen (aus geom, sonst Zellindex)
  if (!is.null(geom) && "id" %in% names(geom)) {
    punkt_id <- geom$id
  } else {
    punkt_id <- seq_len(nrow(temp_vals))
  }
  temp_vals$id <- punkt_id
  prec_vals$id <- punkt_id

  # in Long-Format und beide zusammenfuehren
  temp_long <- tidyr::pivot_longer(temp_vals, -id,
                                   names_to = "Jahr", values_to = "T_year")
  prec_long <- tidyr::pivot_longer(prec_vals, -id,
                                   names_to = "Jahr", values_to = "P_year")
  out <- dplyr::inner_join(temp_long, prec_long, by = c("id", "Jahr"))

  # Jahr als Zahl + echtes Kalenderjahr aus dem Laufnamen
  out$Jahr        <- as.integer(out$Jahr)
  start_year      <- wl_start_year(run)
  out$Kalenderjahr <- if (!is.na(start_year)) start_year + out$Jahr - 1L else NA_integer_
  out$Zeitlauf    <- run
  out
}


# ---- 3. Alle Laeufe zusammenbauen -------------------------------------------
# rast_list: benannte Liste (Name = Zeitlauf), Elemente wie in wl_timeseries_one_run.
# Rueckgabe: ein langes data.frame ueber alle Laeufe und Punkte.
build_wl_timeseries_input <- function(rast_list, geom = NULL, scale = NULL) {
  if (is.null(names(rast_list)) || any(names(rast_list) == ""))
    stop("rast_list muss benannt sein (Name = Zeitlauf).")

  ergebnisse <- list()
  for (run in names(rast_list)) {
    ergebnisse[[run]] <- wl_timeseries_one_run(rast_list[[run]], run,
                                               geom = geom, scale = scale)
  }
  dplyr::bind_rows(ergebnisse)
}


# ---- 4. Diagramm aus Werten zeichnen ----------------------------------------
# year, temp, prec: gleich lange Vektoren (ein Eintrag je Jahr).
# Rueckgabe: ggplot-Objekt.
plot_wl_timeseries <- function(year, temp, prec,
                               name = "", period = "",
                               trend = TRUE,
                               col_temp = "#c0392b", col_prec = "#2c5fa8") {
  stopifnot(length(year) == length(temp), length(temp) == length(prec))

  daten <- dplyr::tibble(year = year, temp = temp, prec = prec)

  # --- zweite y-Achse: Niederschlag linear auf die Temperaturachse abbilden ---
  t_lo <- min(c(temp, 0)); t_hi <- max(temp)
  p_lo <- min(prec);       p_hi <- max(prec)
  spanne_t <- t_hi - t_lo
  spanne_p <- p_hi - p_lo
  if (spanne_t == 0) spanne_t <- 1
  if (spanne_p == 0) spanne_p <- 1

  # Niederschlag -> Temperaturachse und zurueck (fuer sec_axis)
  prec_to_temp <- function(p) (p - p_lo) / spanne_p * spanne_t + t_lo
  temp_to_prec <- function(t) (t - t_lo) / spanne_t * spanne_p + p_lo

  daten$prec_auf_temp <- prec_to_temp(daten$prec)

  # --- Plot schrittweise aufbauen --------------------------------------------
  p <- ggplot2::ggplot(daten, ggplot2::aes(x = year))

  # Niederschlag als helle Balken (rechte Achse)
  p <- p + ggplot2::geom_col(ggplot2::aes(y = prec_auf_temp),
                             fill = col_prec, alpha = 0.30, width = 0.7)

  # Temperatur als Linie + Punkte (linke Achse)
  p <- p + ggplot2::geom_line(ggplot2::aes(y = temp),
                              colour = col_temp, linewidth = 0.9)
  p <- p + ggplot2::geom_point(ggplot2::aes(y = temp),
                               colour = col_temp, size = 1.4)

  # optionale lineare Trendlinien ueber die 30 Jahre
  if (isTRUE(trend)) {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(y = temp),
                                  method = "lm", formula = y ~ x, se = FALSE,
                                  colour = col_temp, linewidth = 0.6,
                                  linetype = "dashed")
    p <- p + ggplot2::geom_smooth(ggplot2::aes(y = prec_auf_temp),
                                  method = "lm", formula = y ~ x, se = FALSE,
                                  colour = col_prec, linewidth = 0.6,
                                  linetype = "dashed")
  }

  # Achsen: links Temperatur, rechts Niederschlag
  p <- p + ggplot2::scale_y_continuous(
    name = "Jahresmitteltemperatur [°C]",
    sec.axis = ggplot2::sec_axis(~ temp_to_prec(.),
                                 name = "Jahresniederschlag [mm]"))

  # Beschriftung + Stil
  untertitel <- paste(c(
    sprintf("Ø %.1f °C  |  %d mm/a", mean(temp), round(mean(prec))),
    if (nzchar(period)) period
  ), collapse = "   ·   ")

  p <- p +
    ggplot2::labs(title = name, subtitle = untertitel, x = "Jahr") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      axis.title.y.left  = ggplot2::element_text(colour = col_temp),
      axis.text.y.left   = ggplot2::element_text(colour = col_temp),
      axis.title.y.right = ggplot2::element_text(colour = col_prec),
      axis.text.y.right  = ggplot2::element_text(colour = col_prec),
      plot.subtitle      = ggplot2::element_text(size = 9, colour = "grey25"))
  p
}


# ---- 5. Bequemer Wrapper auf das Long-Format --------------------------------
# Zeichnet den 30-Jahres-Verlauf fuer einen Punkt + Lauf (Klick im Plot).
plot_wl_timeseries_from_long <- function(df, id_val, run, ...) {
  sub <- df[df$id == id_val & df$Zeitlauf == run, , drop = FALSE]
  sub <- sub[order(sub$Jahr), ]
  if (nrow(sub) == 0)
    stop("Keine Daten fuer id=", id_val, ", Lauf=", run, ".")

  # echte Kalenderjahre nutzen, falls vorhanden
  if (all(!is.na(sub$Kalenderjahr))) {
    x_jahr <- sub$Kalenderjahr
  } else {
    x_jahr <- sub$Jahr
  }

  plot_wl_timeseries(
    year = x_jahr, temp = sub$T_year, prec = sub$P_year,
    name = paste0("BWI-Punkt ", id_val), period = run, ...
  )
}
