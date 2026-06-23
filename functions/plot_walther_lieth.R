# =============================================================================
# Walther-Lieth-Klimadiagramm zeichnen (Base R, ohne Zusatzpakete)
#
# Konvention:
#   - Temperatur- und Niederschlagsachse gekoppelt: 1 degC : 2 mm (10 degC=20 mm)
#   - bis 100 mm wird 1:2 dargestellt, darueber 1:20 gestaucht und gefuellt
#   - aride Periode  (Temp-Kurve ueber Niederschlag): roter Punktraster
#   - humide Periode (Niederschlag ueber Temp):        blaue Schraffur
#
# Eingang: zwei Vektoren der Laenge 12 (Jan..Dez): T_mean [degC], P_sum [mm].
# Genau die Werte aus build_walther_lieth_input() je id + Zeitlauf.
#
# Alternative: das Paket 'climatol' bietet mit climatol::diagwl() eine
# etablierte Implementierung; diese hier ist dependency-frei fuer den
# Klick-im-Plot-Anwendungsfall.
# =============================================================================

#' Walther-Lieth-Klimadiagramm fuer einen Punkt/Lauf.
#'
#' @param temp   numeric(12): mittlere Monatstemperatur Jan..Dez [degC]
#' @param prec   numeric(12): mittlere Monatsniederschlagssumme Jan..Dez [mm]
#' @param name   Stations-/Punktbezeichnung (Diagrammtitel)
#' @param alt    Hoehe ue. NN [m] (optional, Kopfzeile)
#' @param lat,lon Koordinaten (optional, Kopfzeile)
#' @param period Messzeitraum/Lauf, z.B. "RCP45_MPIWRF_2071-2100" (Kopfzeile)
#' @param mlabels Monatsbeschriftung (Default deutsche Initialen)
#' @param col_temp,col_prec Linienfarben
#' @return (unsichtbar) Liste mit abgeleiteten Kennwerten (Jahresmittel etc.)
plot_walther_lieth <- function(temp, prec,
                               name = "", alt = NA, lat = NA, lon = NA,
                               period = "",
                               mlabels = c("J","F","M","A","M","J",
                                           "J","A","S","O","N","D"),
                               col_temp = "#c0392b", col_prec = "#2c5fa8") {
  stopifnot(length(temp) == 12, length(prec) == 12)
  temp <- as.numeric(temp); prec <- as.numeric(prec)

  # --- Niederschlag auf die Temperaturachse abbilden ------------------------
  # bis 100 mm:  t = p/2 ;  darueber: t = 50 + (p-100)/20
  p2t <- function(p) ifelse(p <= 100, p / 2, 50 + (p - 100) / 20)
  # Umkehrung fuer die rechte Achsenbeschriftung
  t2p <- function(t) ifelse(t <= 50, t * 2, 100 + (t - 50) * 20)

  x  <- 1:12
  pt <- p2t(prec)                       # Niederschlag in Temp-Achseneinheiten

  # --- Achsengrenzen --------------------------------------------------------
  ymax <- max(50, ceiling(max(c(temp, pt)) / 10) * 10)
  ymin <- min(0, floor(min(temp) / 10) * 10)

  # --- Plotgeruest ----------------------------------------------------------
  op <- graphics::par(mar = c(3.5, 4, 4, 4) + 0.1, xpd = FALSE)
  on.exit(graphics::par(op), add = TRUE)

  plot(NA, xlim = c(1, 12), ylim = c(ymin, ymax),
       axes = FALSE, xlab = "", ylab = "")

  # --- Fein interpolierte Kurven fuer die Flaechenfuellung ------------------
  xf <- seq(1, 12, length.out = 12 * 24)
  tf <- stats::approx(x, temp, xf)$y
  pf <- stats::approx(x, prec, xf)$y
  ptf <- p2t(pf)
  base0 <- max(ymin, 0)                 # Fuellungen ab 0 (bzw. ymin falls >0)

  for (i in seq_along(xf)) {
    lo <- min(tf[i], ptf[i]); hi <- max(tf[i], ptf[i])
    if (ptf[i] >= tf[i]) {
      # humid: blaue Schraffur zwischen Temp- und Niederschlagskurve
      graphics::segments(xf[i], lo, xf[i], hi, col = col_prec, lwd = 1)
      # perhumid (>100 mm): Bereich oberhalb 50 zusaetzlich solide fuellen
      if (ptf[i] > 50)
        graphics::segments(xf[i], 50, xf[i], ptf[i],
                           col = grDevices::adjustcolor(col_prec, 0.6), lwd = 1)
    } else {
      # arid: roter Punktraster zwischen Niederschlags- und Temp-Kurve
      graphics::segments(xf[i], lo, xf[i], hi,
                         col = grDevices::adjustcolor(col_temp, 0.30), lwd = 1)
    }
  }

  # --- Kurven ---------------------------------------------------------------
  graphics::lines(x, temp, col = col_temp, lwd = 2, type = "l")
  graphics::lines(x, pt,   col = col_prec, lwd = 2, type = "l")
  graphics::points(x, temp, col = col_temp, pch = 16, cex = 0.6)
  graphics::points(x, pt,   col = col_prec, pch = 16, cex = 0.6)

  # --- Frosthinweis: Monate mit Mitteltemperatur < 0 ------------------------
  frost <- which(temp < 0)
  if (length(frost) > 0)
    graphics::rect(frost - 0.5, ymin, frost + 0.5, ymin + (ymax - ymin) * 0.02,
                   col = "#34495e", border = NA)

  # --- Achsen ---------------------------------------------------------------
  graphics::axis(1, at = x, labels = mlabels, tick = TRUE, cex.axis = 0.9)
  # links: Temperatur
  t_ticks <- pretty(c(ymin, min(ymax, 50)))
  graphics::axis(2, at = t_ticks, labels = t_ticks, las = 1,
                 col.axis = col_temp, cex.axis = 0.9)
  graphics::mtext("Temperatur [°C]", side = 2, line = 2.5, col = col_temp)
  # rechts: Niederschlag (0..100 mm im 1:2-Bereich, dann 1:20)
  pr_low  <- seq(0, 100, by = 20)
  pr_high <- if (ymax > 50) seq(200, t2p(ymax), by = 100) else numeric(0)
  pr_vals <- c(pr_low, pr_high)
  graphics::axis(4, at = p2t(pr_vals), labels = pr_vals, las = 1,
                 col.axis = col_prec, cex.axis = 0.9)
  graphics::mtext("Niederschlag [mm]", side = 4, line = 2.5, col = col_prec)
  graphics::abline(h = 50, col = "grey80", lty = 3)   # 100-mm-/1:20-Bruchlinie
  graphics::box()

  # --- Kopfzeile / Kennwerte ------------------------------------------------
  mat <- mean(temp)                  # Jahresmitteltemperatur
  map <- sum(prec)                   # Jahresniederschlagssumme
  sub <- paste0(
    if (!is.na(alt)) paste0(round(alt), " m  ") else "",
    if (!is.na(lat) && !is.na(lon))
      paste0("(", round(lat, 3), ", ", round(lon, 3), ")  ") else "",
    sprintf("%.1f °C  |  %d mm", mat, round(map))
  )
  graphics::title(main = name, line = 2.4, cex.main = 1.1)
  graphics::mtext(period, side = 3, line = 1.2, cex = 0.85, col = "grey30")
  graphics::mtext(sub,    side = 3, line = 0.2, cex = 0.85)

  invisible(list(MAT = mat, MAP = map,
                 arid_months = which(pt < temp),
                 frost_months = frost))
}

# ---- Bequemer Wrapper auf das Long-Format ----------------------------------
#' Diagramm direkt aus dem Long-Format (build_walther_lieth_input) zeichnen.
#'
#' @param df    data.frame mit Spalten id, Zeitlauf, Monat, T_mean, P_sum
#' @param id_val   gewuenschte Punkt-id (Klick im Plot)
#' @param run      gewuenschter Zeitlauf
#' @param ...      weitere Argumente an plot_walther_lieth (name, alt, ...)
plot_walther_lieth_from_long <- function(df, id_val, run, ...) {
  sub <- df[df$id == id_val & df$Zeitlauf == run, , drop = FALSE]
  sub <- sub[order(sub$Monat), ]
  if (nrow(sub) != 12)
    stop("Erwarte 12 Monatszeilen, gefunden: ", nrow(sub),
         " (id=", id_val, ", Lauf=", run, ").")
  alt <- if ("altitude" %in% names(sub)) sub$altitude[1] else NA
  lat <- if ("Lat" %in% names(sub)) sub$Lat[1] else NA
  lon <- if ("Lon" %in% names(sub)) sub$Lon[1] else NA
  plot_walther_lieth(
    temp = sub$T_mean, prec = sub$P_sum,
    name = paste0("BWI-Punkt ", id_val),
    alt = alt, lat = lat, lon = lon, period = run, ...
  )
}
