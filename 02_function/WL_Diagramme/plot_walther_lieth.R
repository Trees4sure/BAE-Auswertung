# =============================================================================
# Walther-Lieth-Klimadiagramm (ggplot2), an der klassischen Darstellung
# orientiert (vgl. climatol::diagwl / Lehrbuch):
#
#   - 4-Ecken-Kopf: Name + Hoehe/Laenge/Breite (links), Periode + Mittel (rechts)
#   - Max/Min-Temperatur oben im Diagramm
#   - humide Periode  (Niederschlag > Temperatur):  blaue Senkrechtschraffur
#   - aride  Periode  (Temperatur > Niederschlag):  rote Punktschraffur (gepunktet)
#   - perhumide Periode (> 100 mm):                 volle blaue Flaeche
#   - Frostmonate (Mitteltemp < 0):                 dunkle Balken am Boden
#   - Achsenkopplung 1 degC : 2 mm, ab 100 mm 1:20
#
# Sonderzeichen bewusst als \u-Escapes (z.B. Grad-Zeichen) -> kein Mojibake,
# egal welches Source-Encoding. Das war im alten Output das Mojibake-Problem (/).
#
# Eingang: temp[12], prec[12] (Jan..Dez). Rueckgabe: ggplot/patchwork-Objekt.
# =============================================================================

GRAD   <- "\u00b0C"            # Grad Celsius (ASCII-Escape -> kein Mojibake)
MLABEL <- c("J","F","M","A","M","J","J","A","S","O","N","D")
ROT    <- "#c0392b"
BLAU   <- "#2c5fa8"


# ---- Niederschlag <-> Temperaturachse (1:2 bis 100 mm, dann 1:20) -----------
wl_p2t <- function(p) ifelse(p <= 100, p / 2, 50 + (p - 100) / 20)
wl_t2p <- function(t) ifelse(t <= 50, t * 2, 100 + (t - 50) * 20)


# ---- 1. Kopfzeile (4 Ecken) als eigenes Mini-Plot ---------------------------
# Links: Name + Standort.  Rechts: Periode + Jahresmittel.
wl_header <- function(name, elevation, lon, lat, period, mat, map) {
  links <- paste0(
    "H\u00f6he: ",   if (is.na(elevation)) "?" else paste0(round(elevation), " m"), "\n",
    "L\u00e4nge: ",  if (is.na(lon)) "?" else sprintf("%.3f", lon), "\n",
    "Breite: ",      if (is.na(lat)) "?" else sprintf("%.3f", lat))
  rechts <- paste0(
    "Mittl. Lufttemperatur: ", sprintf("%.1f %s", mat, GRAD), "\n",
    "Jahresniederschlag: ",    sprintf("%d mm", as.integer(round(map))))

  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 1, hjust = 0, vjust = 1,
                      label = name, fontface = "bold", size = 5) +
    ggplot2::annotate("text", x = 0, y = 0.55, hjust = 0, vjust = 1,
                      label = links, size = 3.2, lineheight = 0.95) +
    ggplot2::annotate("text", x = 1, y = 1, hjust = 1, vjust = 1,
                      label = period, fontface = "bold", size = 4) +
    ggplot2::annotate("text", x = 1, y = 0.55, hjust = 1, vjust = 1,
                      label = rechts, size = 3.2, lineheight = 0.95) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}


# ---- 2. Hauptdiagramm -------------------------------------------------------
wl_panel <- function(temp, prec, t_abs_max = NA, t_abs_min = NA) {
  monate  <- 1:12
  monthly <- data.frame(month = monate, temp = temp, pt = wl_p2t(prec))

  # Feine Interpolation fuer Schraffuren / Fuellung
  xf  <- seq(1, 12, length.out = 12 * 24)
  tf  <- stats::approx(monate, temp, xf)$y
  ptf <- wl_p2t(stats::approx(monate, prec, xf)$y)
  fine <- data.frame(x = xf, tf = tf, ptf = ptf, humid = ptf >= tf)

  # Schraffur-Linien (ausgeduennt, damit es nach Schraffur aussieht)
  hatch   <- fine[seq(1, nrow(fine), by = 3), ]
  humid_h <- hatch[hatch$humid, ]
  arid_h  <- hatch[!hatch$humid, ]

  # Perhumide Flaeche (> 100 mm -> pt > 50), zusammenhaengende Gruppen
  fine$wet <- fine$ptf > 50
  fine$grp <- cumsum(c(TRUE, diff(fine$wet) != 0))
  wet <- fine[fine$wet, ]

  # Achsengrenzen: oben WL-typisch, unten ein schmales Band fuer Frostbalken
  ymax <- max(50, ceiling(max(c(monthly$temp, monthly$pt)) / 10) * 10)
  ymin <- min(0, floor(min(temp))) - 3

  frost <- data.frame(month = which(temp < 0))

  rechts_breaks <- c(seq(0, 100, 20),
                     if (ymax > 50) seq(200, wl_t2p(ymax), 100) else NULL)

  farben <- c("Lufttemperatur" = ROT, "Niederschlag" = BLAU,
              "humide Periode" = BLAU, "aride Periode" = ROT)

  p <- ggplot2::ggplot()
  # perhumide Flaeche (nur falls es Monate > 100 mm gibt)
  if (nrow(wet) > 0)
    p <- p + ggplot2::geom_ribbon(data = wet,
      ggplot2::aes(x = x, ymin = 50, ymax = ptf, group = grp,
                   fill = "perhumide Periode (> 100 mm)"))
  # humide Senkrechtschraffur (blau)
  if (nrow(humid_h) > 0)
    p <- p + ggplot2::geom_segment(data = humid_h,
      ggplot2::aes(x = x, xend = x, y = tf, yend = ptf, colour = "humide Periode"))
  # aride Punktschraffur (rot, gepunktet)
  if (nrow(arid_h) > 0)
    p <- p + ggplot2::geom_segment(data = arid_h,
      ggplot2::aes(x = x, xend = x, y = ptf, yend = tf, colour = "aride Periode"),
      linetype = "dotted")
  # Bruchlinie + Kurven + Punkte
  p <- p +
    ggplot2::geom_hline(yintercept = 50, colour = "grey75", linetype = "dashed") +
    ggplot2::geom_line(data = monthly,
      ggplot2::aes(month, pt, colour = "Niederschlag"), linewidth = 0.8) +
    ggplot2::geom_line(data = monthly,
      ggplot2::aes(month, temp, colour = "Lufttemperatur"), linewidth = 0.8) +
    ggplot2::geom_point(data = monthly, ggplot2::aes(month, temp),
                        colour = ROT, size = 1.3) +
    ggplot2::geom_point(data = monthly, ggplot2::aes(month, pt),
                        colour = BLAU, size = 1.3)

  # Frostbalken am unteren Rand
  if (nrow(frost) > 0)
    p <- p + ggplot2::geom_rect(data = frost,
      ggplot2::aes(xmin = month - 0.5, xmax = month + 0.5,
                   ymin = ymin + 0.2, ymax = ymin + 1.2),
      fill = "#34495e")

  # Max/Min-Temperatur oben (falls vorhanden)
  if (!is.na(t_abs_max))
    p <- p + ggplot2::annotate("text", x = 1, y = ymax, hjust = 0, vjust = 1,
              size = 3, colour = ROT, label = sprintf("Max: %.1f %s", t_abs_max, GRAD))
  if (!is.na(t_abs_min))
    p <- p + ggplot2::annotate("text", x = 1, y = ymax - ymax * 0.08, hjust = 0,
              vjust = 1, size = 3, colour = BLAU,
              label = sprintf("Min: %.1f %s", t_abs_min, GRAD))

  # Legende dynamisch: nur tatsaechlich vorhandene Kategorien (sonst Fehler,
  # weil override.aes sonst mehr Eintraege als Legenden-Keys haette)
  present <- c("Lufttemperatur", "Niederschlag")
  if (nrow(humid_h) > 0) present <- c(present, "humide Periode")
  if (nrow(arid_h)  > 0) present <- c(present, "aride Periode")
  linientyp <- ifelse(present == "aride Periode", "dotted", "solid")

  p <- p +
    ggplot2::scale_colour_manual(name = NULL, limits = present, breaks = present,
                                 values = farben) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(linetype = linientyp))) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = MLABEL,
                                expand = c(0.01, 0.01)) +
    ggplot2::scale_y_continuous(
      name = paste0("Mitteltemperatur [", GRAD, "]"),
      limits = c(ymin, ymax), breaks = seq(0, min(ymax, 50), 10),
      sec.axis = ggplot2::sec_axis(~ wl_t2p(.),
                                   name = "Niederschlagssumme [mm]",
                                   breaks = rechts_breaks)) +
    ggplot2::labs(x = "Monate  (Balken unten: Frostmonate)") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.y.left  = ggplot2::element_text(colour = ROT),
      axis.text.y.left   = ggplot2::element_text(colour = ROT),
      axis.title.y.right = ggplot2::element_text(colour = BLAU),
      axis.text.y.right  = ggplot2::element_text(colour = BLAU),
      legend.position    = "bottom")

  # perhumide Fuellung nur skalieren, wenn sie vorkommt (sonst Warnung)
  if (nrow(wet) > 0)
    p <- p + ggplot2::scale_fill_manual(name = NULL,
              values = c("perhumide Periode (> 100 mm)" = BLAU))
  p
}


# ---- 3. Kopf + Diagramm zusammensetzen --------------------------------------
#' Walther-Lieth-Klimadiagramm fuer einen Punkt/Lauf.
#'
#' @param temp,prec numeric(12) Jan..Dez: Mitteltemperatur [degC] / Summe [mm]
#' @param name      Stations-/Punktname
#' @param elevation,lon,lat  Standort (Kopfzeile)
#' @param period    Messzeitraum/Lauf
#' @param t_abs_max,t_abs_min  absolute Extremtemperaturen (optional)
#' @return ggplot-/patchwork-Objekt
plot_walther_lieth <- function(temp, prec, name = "",
                               elevation = NA, lon = NA, lat = NA,
                               period = "", t_abs_max = NA, t_abs_min = NA) {
  stopifnot(length(temp) == 12, length(prec) == 12)
  temp <- as.numeric(temp); prec <- as.numeric(prec)

  panel <- wl_panel(temp, prec, t_abs_max, t_abs_min)

  # Kopf nur, wenn patchwork da ist; sonst einfacher Titel als Fallback
  if (requireNamespace("patchwork", quietly = TRUE)) {
    kopf <- wl_header(name, elevation, lon, lat, period, mean(temp), sum(prec))
    patchwork::wrap_plots(kopf, panel, ncol = 1, heights = c(1, 5))
  } else {
    panel + ggplot2::labs(
      title = name,
      subtitle = sprintf("%.1f %s | %d mm/a  %s",
                         mean(temp), GRAD, as.integer(round(sum(prec))), period))
  }
}


# ---- 4. Bequemer Wrapper auf das Long-Format --------------------------------
#' Diagramm direkt aus dem Long-Format (build_walther_lieth_input) zeichnen.
#'
#' @param id_col Schluesselspalte fuer id_val (Default "id"). Die abgelegten
#'   App-Daten sind nach MASTER_ID gekeyt -> dann id_col = "MASTER_ID".
#' @param name   optionaler Diagramm-Titel; NULL = "Punkt <id_val>".
plot_walther_lieth_from_long <- function(df, id_val, run, id_col = "id",
                                         name = NULL, ...) {
  if (!id_col %in% names(df))
    stop("Spalte '", id_col, "' fehlt im df (vorhanden: ",
         paste(names(df), collapse = ", "), "). id_col= passend setzen ",
         "(App-Daten sind nach 'MASTER_ID' gekeyt).")
  sub <- df[df[[id_col]] == id_val & df$Zeitlauf == run, , drop = FALSE]
  sub <- sub[order(sub$Monat), ]
  if (nrow(sub) != 12)
    stop("Erwarte 12 Monatszeilen, gefunden: ", nrow(sub),
         " (", id_col, "=", id_val, ", Lauf=", run, ").")

  hole <- function(spalte) if (spalte %in% names(sub)) sub[[spalte]][1] else NA
  plot_walther_lieth(
    temp = sub$T_mean, prec = sub$P_sum,
    name = if (is.null(name)) paste0("Punkt ", id_val) else name,
    elevation = hole("altitude"), lon = hole("Lon"), lat = hole("Lat"),
    period = run, ...
  )
}
