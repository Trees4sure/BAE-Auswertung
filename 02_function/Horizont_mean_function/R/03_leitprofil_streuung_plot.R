# =====================================================================
# 03_leitprofil_streuung_plot.R
# ---------------------------------------------------------------------
# Tiefen-orientierte Darstellung der Leitprofil-Kennwerte, passend zum
# aqp-Horizontprofil:
#   - Kornanteile (Sand/Schluff/Ton) als gestapelte Zusammensetzung je
#     Horizont in Erdtoenen  -> gemeinsame 0-100 %-Skala
#   - Chemie/Speicher (BASEN, NFK) GETRENNT in einem eigenen Panel mit
#     freier x-Achse, damit die unterschiedlichen Einheiten nicht auf
#     einer Achse kollidieren (das war die Quelle der "komischen Werte").
#
# Bewusst KEINE geom_smooth-Glaettung: bei nur wenigen Horizonten
# ueberschwingt Loess (Anteile <0 / >100). Stattdessen die echten
# Horizont-Tiefenintervalle als Rechtecke/Punkte -> deckt sich exakt
# mit dem aqp-Profil daneben.
#
# Einstiege:
#   leitprofil_kennwerte()      -> Aggregation je HORIZONT (dplyr)
#   leitprofil_streuung_plot()  -> eigenstaendige Abbildung        (Variante 2)
#   leitprofil_linien_plot()    -> Linien ueber die Tiefe, linear  (Variante 2b)
#   horizont_mit_streuung()     -> aqp-Profil + Balken seitlich    (Variante 1)
#   horizont_mit_linien()       -> aqp-Profil + Linien seitlich    (Variante 1b)
#
# Die bestehende horizont_abfolge_plot() wird NICHT veraendert; Variante 1
# ruft sie unveraendert auf und stellt das Ergebnis nur daneben.
#
# Benoetigte Pakete: ggplot2, dplyr, tidyr, patchwork, cowplot
#   (zusaetzlich zu aqp/dplyr aus 01_db_zugriff.R / 02_horizont_abfolge_plot.R)
# Reihenfolge beim Sourcen: erst 00_.., 01_.., 02_.., dann diese Datei.
# =====================================================================

# ---- Erdton-Palette (Kornfraktionen) --------------------------------
# Sand hell (sandgelb) -> Schluff ocker -> Ton rotbraun.
.ERDTOENE <- c(Sand = "#E4C68C", Schluff = "#B98A4E", Ton = "#7C4A2D")


# ---------------------------------------------------------------------
# 1) Kennwerte je HORIZONT aggregieren (ein Tiefenband je Horizont)
# ---------------------------------------------------------------------
#' @param data_input  Leitprofildaten (aus lade_leitprofile())
#' @param soeh_krz    Feinbodenform-Kuerzel (ein oder mehrere; Kombiformen
#'                    werden ueber .resolve_soeh_krz auf Teilformen aufgeloest)
#' @param region      optionaler BL-Filter (z.B. "MV"); NULL = alle Regionen
#' @return  tibble je HORIZONT: TIEFE_OG/UG (gemittelt) + Kornanteile + Chemie
leitprofil_kennwerte <- function(data_input, soeh_krz, region = NULL) {

  df <- data_input
  if (!is.null(region) && "BL" %in% names(df)) df <- dplyr::filter(df, BL == region)

  # Kombiform-Fallback nutzen, falls 02_.. geladen ist; sonst direkt filtern.
  codes <- if (exists(".resolve_soeh_krz")) .resolve_soeh_krz(df, soeh_krz) else soeh_krz
  df <- dplyr::distinct(dplyr::filter(df, SOEH_KRZ %in% codes))
  if (nrow(df) == 0)
    stop("Keine Leitprofil-Zeilen fuer SOEH_KRZ = ", paste(soeh_krz, collapse = ", "),
         if (!is.null(region)) paste0(" (Region ", region, ")") else " (alle Regionen)")

  # -9999 -> NA (nur auf den numerischen Kennwerten, wie im Ausgangs-Skript)
  kennw <- intersect(c("TIEFE_OG", "TIEFE_UG", "SAND", "SCHLUFF", "TON",
                       "FEINSAND", "MITTELSAND", "GROBSAND", "BASEN", "NFK_DEHNER"),
                     names(df))
  df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(kennw),
                                        ~ dplyr::na_if(as.numeric(.), -9999)))

  # fehlende Untergrenze abfangen (wie in aufbereiten_profil): +50 cm
  df <- dplyr::mutate(df,
    TIEFE_UG = dplyr::if_else(is.na(TIEFE_UG), TIEFE_OG + 50, TIEFE_UG))

  # je HORIZONT ueber die (regionalen) Profile mitteln -> ein Tiefenband je Horizont
  df %>%
    dplyr::group_by(HORIZONT) %>%
    dplyr::summarise(
      TIEFE_OG   = mean(TIEFE_OG,   na.rm = TRUE),
      TIEFE_UG   = mean(TIEFE_UG,   na.rm = TRUE),
      Sand       = mean(SAND,       na.rm = TRUE),
      Schluff    = mean(SCHLUFF,    na.rm = TRUE),
      Ton        = mean(TON,        na.rm = TRUE),
      Feinsand   = mean(FEINSAND,   na.rm = TRUE),
      Mittelsand = mean(MITTELSAND, na.rm = TRUE),
      Grobsand   = mean(GROBSAND,   na.rm = TRUE),
      BASEN      = mean(BASEN,      na.rm = TRUE),
      NFK        = mean(NFK_DEHNER, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::arrange(TIEFE_OG)
}


# ---------------------------------------------------------------------
# 2) Eigenstaendige Abbildung: Kornanteile (Erdtoene) + Chemie getrennt
# ---------------------------------------------------------------------
#' @param data_input,soeh_krz,region  wie leitprofil_kennwerte()
#' @param titel      optionaler Titel; NULL = aus SOEH_KRZ/Region gebaut
#' @return  patchwork-Objekt (Korn-Panel links, Chemie-Panel rechts)
leitprofil_streuung_plot <- function(data_input, soeh_krz, region = NULL,
                                     titel = NULL) {

  kw <- leitprofil_kennwerte(data_input, soeh_krz, region = region)

  if (is.null(titel))
    titel <- paste0("Leitprofil-Kennwerte - ", paste(soeh_krz, collapse = ", "),
                    if (!is.null(region)) paste0("  (", region, ")") else "  (alle Regionen)")

  # --- Korn: Sand/Schluff/Ton je Horizont in gestapelte x-Positionen legen ---
  korn_long <- kw %>%
    dplyr::select(HORIZONT, TIEFE_OG, TIEFE_UG, Sand, Schluff, Ton) %>%
    tidyr::pivot_longer(c(Sand, Schluff, Ton),
                        names_to = "Fraktion", values_to = "Anteil") %>%
    dplyr::mutate(Fraktion = factor(Fraktion, levels = names(.ERDTOENE)),
                  Anteil   = tidyr::replace_na(Anteil, 0)) %>%
    dplyr::group_by(HORIZONT) %>%
    dplyr::arrange(Fraktion, .by_group = TRUE) %>%
    dplyr::mutate(xmax = cumsum(Anteil), xmin = xmax - Anteil) %>%
    dplyr::ungroup()

  p_korn <- ggplot2::ggplot(korn_long) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = TIEFE_OG, ymax = TIEFE_UG, fill = Fraktion),
                       color = "white", linewidth = 0.2) +
    ggplot2::geom_text(data = kw,
                       ggplot2::aes(x = -2, y = (TIEFE_OG + TIEFE_UG) / 2, label = HORIZONT),
                       hjust = 1, vjust = 0.5, size = 3, color = "grey30") +
    ggplot2::scale_fill_manual(values = .ERDTOENE, name = NULL) +
    ggplot2::scale_y_reverse(name = "Tiefe [cm]") +
    ggplot2::scale_x_continuous(name = "Kornanteil [%]", limits = c(-16, 100),
                                breaks = c(0, 25, 50, 75, 100), expand = c(0, 0)) +
    ggplot2::labs(subtitle = "Kornzusammensetzung") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())

  # --- Chemie: BASEN (%) und NFK (mm) getrennt, freie x-Achse je Kennwert ---
  chem_long <- kw %>%
    dplyr::mutate(TIEFE_MID = (TIEFE_OG + TIEFE_UG) / 2) %>%
    dplyr::select(HORIZONT, TIEFE_MID, BASEN, NFK) %>%
    tidyr::pivot_longer(c(BASEN, NFK), names_to = "Kennwert", values_to = "Wert") %>%
    dplyr::mutate(Kennwert = dplyr::recode(Kennwert,
                    BASEN = "Basensaettigung [%]", NFK = "NFK [mm]"))

  p_chem <- ggplot2::ggplot(chem_long, ggplot2::aes(x = Wert, y = TIEFE_MID)) +
    ggplot2::geom_path(ggplot2::aes(group = Kennwert), color = "grey55", na.rm = TRUE) +
    ggplot2::geom_point(color = "grey20", size = 1.6, na.rm = TRUE) +
    ggplot2::facet_wrap(~ Kennwert, scales = "free_x", nrow = 1) +
    ggplot2::scale_y_reverse(name = NULL) +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::labs(subtitle = "Chemie / Speicher (getrennte Achsen)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  patchwork::wrap_plots(p_korn, p_chem, widths = c(1, 1.2)) +
    patchwork::plot_annotation(title = titel)
}


# ---------------------------------------------------------------------
# 2b) Linien-Variante: Kornfraktionen + Basen ueber die Horizonttiefe
# ---------------------------------------------------------------------
# Entspricht dem urspruenglichen "Auswertung der mittleren Horizonteigen-
# schaften"-Plot, aber mit GERADEN Segmenten (geom_line) statt geom_smooth.
# geom_smooth (Loess) schwingt bei nur wenigen Horizonten ueber und liefert
# unplausible Werte (<0 / >100); die lineare Verbindung der echten Horizont-
# Mittelwerte bleibt dagegen exakt auf den gemessenen Punkten.
#
# NFK (mit_nfk = TRUE) ist in mm, die uebrigen Kennwerte in %. Damit alle Linien
# in EINEM Panel bleiben, wird NFK auf die 0-100-%-Achse gestaucht und ueber eine
# ZWEITE Achse (oben bzw. rechts, sec_axis) wieder in mm beschriftet. So draengt
# NFK die %-Linien nicht mehr zusammen. mit_nfk = FALSE -> nur %-Achse.
#
# vertikal = FALSE  -> Tiefe auf der x-Achse (freistehender Plot).
# vertikal = TRUE   -> Tiefe auf der y-Achse (nach unten), damit die Linien
#                      neben dem aqp-Horizontprofil auf gleicher Tiefe liegen
#                      (so nutzt horizont_mit_linien() die Funktion).
#' @param data_input,soeh_krz,region  wie leitprofil_kennwerte()
#' @param titel    optionaler Titel; NULL = aus SOEH_KRZ/Region gebaut
#' @param mit_nfk  TRUE = NFK-Linie zusaetzlich (mm, ueber zweite Achse lesbar)
#' @param glatt    TRUE = weiche Spline-Kurven DURCH die Punkte (geklammert);
#'                 FALSE = gerade Verbindungen (linear) zwischen den Horizonten
#' @param vertikal TRUE = Tiefe vertikal (zum Anlegen neben das aqp-Profil)
#' @return  ggplot-Objekt
leitprofil_linien_plot <- function(data_input, soeh_krz, region = NULL,
                                   titel = NULL, mit_nfk = TRUE,
                                   glatt = TRUE, vertikal = FALSE) {

  # Farben: Erdtoene fuer die Kornfraktionen, Basen (und NFK) als Kontrast.
  farben <- c(Sand       = "#E4C68C",   # sandgelb
              Feinsand   = "#E2A93B",   # gold
              Mittelsand = "#C9772D",   # orange-braun
              Grobsand   = "#8C4A2F",   # rotbraun
              Schluff    = "#9C7A3C",   # oliv-ocker
              Ton        = "#5B3A29",   # dunkelbraun
              Basen      = "#3B6E9A",   # blau (Chemie, hebt sich ab)
              NFK        = "#2E7D64")   # gruen (nur bei mit_nfk)

  kw <- leitprofil_kennwerte(data_input, soeh_krz, region = region) %>%
    dplyr::rename(Basen = BASEN)

  if (is.null(titel))
    titel <- paste0("Mittlere Horizonteigenschaften - ", paste(soeh_krz, collapse = ", "),
                    if (!is.null(region)) paste0("  (", region, ")") else "  (alle Regionen)")

  # Welche Linien: Kornfraktionen + Basen, optional NFK. Tiefe = untere Horizont-
  # grenze (TIEFE_UG), wie in der Ausgangsfunktion (mean_HORZ = mean(TIEFE_UG)).
  kennwerte <- c("Sand", "Feinsand", "Mittelsand", "Grobsand", "Schluff", "Ton", "Basen")
  if (mit_nfk) kennwerte <- c(kennwerte, "NFK")

  # NFK ist in mm, alle anderen Kennwerte in %. Damit NFK auf der 0-100-%-Achse
  # mitlaeuft, wird es auf diesen Bereich gestaucht (NFK/nfk_max*100) und ueber
  # eine ZWEITE Achse (sec_axis) wieder in mm lesbar gemacht. nfk_max = auf 10er
  # aufgerundetes Datenmaximum, damit die zweite Achse runde Werte zeigt.
  nfk_max <- NA_real_
  if (mit_nfk) {
    m <- suppressWarnings(max(kw$NFK, na.rm = TRUE))
    nfk_max <- if (!is.finite(m) || m <= 0) 100 else ceiling(m / 10) * 10
  }

  lang <- kw %>%
    dplyr::select(HORIZONT, TIEFE_UG, dplyr::all_of(kennwerte)) %>%
    tidyr::pivot_longer(dplyr::all_of(kennwerte),
                        names_to = "Kennwert", values_to = "Wert") %>%
    dplyr::mutate(Kennwert = factor(Kennwert, levels = kennwerte)) %>%
    dplyr::arrange(Kennwert, TIEFE_UG)

  # NFK-Werte in den %-Raum stauchen (Punkte, Kurve und Labels teilen danach
  # dieselbe 0-100-Skala; die sec_axis rechnet fuer die Beschriftung zurueck).
  if (!is.na(nfk_max))
    lang <- dplyr::mutate(lang,
      Wert = dplyr::if_else(Kennwert == "NFK", Wert / nfk_max * 100, Wert))

  # Ende jeder Linie (tiefster Horizont) fuer die Direkt-Beschriftung.
  enden <- dplyr::filter(lang, TIEFE_UG == max(TIEFE_UG))

  # Zweite Achse fuer NFK (mm); ohne NFK bleibt es bei der reinen %-Achse.
  sek <- if (!is.na(nfk_max))
    ggplot2::sec_axis(~ . / 100 * nfk_max, name = "NFK [mm]") else ggplot2::waiver()

  # Linien-Datensatz: bei glatt = TRUE ein kubischer Spline (n = 200) DURCH die
  # echten Horizontpunkte, je Kennwert auf einen plausiblen Bereich geklammert
  # (0..100 %, NFK bis Datenmaximum). So bleiben die Kurven weich wie bei
  # geom_smooth, laufen aber - anders als Loess - nicht ins Unplausible (<0/>100)
  # und bleiben an den gemessenen Punkten. glatt = FALSE -> die rohen Punkte
  # (gerade Verbindung). Die echten Werte werden separat als Punkte gezeichnet.
  linien <- lang
  if (glatt) {
    linien <- lang %>%
      dplyr::filter(!is.na(Wert)) %>%
      dplyr::group_by(Kennwert) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::group_modify(~ {
        s <- stats::spline(.x$TIEFE_UG, .x$Wert, n = 200)
        # Alle Werte liegen jetzt im 0-100-Raum (NFK oben bereits gestaucht).
        data.frame(TIEFE_UG = s$x, Wert = pmin(pmax(s$y, 0), 100))
      }) %>%
      dplyr::ungroup()
  }

  if (vertikal) {
    # Tiefe nach unten (scale_y_reverse) -> deckt sich mit dem aqp-Profil.
    ggplot2::ggplot(lang, ggplot2::aes(x = Wert, y = TIEFE_UG, color = Kennwert)) +
      ggplot2::geom_hline(data = kw, ggplot2::aes(yintercept = TIEFE_UG),
                          color = "grey85", linewidth = 0.3) +
      ggplot2::geom_path(data = linien, linewidth = 1, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
      ggplot2::geom_text(data = enden, ggplot2::aes(label = Kennwert),
                         hjust = -0.1, vjust = 0.4, size = 3, na.rm = TRUE) +
      ggplot2::scale_color_manual(values = farben, guide = "none") +
      ggplot2::scale_y_reverse(name = "Tiefe [cm]", breaks = round(kw$TIEFE_UG)) +
      ggplot2::scale_x_continuous(name = "Mittelwert je Horizont [%]", sec.axis = sek,
                                  limits = c(0, 100),
                                  expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
      ggplot2::labs(title = titel) +
      ggplot2::theme_minimal() +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(12, 30, 6, 6))
  } else {
    ggplot2::ggplot(lang, ggplot2::aes(x = TIEFE_UG, y = Wert, color = Kennwert)) +
      ggplot2::geom_vline(data = kw, ggplot2::aes(xintercept = TIEFE_UG),
                          color = "grey85", linewidth = 0.3) +
      ggplot2::geom_text(data = kw, ggplot2::aes(x = TIEFE_UG, y = Inf, label = HORIZONT),
                         inherit.aes = FALSE, angle = 90, vjust = -0.4, hjust = 1,
                         size = 3, color = "grey55") +
      ggplot2::geom_line(data = linien, linewidth = 1, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
      ggplot2::geom_text(data = enden, ggplot2::aes(label = Kennwert),
                         hjust = -0.15, vjust = 0.4, size = 3.2, na.rm = TRUE) +
      ggplot2::scale_color_manual(values = farben, guide = "none") +
      ggplot2::scale_x_continuous(name = "Tiefe des Horizontes [cm]",
                                  breaks = round(kw$TIEFE_UG),
                                  limits = c(0, max(kw$TIEFE_UG) + 8),
                                  expand = ggplot2::expansion(mult = c(0.01, 0.12))) +
      ggplot2::scale_y_continuous(name = "Mittelwert je Horizont [%]", sec.axis = sek,
                                  limits = c(0, 100)) +
      ggplot2::labs(title = titel) +
      ggplot2::theme_minimal() +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(12, 12, 6, 6))
  }
}


# ---------------------------------------------------------------------
# 3) Variante 1: aqp-Horizontprofil + Streuung seitlich daneben
# ---------------------------------------------------------------------
# Ruft die UNVERAENDERTE horizont_abfolge_plot() (base-R/aqp) auf und faengt
# sie via cowplot als Grob ein, damit sie neben das ggplot-Streuungspanel passt.
#' @param koernung   an horizont_abfolge_plot() durchgereicht (KA5-Symbole)
#' @param rel_breite relative Spaltenbreiten c(Profil, Streuung)
#' @return  cowplot-Objekt (mit print()/plot() zeichnen oder ggsave())
horizont_mit_streuung <- function(data_input, soeh_krz, region = NULL,
                                  koernung = TRUE, rel_breite = c(1, 1.4)) {

  gg <- leitprofil_streuung_plot(data_input, soeh_krz, region = region)

  aqp_grob <- cowplot::as_grob(function()
    horizont_abfolge_plot(data_input, soeh_krz = soeh_krz,
                          region = region, koernung = koernung))

  cowplot::plot_grid(aqp_grob, gg, nrow = 1, rel_widths = rel_breite)
}


# ---------------------------------------------------------------------
# 3b) Variante 1b: aqp-Horizontprofil + lineare Kennwert-LINIEN daneben
# ---------------------------------------------------------------------
# Wie horizont_mit_streuung(), aber statt der Balken das Linien-Panel
# (leitprofil_linien_plot, vertikal) rechts neben dem aqp-Profil - Tiefe
# in beiden nach unten, damit die Horizonte auf gleicher Hoehe liegen.
#' @param koernung   an horizont_abfolge_plot() durchgereicht (KA5-Symbole)
#' @param mit_nfk    an leitprofil_linien_plot() durchgereicht (NFK-Linie)
#' @param glatt      an leitprofil_linien_plot() durchgereicht (weiche Kurven)
#' @param rel_breite relative Spaltenbreiten c(Profil, Linien)
#' @return  cowplot-Objekt (mit print()/plot() zeichnen oder ggsave())
horizont_mit_linien <- function(data_input, soeh_krz, region = NULL,
                                koernung = TRUE, mit_nfk = TRUE, glatt = TRUE,
                                rel_breite = c(1, 1.6)) {

  gg <- leitprofil_linien_plot(data_input, soeh_krz, region = region,
                               mit_nfk = mit_nfk, glatt = glatt, vertikal = TRUE)

  aqp_grob <- cowplot::as_grob(function()
    horizont_abfolge_plot(data_input, soeh_krz = soeh_krz,
                          region = region, koernung = koernung))

  cowplot::plot_grid(aqp_grob, gg, nrow = 1, rel_widths = rel_breite)
}
