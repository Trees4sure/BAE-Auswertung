# =============================================================================
# Gemeinsame Helfer fuer die WL-Vergleichsdiagramme (Monats-WL + WaLi-Trend).
# =============================================================================

# ---- Farben + Monatslabels (einheitlich in allen Diagrammen) ----------------
COL_TEMP <- "#c0392b"   # rot   (Temperatur)
COL_PREC <- "#2c5fa8"   # blau  (Niederschlag)
COL_DRY  <- "#a6611a"   # braun (trockener)
MONATE   <- c("J","F","M","A","M","J","J","A","S","O","N","D")

# ---- Feste WL-Skalierung: Niederschlag <-> Temperaturachse -----------------
# 1 degC : 2 mm bis 100 mm, darueber 1:20 (klassisches Walther-Lieth).
wl_p2t <- function(p) ifelse(p <= 100, p / 2, 50 + (p - 100) / 20)
wl_t2p <- function(t) ifelse(t <= 50, t * 2, 100 + (t - 50) * 20)


# ---- Feine Interpolation + humid/arid-Gruppen fuer geom_ribbon -------------
# Gibt ein tibble zurueck: x | temp | prec | pt | humid | grp
# (grp = fortlaufende Nummer je zusammenhaengendem humid- bzw. arid-Abschnitt)
wl_make_fine <- function(month, temp, prec, dichte = 24) {
  x   <- seq(min(month), max(month), length.out = length(month) * dichte)
  tf  <- stats::approx(month, temp, x)$y
  pf  <- stats::approx(month, prec, x)$y
  ptf <- wl_p2t(pf)

  humid   <- ptf >= tf
  wechsel <- humid != dplyr::lag(humid, default = humid[1])

  dplyr::tibble(x = x, temp = tf, prec = pf, pt = ptf,
                humid = humid, grp = cumsum(wechsel))
}


# ---- Empfehlungsstufen: Reihenfolge + Farben (gruen -> rot) -----------------
wl_rec_levels <- c("sehr empfohlen", "empfohlen",
                   "bedingt empfohlen", "nicht empfohlen")

wl_rec_colors <- c("sehr empfohlen"    = "#1a9850",
                   "empfohlen"         = "#a6d96a",
                   "bedingt empfohlen" = "#fdae61",
                   "nicht empfohlen"   = "#d73027")

# Empfehlung als geordneten Faktor setzen (fuer stabile Farben/Sortierung)
wl_as_rec_factor <- function(x) factor(x, levels = wl_rec_levels)


# ---- Laeufe in gewuenschter Reihenfolge als Faktor --------------------------
# runs = NULL -> Reihenfolge des ersten Auftretens
wl_run_factor <- function(x, runs = NULL) {
  if (is.null(runs)) runs <- unique(x)
  factor(x, levels = runs)
}


# ---- Gemeinsames Theme (rote Temp-Achse links, blaue Niederschlag-Achse) ----
wl_compare_theme <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.y.left  = ggplot2::element_text(colour = COL_TEMP),
      axis.text.y.left   = ggplot2::element_text(colour = COL_TEMP),
      axis.title.y.right = ggplot2::element_text(colour = COL_PREC),
      axis.text.y.right  = ggplot2::element_text(colour = COL_PREC),
      legend.position    = "bottom",
      plot.subtitle      = ggplot2::element_text(size = 8, colour = "grey25"))
}

# ---- Mehrere ggplots stapeln/nebeneinander (braucht 'patchwork') ------------
wl_patch <- function(plots, ncol = 1, heights = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Paket 'patchwork' wird fuer kombinierte Plots benoetigt.")
  p <- patchwork::wrap_plots(plots, ncol = ncol)
  if (!is.null(heights)) p <- p + patchwork::plot_layout(heights = heights)
  p
}
