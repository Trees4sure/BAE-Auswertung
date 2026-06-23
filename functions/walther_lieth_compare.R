# =============================================================================
# Vergleich von Monats-WL-Diagrammen zwischen Klimalaeufen.
#
#   plot_walther_lieth_facets() - Small Multiples (ein WL je Lauf, gem. Achsen)
#   plot_walther_lieth_delta()  - Delta-WL (Vergleichslauf minus Referenz)
#   compare_walther_lieth()     - Wrapper: "facets", "delta" oder "both"
#
# Erwartet geladen: walther_lieth_helpers.R (Farben, MONATE, wl_* Helfer)
# Eingang: Long-Format wie aus build_walther_lieth_input()
#          (id | Zeitlauf | Monat | T_mean | P_sum).
# =============================================================================


# ---- 1. Small Multiples: ein Monats-WL je Lauf ------------------------------
plot_walther_lieth_facets <- function(df, id_val, runs = NULL, ncol = NULL) {
  d <- df[df$id == id_val, ]
  if (!is.null(runs)) d <- d[d$Zeitlauf %in% runs, ]
  if (nrow(d) == 0) stop("Keine Daten fuer id=", id_val, ".")
  runs <- if (is.null(runs)) unique(d$Zeitlauf) else runs

  # Monatliche Kurvendaten (gemeinsame, feste WL-Skalierung)
  monthly <- dplyr::tibble(
    Zeitlauf = wl_run_factor(d$Zeitlauf, runs),
    month = d$Monat, temp = d$T_mean, prec = d$P_sum, pt = wl_p2t(d$P_sum)
  )

  # Feine Fuelldaten je Lauf (Gruppe eindeutig je Facet)
  fine_list <- list()
  for (r in runs) {
    sub <- monthly[monthly$Zeitlauf == r, ]
    f <- wl_make_fine(sub$month, sub$temp, sub$prec)
    f$Zeitlauf <- r
    f$grp <- paste(r, f$grp)
    fine_list[[r]] <- f
  }
  fine <- dplyr::bind_rows(fine_list)
  fine$Zeitlauf <- wl_run_factor(fine$Zeitlauf, runs)

  humid <- fine[fine$humid, ]
  arid  <- fine[!fine$humid, ]

  ymax <- max(50, ceiling(max(c(monthly$temp, monthly$pt)) / 10) * 10)
  ymin <- min(0,  floor(min(monthly$temp) / 10) * 10)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = humid,
      ggplot2::aes(x, ymin = temp, ymax = pt, group = grp),
      fill = COL_PREC, alpha = 0.30) +
    ggplot2::geom_ribbon(data = arid,
      ggplot2::aes(x, ymin = pt, ymax = temp, group = grp),
      fill = COL_TEMP, alpha = 0.22) +
    ggplot2::geom_hline(yintercept = 50, colour = "grey80", linetype = "dotted") +
    ggplot2::geom_line(data = monthly,
      ggplot2::aes(month, temp), colour = COL_TEMP, linewidth = 0.8) +
    ggplot2::geom_line(data = monthly,
      ggplot2::aes(month, pt), colour = COL_PREC, linewidth = 0.8) +
    ggplot2::facet_wrap(~ Zeitlauf, ncol = ncol) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = MONATE) +
    ggplot2::scale_y_continuous(
      name = "Temperatur [°C]", limits = c(ymin, ymax),
      sec.axis = ggplot2::sec_axis(~ wl_t2p(.), name = "Niederschlag [mm]",
                                   breaks = c(seq(0, 100, 20),
                                              if (ymax > 50) seq(200, wl_t2p(ymax), 100)))) +
    ggplot2::labs(title = paste0("Walther-Lieth-Vergleich · Punkt ", id_val),
                  x = "Monat") +
    wl_compare_theme()
}


# ---- 2. Delta-WL: Vergleichslauf minus Referenz -----------------------------
# Zeigt ΔT (rote Linie) und ΔP (Balken: blau feuchter / braun trockener) je
# Monat um die Null-Linie. Monatsdeltas sind klein -> echte 1:2-Kopplung.
plot_walther_lieth_delta <- function(df, id_val, run_ref, run_cmp) {
  a <- df[df$id == id_val & df$Zeitlauf == run_ref, ]
  b <- df[df$id == id_val & df$Zeitlauf == run_cmp, ]
  a <- a[order(a$Monat), ]; b <- b[order(b$Monat), ]
  if (nrow(a) != 12 || nrow(b) != 12)
    stop("Beide Laeufe brauchen 12 Monate (Ref: ", nrow(a), ", Cmp: ", nrow(b), ").")

  delta <- dplyr::tibble(
    month = 1:12,
    dT = b$T_mean - a$T_mean,
    dP = b$P_sum  - a$P_sum
  )
  delta$dP_auf_temp <- delta$dP / 2          # 1 degC : 2 mm
  delta$richtung    <- ifelse(delta$dP < 0, "trockener", "feuchter")

  ggplot2::ggplot(delta, ggplot2::aes(x = month)) +
    ggplot2::geom_col(ggplot2::aes(y = dP_auf_temp, fill = richtung),
                      alpha = 0.45, width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_line(ggplot2::aes(y = dT), colour = COL_TEMP, linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(y = dT), colour = COL_TEMP, size = 1.4) +
    ggplot2::scale_fill_manual(values = c("feuchter" = COL_PREC,
                                          "trockener" = COL_DRY),
                               name = NULL) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = MONATE) +
    ggplot2::scale_y_continuous(
      name = "ΔTemperatur [°C]",
      sec.axis = ggplot2::sec_axis(~ . * 2, name = "ΔNiederschlag [mm]")) +
    ggplot2::labs(
      title = paste0("ΔWL · Punkt ", id_val),
      subtitle = sprintf("%s  −  %s   ·   Ø %+.1f °C  |  %+d mm/a",
                         run_cmp, run_ref, mean(delta$dT), round(sum(delta$dP))),
      x = "Monat") +
    wl_compare_theme()
}


# ---- 3. Wrapper -------------------------------------------------------------
# mode: "facets", "delta" oder "both" (both -> patchwork, Referenz = erster Lauf)
compare_walther_lieth <- function(df, id_val, runs = NULL,
                                  mode = c("both", "facets", "delta"),
                                  ref = NULL) {
  mode <- match.arg(mode)
  d_runs <- if (is.null(runs)) unique(df$Zeitlauf[df$id == id_val]) else runs
  if (is.null(ref)) ref <- d_runs[1]

  if (mode == "facets")
    return(plot_walther_lieth_facets(df, id_val, runs = d_runs))

  # Delta je Vergleichslauf (alle ausser Referenz)
  cmps <- setdiff(d_runs, ref)
  deltas <- lapply(cmps, function(rc)
    plot_walther_lieth_delta(df, id_val, ref, rc))

  if (mode == "delta") {
    if (length(deltas) == 1) return(deltas[[1]])
    return(wl_patch(deltas, ncol = 1))
  }

  # mode == "both": Small Multiples oben, Deltas darunter
  oben  <- plot_walther_lieth_facets(df, id_val, runs = d_runs)
  unten <- if (length(deltas) == 1) deltas[[1]] else wl_patch(deltas, ncol = length(deltas))
  wl_patch(list(oben, unten), ncol = 1, heights = c(2, 1.4))
}
