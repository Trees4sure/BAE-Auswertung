# =============================================================================
# WaLi-Zeitstrahl: ALLE Laeufe EINES Punktes auf EINER durchgehenden Achse
# (1961..2100). Jeder Lauf liegt in seiner realen Zeitscheibe (Kalenderjahr);
# mehrere Szenarien in derselben Scheibe (z.B. 2071-2100: RCP45/RCP85, mehrere
# GCMs) ueberlagern sich als ALPHA-Balken (Niederschlag) + Linien (Temperatur).
# So sieht man auf einen Blick, welche PFADE die Szenarien nehmen - und die
# Differenzen zwischen ihnen direkt (ersetzt die separate Mittel-Shift-Leiste).
#
# Eingang: Trend-Long wie aus build_wali_trend_input() / wl_trend_nr_from_files()
#          (id bzw. MASTER_ID | Zeitlauf | Kalenderjahr | T_year | P_year).
# Erwartet geladen: ggplot2, dplyr (Farben optional aus walther_lieth_helpers.R).
# =============================================================================


# ---- Szenario-Kuerzel aus dem Laufnamen ("RCP85_MPICLM_2071-2100" -> "RCP85").
.wali_szenario <- function(run){
  toupper(sub("_.*$", "", as.character(run)))
}


# ---- Zeitstrahl-Plot --------------------------------------------------------
#' Durchgehender 1961-2100-Vergleich aller Laeufe EINES Punktes.
#'
#' @param df        Trend-Long (id/MASTER_ID | Zeitlauf | Kalenderjahr | T_year | P_year).
#' @param id_val    Punkt-/MASTER_ID-Wert.
#' @param id_col    Schluesselspalte (Default "id"; App-Daten: "MASTER_ID").
#' @param runs      optional: nur diese Laeufe (Default: alle fuer den Punkt).
#' @param prec_mode "absolut" (Default) = Jahresniederschlag; "diff" = Abweichung
#'                  zum Referenz-Mittel (ref_run) -> Balken zeigen die Differenz.
#' @param ref_run   Referenzlauf fuer prec_mode="diff" (Default: fruehester Lauf).
#' @param col_by    Farbgruppe: "Szenario" (Default) oder "Zeitlauf".
#' @param trend     TRUE: lineare Trendlinie je Lauf (gestrichelt).
#' @param name      optionaler Titel.
#' @return ggplot-Objekt.
plot_wali_timeline <- function(df, id_val, id_col = "id", runs = NULL,
                               prec_mode = c("absolut", "diff"),
                               ref_run = NULL,
                               col_by = c("Szenario", "Zeitlauf"),
                               trend = TRUE, name = NULL){
  prec_mode <- match.arg(prec_mode)
  col_by    <- match.arg(col_by)
  for(pkg in c("ggplot2", "dplyr", "tidyr"))
    if(!requireNamespace(pkg, quietly = TRUE)) stop("Paket '", pkg, "' noetig.")

  if(!id_col %in% names(df))
    stop("Spalte '", id_col, "' fehlt (vorhanden: ",
         paste(names(df), collapse = ", "), ").")
  sub <- df[df[[id_col]] == id_val, , drop = FALSE]
  if(!is.null(runs)) sub <- sub[sub$Zeitlauf %in% runs, , drop = FALSE]
  if(nrow(sub) == 0) stop("Keine Daten fuer ", id_col, "=", id_val, ".")
  if(!"Kalenderjahr" %in% names(sub) || all(is.na(sub$Kalenderjahr)))
    stop("Kalenderjahr fehlt - der Zeitstrahl braucht echte Jahre.")
  sub <- sub[!is.na(sub$Kalenderjahr), , drop = FALSE]

  sub$Szenario <- .wali_szenario(sub$Zeitlauf)
  farbe <- if(col_by == "Szenario") "Szenario" else "Zeitlauf"

  # Niederschlag absolut oder als Differenz zum Referenz-Mittel
  if(prec_mode == "diff"){
    ref <- if(!is.null(ref_run)) ref_run else
      sub$Zeitlauf[which.min(sub$Kalenderjahr)]
    ref_mean   <- mean(sub$P_year[sub$Zeitlauf == ref], na.rm = TRUE)
    sub$P_show <- sub$P_year - ref_mean
    p_lab <- "Δ Jahresniederschlag [mm]"
  } else {
    sub$P_show <- sub$P_year
    p_lab <- "Jahresniederschlag [mm]"
  }
  t_lab <- "Jahresmitteltemperatur [°C]"

  # ZWEI gestapelte Panels (Temperatur / Niederschlag) statt Doppelachse: keine
  # Quetschung mehr, jeder Lauf bleibt eine eigene Linie (auch v2 vs. Original).
  lang <- tidyr::pivot_longer(sub, cols = c("T_year", "P_show"),
                              names_to = "Groesse", values_to = "Wert")
  lang$Panel <- factor(ifelse(lang$Groesse == "T_year", t_lab, p_lab),
                       levels = c(t_lab, p_lab))

  aes <- ggplot2::aes
  p <- ggplot2::ggplot(lang, aes(x = .data$Kalenderjahr, y = .data$Wert,
                                 colour = .data[[farbe]], group = .data$Zeitlauf)) +
    ggplot2::geom_line(linewidth = 0.6, alpha = 0.7)

  if(isTRUE(trend))
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                                  linewidth = 0.5, linetype = "dashed", alpha = 0.6)

  # Nulllinie nur im Niederschlags-Panel (Differenz-Modus)
  if(prec_mode == "diff")
    p <- p + ggplot2::geom_hline(
      data = data.frame(Panel = factor(p_lab, levels = c(t_lab, p_lab))),
      aes(yintercept = 0), inherit.aes = FALSE, colour = "grey60", linewidth = 0.3)

  p +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Panel),
                        scales = "free_y", switch = "y") +
    ggplot2::labs(
      title = if(is.null(name)) paste0("WaLi-Zeitstrahl · Punkt ", id_val) else name,
      subtitle = "oben Temperatur, unten Niederschlag; je Linie ein Lauf",
      x = "Kalenderjahr", y = NULL, colour = farbe) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      strip.placement   = "outside",
      strip.background   = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      plot.subtitle     = ggplot2::element_text(size = 9, colour = "grey25"),
      legend.position   = "bottom")
}


# ---- Beispiel (auskommentiert) ---------------------------------------------
# source("02_function/WL_Diagramme/wali_trend.R")
# source("02_function/WL_Diagramme/wali_timeline.R")
# # alle Laeufe EINES Punktes (ids=1!) ueber die BWI-Jahresraster:
# ts_all <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi, ids = 1)
# plot_wali_timeline(ts_all, id_val = 1)                         # absolut
# plot_wali_timeline(ts_all, id_val = 1, prec_mode = "diff",
#                    ref_run = "OBS_DWD_1961-1990")              # Differenzen
