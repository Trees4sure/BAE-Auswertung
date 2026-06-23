# =============================================================================
# Gesamtuebersicht fuer EINEN Standort ueber mehrere Klimalaeufe.
#
# Layout (eine Grafik, gemeinsame Zeilen-Ausrichtung):
#
#   +----------------------+------------------------------+
#   | WL Referenz          | Empfehlungswechsel (Arten)   |   <- Zeile Referenz
#   +----------------------+------------------------------+
#   | WL Szenario 1        | Delta-WL Szenario 1          |   <- Zeile Szenario 1
#   +----------------------+------------------------------+
#   | WL Szenario 2        | Delta-WL Szenario 2          |   <- Zeile Szenario 2
#   +----------------------+------------------------------+
#
# Links: die WL-Diagramme klein untereinander. Rechts oben die Veraenderung der
# Baumartenempfehlung, darunter je Szenario das Differenz-Diagramm (warum sich
# das Klima - und damit die Empfehlung - verschiebt).
#
# Erwartet geladen:
#   plot_walther_lieth.R      (wl_panel)
#   walther_lieth_compare.R   (plot_walther_lieth_delta)
#   recommendation_strip.R    (plot_recommendation_strip)
#   walther_lieth_helpers.R   (wl_patch)
# =============================================================================

#' Standort-Gesamtuebersicht: WL je Lauf + Empfehlungswechsel + Delta-Diagramme.
#'
#' @param wl_df   Monats-Long-Format (id | Zeitlauf | Monat | T_mean | P_sum)
#' @param rec_df  Empfehlungen (id | Zeitlauf | Baumart | Empfehlung)
#' @param id_val  Punkt-id
#' @param ref     Referenzlauf (oben)
#' @param scenarios  Vergleichslaeufe (darunter, in Reihenfolge)
#' @return patchwork-Objekt
compose_scenario_overview <- function(wl_df, rec_df, id_val, ref, scenarios) {
  runs_all <- c(ref, scenarios)

  # --- kompaktes WL je Lauf (nur Panel, Lauf als Titel, ohne Legende) --------
  wl_klein <- function(run) {
    sub <- wl_df[wl_df$id == id_val & wl_df$Zeitlauf == run, ]
    sub <- sub[order(sub$Monat), ]
    wl_panel(sub$T_mean, sub$P_sum) +
      ggplot2::labs(title = run, x = NULL) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 10, face = "bold"))
  }
  links <- wl_patch(lapply(runs_all, wl_klein), ncol = 1)

  # --- rechts: Empfehlungswechsel oben, dann Delta je Szenario ---------------
  strip <- plot_recommendation_strip(rec_df, id_val, runs = runs_all) +
    ggplot2::labs(title = "Baumartenempfehlung")

  delta_one <- function(i) {
    p <- plot_walther_lieth_delta(wl_df, id_val, ref, scenarios[i])
    # Legende nur beim untersten Delta zeigen (sonst mehrfach)
    if (i == length(scenarios)) p
    else p + ggplot2::theme(legend.position = "none")
  }
  deltas <- lapply(seq_along(scenarios), delta_one)
  rechts <- wl_patch(c(list(strip), deltas), ncol = 1)

  # --- beide Spalten nebeneinander -------------------------------------------
  wl_patch(list(links, rechts), ncol = 2)
}
