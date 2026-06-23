# =============================================================================
# Demo: Differenz-Diagramme + Empfehlungs-Leiste auf den Verschiebungsdaten.
#
# Voraussetzung: Pakete ggplot2, dplyr, patchwork.
# Aufruf:  source("02_function/testdata/demo_compare.R")
# =============================================================================

# ---- Funktionen laden -------------------------------------------------------
source("02_function/walther_lieth_helpers.R")
source("02_function/plot_walther_lieth.R")
source("02_function/walther_lieth_compare.R")
source("02_function/wali_trend_compare.R")
source("02_function/recommendation_strip.R")
source("02_function/compose_overview.R")

# ---- Testdaten einlesen -----------------------------------------------------
wl  <- read.csv("02_function/testdata/shift_monthly_test.csv")
ts  <- read.csv("02_function/testdata/shift_wali_trend_test.csv")
rec <- read.csv("02_function/testdata/recommendation_test.csv")

pid  <- 70041234
runs <- c("Referenz_1991-2020", "RCP45_2071-2100", "RCP85_2071-2100")

# ---- 1. Monats-WL: Small Multiples + Delta ----------------------------------
p_wl_facets <- plot_walther_lieth_facets(wl, pid, runs = runs)
p_wl_delta  <- plot_walther_lieth_delta(wl, pid,
                                        run_ref = "Referenz_1991-2020",
                                        run_cmp = "RCP85_2071-2100")
p_wl_both   <- compare_walther_lieth(wl, pid, runs = runs, mode = "both",
                                     ref = "Referenz_1991-2020")

# ---- 2. WaLi-Trend: Small Multiples + Mittel-Shift --------------------------
p_ts_both <- compare_wali_trend(ts, pid, runs = runs, mode = "both",
                                ref = "Referenz_1991-2020")

# ---- 3. Empfehlungs-Leiste + Kombination mit dem Klimasignal ----------------
p_strip   <- plot_recommendation_strip(rec, pid, runs = runs)
p_combined <- combine_climate_recommendation(p_wl_facets, rec, pid, runs = runs)

# ---- 4. Gesamtuebersicht (links WL untereinander, rechts Empfehlung + Delta) -
p_overview <- compose_scenario_overview(
  wl, rec, pid,
  ref = "Referenz_1991-2020",
  scenarios = c("RCP45_2071-2100", "RCP85_2071-2100"))

# ---- Anzeigen (im interaktiven Betrieb) -------------------------------------
if (interactive()) {
  print(p_wl_both)     # Monats-WL-Vergleich (Facets + Delta)
  print(p_ts_both)     # WaLi-Trend-Vergleich (Facets + Mittel-Shift)
  print(p_combined)    # Klima-Small-Multiples ueber der Empfehlungs-Leiste
  print(p_overview)    # Gesamtuebersicht (das neue Layout)
}

# ---- Optional als Datei speichern -------------------------------------------
# ggplot2::ggsave("uebersicht_standort.png", p_overview, width = 11, height = 8)
