# =============================================================================
# Test-/Beispieldaten fuer die Walther-Lieth-Funktionen erzeugen.
#
# Legt realistische Zufallsdaten im Long-Format an (so, wie sie
# build_walther_lieth_input() bzw. build_wali_trend_input() liefern),
# schreibt sie als CSV und zeichnet zur Kontrolle Beispiel-Diagramme.
#
# Aufruf:  source("02_function/WL_Diagramme/testdata/make_test_data.R")
# =============================================================================

set.seed(42)

out_dir <- "02_function/WL_Diagramme/testdata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Punkt-Metadaten (id, Hoehe m, Lon, Lat) -------------------------------
punkte <- data.frame(
  id       = c(70041234, 70045678, 70049999),
  altitude = c(480,       120,       950),
  Lon      = c(9.85,      10.20,     11.30),
  Lat      = c(48.40,     52.10,     47.55)
)

# Jahresmittel grob ueber Hoehengradient
t_annual <- function(alt) 10.2 - 0.0058 * alt   # degC
p_annual <- function(alt) 600  + 0.50   * alt   # mm/a


# ---- 1. Monatsdaten (Walther-Lieth: id|Zeitlauf|Monat|T_mean|P_sum) --------
mon_runs <- list(
  "OBS_DWD_1961-1990"      = list(dT = 0.0, pf = 1.00),
  "RCP45_MPIWRF_2071-2100" = list(dT = 3.0, pf = 0.95)
)

monthly_rows <- list()
for (i in seq_len(nrow(punkte))) {
  p   <- punkte[i, ]
  Tm  <- t_annual(p$altitude)
  Pa  <- p_annual(p$altitude)

  for (run in names(mon_runs)) {
    par <- mon_runs[[run]]
    m   <- 1:12

    # Temperatur: Sinus mit Minimum im Januar, Maximum im Juli
    temp <- Tm + par$dT + 8.7 * cos(2 * pi * (m - 7) / 12) + rnorm(12, 0, 0.4)
    # Niederschlag: leichtes Sommermaximum
    season <- 1 + 0.30 * cos(2 * pi * (m - 7) / 12)
    prec   <- (Pa / 12) * season * par$pf + rnorm(12, 0, 6)

    monthly_rows[[paste(p$id, run)]] <- data.frame(
      id = p$id, Zeitlauf = run,
      altitude = p$altitude, Lon = p$Lon, Lat = p$Lat,
      Monat = m,
      T_mean = round(temp, 1),
      P_sum  = round(pmax(prec, 1), 0)
    )
  }
}
monthly <- do.call(rbind, monthly_rows)
row.names(monthly) <- NULL
write.csv(monthly, file.path(out_dir, "walther_lieth_monthly_test.csv"),
          row.names = FALSE)


# ---- 2. 30-Jahres-Verlauf (1049/1050: id|Zeitlauf|Jahr|...|T_year|P_year) --
ts_runs <- list(
  "RCP45_MPIWRF_2071-2100" = list(off = 3.0, slope = 0.030, psl = -1.5),
  "RCP85_HADWRF_2071-2100" = list(off = 4.2, slope = 0.060, psl = -3.0)
)

ts_rows <- list()
for (i in seq_len(nrow(punkte))) {
  p  <- punkte[i, ]
  Tm <- t_annual(p$altitude)
  Pa <- p_annual(p$altitude)

  for (run in names(ts_runs)) {
    par <- ts_runs[[run]]
    j   <- 1:30                       # Jahresindex
    cal <- 2070 + j                   # Kalenderjahr

    # Temperatur mit Erwaermungstrend, Niederschlag mit leichtem Rueckgang
    temp <- Tm + par$off + par$slope * (j - 1) + rnorm(30, 0, 0.35)
    prec <- Pa + par$psl * (j - 1) + rnorm(30, 0, 70)

    ts_rows[[paste(p$id, run)]] <- data.frame(
      id = p$id, Zeitlauf = run,
      altitude = p$altitude, Lon = p$Lon, Lat = p$Lat,
      Jahr = j, Kalenderjahr = cal,
      T_year = round(temp, 2),
      P_year = round(pmax(prec, 50), 0)
    )
  }
}
timeseries <- do.call(rbind, ts_rows)
row.names(timeseries) <- NULL
write.csv(timeseries, file.path(out_dir, "wali_trend_test.csv"),
          row.names = FALSE)

message("CSV geschrieben: ", out_dir,
        " (monthly: ", nrow(monthly), " Zeilen, timeseries: ",
        nrow(timeseries), " Zeilen)")


# ---- 3. Beispiel: CSV einlesen und Diagramme zeichnen ----------------------
# (nur ausfuehren, wenn die Funktionen geladen sind)
if (interactive()) {
  source("02_function/WL_Diagramme/walther_lieth_input.R")
  source("02_function/WL_Diagramme/plot_walther_lieth.R")
  source("02_function/WL_Diagramme/wali_trend.R")

  wl <- read.csv(file.path(out_dir, "walther_lieth_monthly_test.csv"))
  ts <- read.csv(file.path(out_dir, "wali_trend_test.csv"))

  print(plot_walther_lieth_from_long(
    wl, id_val = 70041234, run = "OBS_DWD_1961-1990"))

  print(plot_wali_trend_from_long(
    ts, id_val = 70041234, run = "RCP85_HADWRF_2071-2100"))
}
