# =============================================================================
# Verschiebungs-Testdaten + Baumartenempfehlungen erzeugen.
#
# Ein Standort (BWI-Punkt), drei Klimalaeufe mit zunehmender Verschiebung:
#   Referenz_1991-2020  ->  RCP45_2071-2100  ->  RCP85_2071-2100
# Damit werden Differenz-Diagramme (Small Multiples + Delta) und die
# Empfehlungs-Leiste sichtbar getestet.
#
# Erzeugt:
#   shift_monthly_test.csv    (echtes WL, Monatswerte)
#   shift_wali_trend_test.csv (WaLi-Trend, 30 Jahreswerte)
#   recommendation_test.csv   (Empfehlungsstufe je Baumart x Lauf)
#
# Aufruf:  source("functions/testdata/make_shift_test_data.R")
# =============================================================================

set.seed(7)
out_dir <- "functions/testdata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Standort + Klima-Grundwerte -------------------------------------------
pid <- 70041234; alt <- 480; lon <- 9.85; lat <- 48.40
Tm  <- 7.4    # Jahresmittel-Basis [degC]
Pa  <- 840    # Jahresniederschlag-Basis [mm]

# Drei Laeufe mit Verschiebungs-Parametern (in fester Reihenfolge)
runs <- c("Referenz_1991-2020", "RCP45_2071-2100", "RCP85_2071-2100")
shift <- list(
  "Referenz_1991-2020" = list(dT = 0.0, summer = 1.00, winter = 1.00,
                              start = 1991, slopeT = 0.015, slopeP = -0.5, baseP = 840),
  "RCP45_2071-2100"    = list(dT = 2.5, summer = 0.90, winter = 1.00,
                              start = 2071, slopeT = 0.030, slopeP = -1.5, baseP = 840 * 0.95),
  "RCP85_2071-2100"    = list(dT = 4.2, summer = 0.75, winter = 1.08,
                              start = 2071, slopeT = 0.060, slopeP = -3.0, baseP = 840 * 0.88)
)


# ---- 1. Monatsdaten (echtes WL) --------------------------------------------
monthly_rows <- list()
for (run in runs) {
  s <- shift[[run]]
  m <- 1:12

  # Temperatur: Sinus, Minimum Januar / Maximum Juli, plus Shift
  temp <- Tm + s$dT + 8.7 * cos(2 * pi * (m - 7) / 12) + rnorm(12, 0, 0.3)

  # Niederschlag: Sommermaximum, im Sommer trockener / Winter feuchter je Lauf
  pf <- ifelse(m %in% c(6, 7, 8), s$summer,
        ifelse(m %in% c(12, 1, 2), s$winter, 1.0))
  season <- 1 + 0.30 * cos(2 * pi * (m - 7) / 12)
  prec   <- (Pa / 12) * season * pf + rnorm(12, 0, 5)

  monthly_rows[[run]] <- data.frame(
    id = pid, Zeitlauf = run, altitude = alt, Lon = lon, Lat = lat,
    Monat = m, T_mean = round(temp, 1), P_sum = round(pmax(prec, 1), 0)
  )
}
monthly <- do.call(rbind, monthly_rows); row.names(monthly) <- NULL
write.csv(monthly, file.path(out_dir, "shift_monthly_test.csv"), row.names = FALSE)


# ---- 2. 30-Jahres-Verlauf (WaLi-Trend) -------------------------------------
ts_rows <- list()
for (run in runs) {
  s <- shift[[run]]
  j   <- 1:30
  cal <- s$start - 1 + j

  temp <- Tm + s$dT + s$slopeT * (j - 1) + rnorm(30, 0, 0.3)
  prec <- s$baseP   + s$slopeP * (j - 1) + rnorm(30, 0, 60)

  ts_rows[[run]] <- data.frame(
    id = pid, Zeitlauf = run, altitude = alt, Lon = lon, Lat = lat,
    Jahr = j, Kalenderjahr = cal,
    T_year = round(temp, 2), P_year = round(pmax(prec, 50), 0)
  )
}
timeseries <- do.call(rbind, ts_rows); row.names(timeseries) <- NULL
write.csv(timeseries, file.path(out_dir, "shift_wali_trend_test.csv"), row.names = FALSE)


# ---- 3. Baumartenempfehlung je Art x Lauf ----------------------------------
# Stufen: sehr empfohlen > empfohlen > bedingt empfohlen > nicht empfohlen
empfehlung <- list(
  "Fichte"        = c("sehr empfohlen", "bedingt empfohlen", "nicht empfohlen"),
  "Weisstanne"    = c("empfohlen",      "empfohlen",         "bedingt empfohlen"),
  "Rotbuche"      = c("sehr empfohlen", "empfohlen",         "bedingt empfohlen"),
  "Trauben-Eiche" = c("empfohlen",      "sehr empfohlen",    "sehr empfohlen"),
  "Douglasie"     = c("empfohlen",      "empfohlen",         "empfohlen")
)
rec_rows <- list()
for (art in names(empfehlung)) {
  rec_rows[[art]] <- data.frame(
    id = pid, Zeitlauf = runs, Baumart = art,
    Empfehlung = empfehlung[[art]]
  )
}
recommendation <- do.call(rbind, rec_rows); row.names(recommendation) <- NULL
write.csv(recommendation, file.path(out_dir, "recommendation_test.csv"), row.names = FALSE)

message("Verschiebungs-Testdaten geschrieben (monthly: ", nrow(monthly),
        ", trend: ", nrow(timeseries), ", rec: ", nrow(recommendation), " Zeilen)")
