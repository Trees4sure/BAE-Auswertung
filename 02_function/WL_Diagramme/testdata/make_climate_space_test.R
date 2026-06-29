# =============================================================================
# Test-/Beispieldaten fuer das Klima-Wolken-Diagramm (climate_space.R) erzeugen.
#
# Legt eine realistische deutschlandweite Punktwolke an (eine Zeile je
# BWI-BZE-Punkt je Lauf) mit den Spalten, die plot_climate_space() erwartet:
#   MASTER_ID | BL | Zeitlauf | MAT | MAP
# und zeichnet zur Kontrolle ein Beispiel (Bundesland MV hellgrau, zwei
# MASTER_IDs rot).
#
# Aufruf:  source("02_function/WL_Diagramme/testdata/make_climate_space_test.R")
# =============================================================================

set.seed(7)

out_dir <- "02_function/WL_Diagramme/testdata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Bundeslaender mit grobem Klima-Schwerpunkt (MAT [degC], MAP [mm]) -------
# (nur fuer plausible Streuung, keine amtlichen Mittelwerte)
bl_klima <- data.frame(
  BL  = c("SH","HH","NI","HB","MV","BB","BE","ST","SN","TH",
          "NW","HE","RP","SL","BW","BY"),
  MAT = c(9.2, 9.6, 9.4, 9.6, 8.9, 9.3, 9.7, 9.2, 9.0, 8.4,
          9.6, 9.1, 9.6, 9.8, 9.0, 8.2),
  MAP = c(820, 760, 740, 700, 600, 560, 580, 540, 700, 720,
          900, 760, 720, 820, 920, 980),
  n   = c(60, 8, 180, 6, 90, 90, 6, 80, 70, 60,
          150, 90, 80, 20, 200, 260)
)

# ---- Punktwolke je Bundesland um den Schwerpunkt streuen --------------------
runs <- c("OBS_DWD_1991-2020", "RCP85_MPIWRF_2071-2100")

rows <- list()
mid  <- 70000000L
for (i in seq_len(nrow(bl_klima))) {
  b <- bl_klima[i, ]
  for (run in runs) {
    # Klimawandel-Verschiebung: waermer, etwas trockener im Sommer-Mittel
    dT <- if (grepl("RCP85", run)) 3.5 else 0
    pf <- if (grepl("RCP85", run)) 0.93 else 1
    mat <- rnorm(b$n, b$MAT + dT, 0.8)
    map <- rnorm(b$n, b$MAP * pf, 70)
    rows[[paste(b$BL, run)]] <- data.frame(
      MASTER_ID = mid + seq_len(b$n),
      BL        = b$BL,
      Zeitlauf  = run,
      MAT       = round(mat, 2),
      MAP       = round(pmax(map, 250), 0)
    )
  }
  mid <- mid + b$n
}
cloud <- do.call(rbind, rows)
rownames(cloud) <- NULL

csv <- file.path(out_dir, "climate_space_test.csv")
write.csv(cloud, csv, row.names = FALSE)
message("geschrieben: ", csv, " (", nrow(cloud), " Zeilen)")


# ---- Kontroll-Plot ----------------------------------------------------------
if (interactive() && requireNamespace("ggplot2", quietly = TRUE)) {
  source("02_function/WL_Diagramme/walther_lieth_helpers.R")  # COL_TEMP
  source("02_function/WL_Diagramme/climate_space.R")

  # zwei Beispiel-MASTER_IDs aus MV greifen
  mv_ids <- cloud$MASTER_ID[cloud$BL == "MV" &
                            cloud$Zeitlauf == "OBS_DWD_1991-2020"][c(1, 30)]

  print(plot_climate_space(
    cloud,
    master_ids   = mv_ids,
    highlight_bl = "MV",
    run          = "OBS_DWD_1991-2020"))
}
