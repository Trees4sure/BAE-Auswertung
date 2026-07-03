# =====================================================================
# Horizont_Abfolge_run.R
# ---------------------------------------------------------------------
# Beispiel-/Ausfuehrungs-Skript: liest die Horizontdaten aus den
# SQLite-Bodendatenbanken (MRS_BWI / MRS_BZE / MRS_NR) und erzeugt
# Horizontabfolge-Plots mit Standard-Farbwahl und KA5-Koernungs-Symbolen.
#
# Ausfuehren aus dem Projekt-Hauptverzeichnis (dort, wo "01_data/..." liegt).
# =====================================================================

# ---- Pakete ----------------------------------------------------------
benoetigt <- c("DBI", "RSQLite", "dplyr", "stringr", "tibble", "aqp")
fehlt <- benoetigt[!vapply(benoetigt, requireNamespace, logical(1), quietly = TRUE)]
if (length(fehlt)) {
  stop("Bitte fehlende Pakete installieren: ",
       "install.packages(c(", paste(sprintf('\"%s\"', fehlt), collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr)
  library(stringr); library(tibble); library(aqp)
})

# ---- Module laden ----------------------------------------------------
source("R/00_ka5_referenz.R")
source("R/01_db_zugriff.R")
source("R/02_horizont_abfolge_plot.R")

# ---- 0) Datenbanken finden / Struktur pruefen (einmalig hilfreich) ---
# boden_dateien()                       # gefundene SQLite-Dateien je Quelle
# db_tabellen(db_pfad("BWI"))           # -> 00_BESCHREIBUNG ... 05_QUALITAETSSCHLUESSEL
# db_spalten(db_pfad("BWI"), TAB$LEITPROFILE)   # Spalten der Leitprofil-Tabelle

# ---- 1) Leitprofile laden (Tabelle 03_LEITPROFILE) -------------------
# 03_LEITPROFILE wird ueber group_ID mit 02_KARTIEREINHEITEN verknuepft;
# SOEH_KRZ und BL werden dabei automatisch ergaenzt.
# Ersetzt frueher: DB_Lp_NR <- sqlQuery(DB_Verbindung, "... MRS_NR_03_Leitprofile")
DB_Lp_NR  <- lade_leitprofile("NR")            # ganze Tabelle inkl. SOEH_KRZ/BL
# DB_Lp_BWI <- lade_leitprofile("BWI")
# DB_Lp_BZE <- lade_leitprofile("BZE")

# Gezielt EINE Feinbodenform per SQL (schnell, entspricht der DB-Browser-Abfrage):
# bis_mv <- lade_leitprofil_fuer("BiS", region = "MV", quelle = "NR")

# ---- 2) Horizontabfolge plotten --------------------------------------
# Ein Profil:
horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "BiS", region = "MV")

# Mehrere Profile nebeneinander:
# horizont_abfolge_plot(DB_Lp_NR,
#                       soeh_krz = c("BiS", "MüS", "DüSG"),
#                       region = "MV")

# ---- 3) Legende der KA5-Koernungs-Symbole ----------------------------
# koernung_legende()

# ---- 4) Plot speichern (optional) ------------------------------------
# png("03_output/Horizontabfolge_SoS.png", width = 1200, height = 900, res = 150)
# horizont_abfolge_plot(DB_Lp_BWI, soeh_krz = "SoS", region = "MV")
# dev.off()
