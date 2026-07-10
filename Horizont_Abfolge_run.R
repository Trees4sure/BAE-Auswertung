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
source("02_function/Horizont_mean_function/R/00_ka5_referenz.R")
source("02_function/Horizont_mean_function/R/01_db_zugriff.R")
source("02_function/Horizont_mean_function/R/02_horizont_abfolge_plot.R")
source("02_function/Horizont_mean_function/R/03_leitprofil_streuung_plot.R")

# ---- 0) Datenbanken finden / Struktur pruefen (einmalig hilfreich) ---
boden_dateien()                        # gefundene SQLite-Dateien je Quelle
db_tabellen(db_pfad("BWI"))            # -> 00_BESCHREIBUNG ... 05_QUALITAETSSCHLUESSEL
db_spalten(db_pfad("BWI"), TAB$LEITPROFILE)   # Spalten der Leitprofil-Tabelle

db_tabellen(db_pfad("NR"))             # Tabellen der NR-Datenbank
db_spalten(db_pfad("NR"), TAB$LEITPROFILE)    # Spalten der Leitprofil-Tabelle NR

# ---- 1) Leitprofile laden (Tabelle 03_LEITPROFILE) -------------------
# 03_LEITPROFILE wird ueber group_ID mit 02_KARTIEREINHEITEN verknuepft;
# SOEH_KRZ und BL werden dabei automatisch ergaenzt.
# Ersetzt frueher: DB_Lp_BWI <- sqlQuery(DB_Verbindung, "... MRS_BWI_03_Leitprofile")
DB_Lp_BWI <- lade_leitprofile("BWI")
DB_Lp_NR  <- lade_leitprofile("NR")
DB_Lp_BZE <- lade_leitprofile("BZE")

# Alle alten DB-Objekte auf einmal (Namen wie im SQL-Server-Skript):
# db <- lade_alle_db()
# str(db, max.level = 1)

# ---- 2) Horizontabfolge plotten --------------------------------------
# Ohne region -> ueber ALLE Regionen suchen und plotten:
horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "DüSG")

# Auf eine Region beschraenken:
# horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "DüSG", region = "MV")

# Kombiform: faellt bei Bedarf auf die Teilformen (MüS und/oder BiS) zurueck:
# horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "MüS/BiS")

# Mehrere Profile nebeneinander:
horizont_abfolge_plot(DB_Lp_BWI,
                      soeh_krz = c("BiS", "AhLG", "BäS", "WnS", "DgL"),
                      region = "MV")

# ---- 2a) Leitprofil-Kennwerte (Kornanteile + Chemie) -----------------
# Benoetigt zusaetzlich: ggplot2, tidyr, patchwork, cowplot
# Variante 1: aqp-Profil UND Streuung seitlich daneben in einer Abbildung:
# horizont_mit_streuung(DB_Lp_NR, soeh_krz = "DüSG")
#
# Variante 2: Streuung eigenstaendig (Korn in Erdtoenen, Chemie getrennt):
# print(leitprofil_streuung_plot(DB_Lp_NR, soeh_krz = "DüSG"))
#
# Nur die aggregierten Kennwerte je Horizont ansehen (zum Debuggen):
# leitprofil_kennwerte(DB_Lp_NR, soeh_krz = "DüSG")

# ---- 2b) Horizontabfolge direkt ueber eine MASTER_ID -----------------
# Quelle wird aus dem Praefix (NR_/BWI_/BZE_) erkannt, sonst alle durchsucht.
horizont_abfolge_plot_master("NR_130_08_66519")
# lade_leitprofil_master("NR_130_08_66519")   # nur laden (mit Quelle-Attribut)

# ---- 3) Legende der KA5-Koernungs-Symbole ----------------------------
# koernung_legende()

# ---- 4) Profile als PNG speichern ------------------------------------

# --- BWI/BZE (Quelle je ID aus dem Praefix erkannt) ---
dir.create("04_results/Horizonte/BWI", recursive = TRUE, showWarnings = FALSE)

BWI_ids <- c("BZE_80220", "BZE_90850", "BZE_30051", "BZE_120049", "BWI_090_16111_2",
             "BZE_90742", "BZE_10008", "BZE_120008", "BZE_80255", "BZE_80099",
             "BZE_90636", "BZE_30008", "BWI_080_4094_1", "BWI_090_13799_1",
             "BWI_080_640_1", "BZE_30006")

for (id in BWI_ids) {
  png(paste0("04_results/Horizonte/BWI/Horizont_", id, ".png"),
      width = 1200, height = 900, res = 150)
  try(horizont_abfolge_plot_master(id))   # try(): eine fehlende ID stoppt die Schleife nicht
  dev.off()
}

# --- NR ---
dir.create("04_results/Horizonte/NR", recursive = TRUE, showWarnings = FALSE)

NR_ids <- c("NR_130_08_66519", "NR_130_08_6189")

for (id in NR_ids) {
  png(paste0("04_results/Horizonte/NR/Horizont_", id, ".png"),
      width = 1200, height = 900, res = 150)
  try(horizont_abfolge_plot_master(id, quelle = "NR"))
  dev.off()
}
