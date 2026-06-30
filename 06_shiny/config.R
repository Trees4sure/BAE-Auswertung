# ============================================================
#  config.R  - Pfade und Laufwerks-Erkennung
#  Wird von app.R beim Start automatisch gesourct.
# ============================================================

# ── 1. Lokales Projektverzeichnis (relativ zur app.R) ───────
LOCAL_DIR <- "../../"
LOCAL_DIR <- sub("/*$", "", LOCAL_DIR)   # -> "../.."

# options(
#   shiny.error          = browser,  # bei jedem unabgefangenen Fehler in den Debugger
#   shiny.fullstacktrace = TRUE,     # vollständiger Stacktrace statt gekürzt
#   shiny.trace          = FALSE     # bei Bedarf TRUE: alle Client/Server-Messages loggen
# )

# ── 2. Externe Datenpfade: automatische Laufwerkserkennung ──
#
#  Priorität:  Workstation (S:\)  >  SSD (E:\)  >  NULL
#
#  S:\  = Netzlaufwerk Workstation
#  E:\  = Externe SSD
#
#  BAE_WM_DIR  -> Wurzel der BAE/NR-Rohdaten
#  WM_DIR      -> Wurzel der WM-Ergebnisse (oft identisch)

WS_BAE <- "S:/00_Benutzer/01_Projekte/02_MultiRiskSuit/Projektergebnisse_Laender/Ergebnisse_BAE"
WS_WM  <- "S:/00_Benutzer/01_Projekte/02_MultiRiskSuit/Projektergebnisse_Laender/Ergebnisse_WM"

SSD_ROOT <- "E:/MRS_Dateien/00_MultiRiskSuit_Endergebnisse/Ergebnisse_BAE_WM"

if (dir.exists(WS_BAE)) {
  
  # ── Workstation (S:\) erreichbar ──────────────────────────
  BAE_WM_DIR <- WS_BAE
  WM_DIR     <- WS_WM
  message("\u2705 Workstation (S:\\) erkannt - externe Daten verfuegbar.")
  
} else if (dir.exists(SSD_ROOT)) {
  
  # ── SSD (E:\) erreichbar ──────────────────────────────────
  # SSD_ROOT ("Ergebnisse_BAE_WM") enthaelt - wie auf der Workstation -
  # zwei getrennte Unterordner: "Ergebnisse_BAE" (BAE-CSVs) und
  # "Ergebnisse_WM" (WM_AP*-Ordner). Fallback auf SSD_ROOT selbst, falls
  # diese Unterordner einmal nicht existieren sollten.
  SSD_BAE_SUB <- file.path(SSD_ROOT, "Ergebnisse_BAE")
  SSD_WM_SUB  <- file.path(SSD_ROOT, "Ergebnisse_WM")
  BAE_WM_DIR  <- if (dir.exists(SSD_BAE_SUB)) SSD_BAE_SUB else SSD_ROOT
  WM_DIR      <- if (dir.exists(SSD_WM_SUB))  SSD_WM_SUB  else SSD_ROOT
  message("\u2705 SSD (E:\\) erkannt - externe Daten verfuegbar.")
  
} else {
  
  # ── Kein externes Laufwerk erreichbar ─────────────────────
  BAE_WM_DIR <- NULL
  WM_DIR     <- NULL
  message("\u26A0\uFE0F  Kein externes Laufwerk gefunden (weder S:\\ noch E:\\).")
  message("   NR-Rohdaten und WM nicht verfuegbar.")
  message("   BWI-Betrieb laeuft normal weiter.")
  message("   -> SSD anschliessen oder Netzlaufwerk verbinden fuer volle Funktionalitaet.")
  
}

# ── 3. Zusammenfassung beim Start ───────────────────────────
message("---")
message("LOCAL_DIR  : ", LOCAL_DIR)
message("BAE_WM_DIR : ", if (!is.null(BAE_WM_DIR)) BAE_WM_DIR else "(nicht verfuegbar)")
message("WM_DIR     : ", if (!is.null(WM_DIR))     WM_DIR     else "(nicht verfuegbar)")
message("---")

# ── 4. Lokale Analysepfade (BZT, Klimakarten) ───────────────
#
#  Diese Verzeichnisse liegen lokal unter LOCAL_DIR und müssen
#  nicht auf externen Laufwerken verfügbar sein.

# sub("/*$","") entfernt ggf. trailing slash aus LOCAL_DIR ("../../" -> "../..")

BZT_CSV_DIR <- normalizePath(file.path(LOCAL_DIR, "01_data/Ergebnisse_BAE/BZT_BAE_Zuordnung"),
                             mustWork = FALSE)
KS_IMG_DIR  <- normalizePath(file.path(LOCAL_DIR, "01_data/Ergebnisse_BAE/BZT_Klimastufeneinteilung"),
                             mustWork = FALSE)

message("BZT_CSV_DIR: ", if (dir.exists(BZT_CSV_DIR)) BZT_CSV_DIR else paste0("(nicht gefunden) ", BZT_CSV_DIR))
message("KS_IMG_DIR : ", if (dir.exists(KS_IMG_DIR))  KS_IMG_DIR  else paste0("(nicht gefunden) ", KS_IMG_DIR))
message("---")


