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


# ── 5. Datenverbindungs-Log ─────────────────────────────────
#
# Zweck: nachvollziehen, WOHER die Daten einer exportierten Grafik/Tabelle
# stammen (Anlass 2026-07: falsche Filter-Zuordnung im Analyse-Tab).
# Geschrieben wird NUR beim Klick auf einen Export-/Download-Button, dann aber
# zweifach:
#   1) zentrale Sammeldatei  -> <result_dir>/BAE_Auswertung/logs/datenverbindungen_<Datum>.log
#   2) optionales Sidecar    -> <export_datei>.log direkt neben der abgelegten
#      Datei (nur bei serverseitig geschriebenen Dateien; Browser-Downloads
#      landen im Download-Ordner des Nutzers, den die App nicht kennt -> dort
#      nur der zentrale Eintrag).
#
# herkunft: Liste aus $inputs (benannte Filterwerte), $quellen (Vektor der real
# gelesenen Dateipfade), optional $fallback (rekursiver NR-Scan aktiv?) und $n
# (Zeilen im Datensatz). Wird an den Ladestellen in app.R befuellt.
schreibe_datenlog <- function(export_name, tab, herkunft = NULL,
                              zentral_dir, sidecar_datei = NULL) {
  inp <- if (!is.null(herkunft)) herkunft$inputs else NULL
  q   <- if (!is.null(herkunft)) herkunft$quellen else character(0)
  block <- c(
    strrep("=", 72),
    paste0("Zeit        : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Export      : ", export_name),
    paste0("Tab/Quelle  : ", tab),
    "Filter:",
    if (length(inp))
      paste0("    ", format(names(inp), width = 12), "= ",
             vapply(inp, function(x) paste(x, collapse = ", "), character(1)))
    else "    (keine erfasst)",
    paste0("Gelesene Dateien (", length(q), "):"),
    if (length(q)) paste0("    - ", q) else "    (keine erfasst)",
    if (isTRUE(herkunft$fallback))
      "    ! REKURSIVER FALLBACK aktiv - Datei-Zuordnung pruefen!" else NULL,
    if (!is.null(herkunft$n)) paste0("Zeilen im Datensatz : ", herkunft$n) else NULL,
    ""
  )
  txt <- paste0(paste(block, collapse = "\n"), "\n")

  # 1) zentrale Sammeldatei (pro Tag eine)
  tryCatch({
    dir.create(zentral_dir, recursive = TRUE, showWarnings = FALSE)
    cat(txt, append = TRUE,
        file = file.path(zentral_dir,
                         paste0("datenverbindungen_", format(Sys.Date(), "%Y%m%d"), ".log")))
  }, error = function(e) message("Datenlog (zentral) fehlgeschlagen: ", conditionMessage(e)))

  # 2) Sidecar neben der abgelegten Datei
  if (!is.null(sidecar_datei) && dir.exists(dirname(sidecar_datei)))
    tryCatch(cat(txt, file = paste0(sidecar_datei, ".log"), append = FALSE),
             error = function(e) message("Datenlog (Sidecar) fehlgeschlagen: ", conditionMessage(e)))

  invisible(txt)
}


