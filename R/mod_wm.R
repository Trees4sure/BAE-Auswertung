# R/mod_wm.R ----
# WM-Daten: Kuerzel-Mapping, Dateisuche, Laden, Darstellung.
# Setzt voraus: WM_DIR (aus config.R), wm_felder (hier definiert)

# WM_DIR vorbelegen
if (!exists("WM_DIR")) WM_DIR <- NULL


# ============================================================
#  WM-KENNGROESSEN-MAPPING
#  Hier aendern wenn neue WM-Modelle oder Spalten hinzukommen.
#  Schema: Kuerzel = list(label = "Anzeigename", cols = c(SPALTE = "Label"))
# ============================================================

wm_felder <- list(
  SLM        = list(label = "Wachstum (SLM)",
                    cols  = c("HG100" = "Oberhöhe im Alter 100 (m)")),
  SSM        = list(label = "Sturmrisiko (SSM)",
                    cols  = c("PSTURM_SW"="Sturm schw. (%)","PSTURM_W"="Sturm stark (%)","PSTURM"="Sturm gesamt (%)")),
  BKSR       = list(label = "Borkenkäfer (BKSR)",
                    cols  = c("P_IPS_30"="Befall p30 (%)","P_IPS_70"="Befall p70 (%)","P_IPS_100"="Befall p100 (%)")),
  SWB        = list(label = "Wasserbilanz (SWB)",
                    cols  = c("SWB"="Standortwasserbilanz (mm)")),
  MM         = list(label = "Mistel (MM)",
                    cols  = c("MISTELBEFALLSRISIKO"="Befallsrisiko")),
  JSM        = list(label = "Stabilität (JSM)",
                    cols  = c("KLASSE"="Klasse","GINI_VORHERSAGE"="GINI-Vorhersage","EXTRAPOLATIONSBEREICH"="Extrapolationsbereich")),
  EVA        = list(label = "Anbauempfehlung (EVA)",
                    cols  = c("HEIGHT"="Oberhöhe in 100 J")),
  SDM        = list(label = "Artverbreitung (SDM)",
                    cols  = c("VKW"="Vorkommenswahrsch.","KLIMARISIKO"="Klimarisiko")),
  UeLZ       = list(label = "Überlebenszeit (UeLZ)",
                    cols  = c("S100"="Überlebenszeit 100 J (%)")),
  FWI        = list(label = "Feuer (FWI)",
                    cols  = c("FWI_JUNI"="Jun","FWI_JULI"="Jul","FWI_AUGUST"="Aug","FWI_SEPTEMBER"="Sep")),
  Brook90    = list(label = "Wasserhaushalt (Brook90)",
                    cols  = c("KNP"="Klimatische NFK","RELAWAT_VP"="Rel. Wasserverfügbarkeit")),
  BSO        = list(label = "BK-Saisonen (BSO)",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  ChaTmax    = list(label = "Phänologie ChaTmax",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  ChaTmean   = list(label = "Phänologie ChaTmean",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  ChaTmin    = list(label = "Phänologie ChaTmin",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  PheShaded  = list(label = "Phänologie PheShaded",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  PheSunny   = list(label = "Phänologie PheSunny",
                    cols  = c("ANZAHL_GENERATIONEN"="Generationen/Jahr")),
  BioC       = list(label = "Bioökonomie (BioC)",
                    cols  = c("ROTATION_TIME"="Umtriebszeit (J)","MEAN_ROTATION_TIME"="Umtriebszeit NR (J)",
                              "SITE_INDEX_CM"="Bonität (cm/J)","MEAN_SITE_INDEX_CM"="Bonität NR (cm/J)")),
  ksJWM      = list(label = "Jahrringbreite (ksJWM)",
                    cols  = c("RING_WIDTH"="Jahrringbreite")),
  PASFi      = list(label = "Schadensrisiko (PASFi)",
                    cols  = c("WINDPRAED"="Wind","SCHNEEPRAED"="Schnee","IPSPRAED"="Borkenkäfer","GESAMTRISK"="Gesamtrisiko"))
)

# ============================================================
#  WM-ORDNER-ALIAS
#  Trage hier manuelle Zuordnungen ein wenn automatische Suche scheitert.
#  Leer lassen solange der Ordnername das Kuerzel enthaelt.
#  Realer Ordnerbestand (Ergebnisse_BAE_WM, Stand 2026-06-12):
#  WM_AP11_SLM, WM_AP11_SSM, WM_AP12_BKSR, WM_AP14_SWB, WM_AP21_MM,
#  WM_AP22_JSM, WM_AP24_EVA, WM_AP41_SDM, WM_AP42_SLM, WM_AP42_UeLZ,
#  WM_AP44_FWI, WM_AP45_Ana, WM_AP53_Brook90, WM_AP54_BSO, WM_AP54_CHAPY,
#  WM_AP54_Phenips, WM_AP58_BioC, WM_AP921_ksJWM
# ============================================================

wm_ordner_alias <- list(
  "SLM"      = "WM_AP11_SLM",    # nicht WM_AP42_SLM (anderer Kennwert, gleiches Kuerzel)
  "ChaTmax"  = "WM_AP54_CHAPY",
  "ChaTmean" = "WM_AP54_CHAPY",
  "ChaTmin"  = "WM_AP54_CHAPY",
  "PheShaded" = "WM_AP54_Phenips",
  "PheSunny"  = "WM_AP54_Phenips"
)

# ── WM-Ordner finden ----

wm_find_ap_dir <- function(wm_ap) {
  if (is.null(WM_DIR) || !dir.exists(WM_DIR)) return(NULL)
  
  if (!is.null(wm_ordner_alias[[wm_ap]])) {
    cand <- file.path(WM_DIR, wm_ordner_alias[[wm_ap]])
    if (dir.exists(cand)) return(cand)
    # Alias-Ordner existiert hier nicht (z.B. lokale Testdaten) ->
    # weiter mit der regulaeren Kuerzel-Suche unten.
  }
  
  all_dirs <- list.dirs(WM_DIR, recursive = FALSE, full.names = TRUE)
  if (length(all_dirs) == 0) return(NULL)
  bn      <- basename(all_dirs)
  matches <- which(
    grepl(paste0("_", wm_ap, "$"),  bn, fixed = FALSE) |
      grepl(paste0("_", wm_ap, "_"),  bn, fixed = FALSE) |
      bn == wm_ap
  )
  if (length(matches) == 0) return(NULL)
  all_dirs[matches[1]]
}

# ── Dateiname-Pattern zusammenbauen ----

wm_build_filename_pattern <- function(region, szenario, modell, zeitraum, baumart) {
  paste0("_", region, "_", szenario, "_", modell, "_", zeitraum, "_", baumart,
         "\\.csv(\\.gz)?$")
}

# Bei Ordnern mit mehreren Kennwerten (z.B. WM_AP54_CHAPY enthaelt ChaTmax/
# ChaTmean/ChaTmin) auf eine Datei mit dem Kuerzel im Namen einschraenken,
# falls vorhanden - sonst erste Trefferdatei verwenden.
wm_pick_file <- function(files, kuerzel) {
  if (length(files) <= 1) return(files[1])
  hit <- grep(kuerzel, basename(files), ignore.case = TRUE, fixed = TRUE)
  if (length(hit) > 0) files[hit[1]] else files[1]
}

# ── WM-Dateien finden (schnell) ----
# In-Memory-Index je AP-Ordner: einmal pro Session rekursiv listen, danach
# nur noch im Speicher per grep filtern. Verhindert die wiederholte (teure)
# rekursive Verzeichnissuche ueber das Netzlaufwerk bei jedem Punkt-Klick.
wm_dir_index <- local({
  cache <- new.env(parent = emptyenv())
  function(ap_dir) {
    if (is.null(cache[[ap_dir]]))
      cache[[ap_dir]] <- list.files(ap_dir, pattern = "\\.csv(\\.gz)?$",
                                    full.names = TRUE, recursive = TRUE)
    cache[[ap_dir]]
  }
})

# Dateien einer AP/Kombination finden: erst optimistisch den (BAE-aehnlichen)
# Unterordner Szen/Modell/Zeitraum direkt ansteuern - existiert er, ist sein
# Ergebnis verbindlich (leer = Datei gibt es nicht), kein teurer Vollscan.
# Nur wenn keine bekannte Struktur passt, auf den gecachten Rekursiv-Index
# zurueckfallen.
wm_find_files <- function(ap_dir, pattern, region, szenario, modell, zeitraum) {
  # Bestaetigte Struktur: {AP}/{Region}/{Szen}/{Modell}/{Zeitraum}/datei.csv
  # (Region = "BWI-BZE" oder "NR01".."NR11"). Direkt ansteuern -> kein Vollscan.
  cand <- c(file.path(ap_dir, region, szenario, modell, zeitraum),
            file.path(ap_dir, szenario, modell, zeitraum))   # ohne Region-Ebene
  for (leaf in cand) {
    if (dir.exists(leaf))
      return(list.files(leaf, pattern = pattern, full.names = TRUE,
                        recursive = TRUE, ignore.case = TRUE))
  }
  # Struktur unerwartet -> gecachter Rekursiv-Index (einmalig teuer, dann schnell)
  grep(pattern, wm_dir_index(ap_dir), value = TRUE, ignore.case = TRUE)
}

# ── WM-CSV laden ----

wm_load_csv <- function(ap_dir, baumart, region, szenario, modell, zeitraum) {
  pattern <- wm_build_filename_pattern(region, szenario, modell, zeitraum, baumart)
  files   <- wm_find_files(ap_dir, pattern, region, szenario, modell, zeitraum)
  if (length(files) == 0) return(NULL)
  d <- tryCatch(data.table::fread(files[1], data.table = FALSE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  names(d) <- toupper(names(d))
  d$Baumart <- baumart
  d
}

# ── WM-Spalte aufbereiten ----

wm_aufbereiten_spalte <- function(df, col_name) {
  col_name <- toupper(col_name)
  if (!col_name %in% names(df)) return(NULL)
  vals <- as.numeric(df[[col_name]])
  if (col_name == "HG100") vals <- vals / 10   # dm -> m
  df$WM_VAL <- vals
  df[!is.na(df$WM_VAL), ]
}

# ── get_wm: WM-Werte fuer einen Punkt laden ----
# Cache-Strategie: pro AP/Region/Szenario/Modell/Zeitraum/Baumart wird die
# komplette CSV (alle MASTER_IDs) einmalig in eine indizierte SQLite-Tabelle
# (wm_cache_<Kuerzel>) importiert. wm_cache_meta haelt Quellpfad + mtime fest;
# aendert sich die Quelldatei, wird die Kombination neu importiert. Ein
# Cache-Treffer vermeidet sowohl die rekursive Verzeichnissuche als auch das
# Einlesen der (potenziell grossen) CSV ueber das Netzlaufwerk.

get_wm <- function(master_id, baumart, szenario, modell, zeitraum,
                   region = "BWI-BZE") {
  if (is.null(WM_DIR) || !dir.exists(WM_DIR)) {
    message("WM: WM_DIR nicht verfuegbar (",
            if (is.null(WM_DIR)) "NULL" else WM_DIR, ") - keine WM-Daten.")
    return(NULL)
  }
  
  region_str <- if (region == "BWI-BZE") "BWI-BZE" else region
  t_gesamt   <- Sys.time()
  
  res <- lapply(names(wm_felder), function(kuerzel) {
    t0   <- Sys.time()
    secs <- function() round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    
    ap_dir <- wm_find_ap_dir(kuerzel)
    if (is.null(ap_dir))
      return(list(row = NULL, log = paste0(kuerzel, ": kein Ordner in ", WM_DIR)))
    
    meta   <- wm_cache_meta_get(kuerzel, region_str, szenario, modell, zeitraum, baumart)
    quelle <- NULL
    mtime  <- NULL
    
    if (!is.null(meta) && file.exists(meta$source_path)) {
      quelle <- meta$source_path
      mtime  <- as.character(file.info(quelle)$mtime)
      if (identical(mtime, meta$source_mtime)) {
        row <- wm_cache_query(kuerzel, region_str, szenario, modell, zeitraum, baumart, master_id)
        if (is.null(row))
          return(list(row = NULL, log = paste0(
            kuerzel, ": CACHE-HIT, aber MASTER_ID nicht in Tabelle (", secs(), "s)")))
        row$WM_KUERZEL <- kuerzel
        return(list(row = row, log = paste0(kuerzel, ": CACHE-HIT (", secs(), "s)")))
      }
    }
    
    # Cache fehlt oder Quelldatei hat sich geaendert -> Datei suchen + neu einlesen
    if (is.null(quelle)) {
      pattern <- wm_build_filename_pattern(region_str, szenario, modell, zeitraum, baumart)
      files   <- wm_find_files(ap_dir, pattern, region_str, szenario, modell, zeitraum)
      if (length(files) == 0)
        return(list(row = NULL, log = paste0(
          kuerzel, ": KEINE DATEI zu Pattern '", pattern, "' unter ",
          ap_dir, " (", secs(), "s)")))
      quelle <- wm_pick_file(files, kuerzel)
      mtime  <- as.character(file.info(quelle)$mtime)
    }
    
    df <- tryCatch(data.table::fread(quelle, data.table = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0)
      return(list(row = NULL, log = paste0(
        kuerzel, ": Datei leer/unlesbar: ", quelle, " (", secs(), "s)")))
    names(df) <- toupper(names(df))
    if (!"MASTER_ID" %in% names(df))
      return(list(row = NULL, log = paste0(
        kuerzel, ": Spalte MASTER_ID fehlt in ", basename(quelle))))
    
    # Deutsches Dezimalkomma in Zahlenspalten reparieren (z.B. "10,5" -> 10.5),
    # falls fread die Spalte wegen Komma als Text eingelesen hat.
    for (cn in setdiff(names(df), "MASTER_ID")) {
      if (is.character(df[[cn]])) {
        conv <- suppressWarnings(as.numeric(gsub(",", ".", df[[cn]], fixed = TRUE)))
        if (!all(is.na(conv))) df[[cn]] <- conv
      }
    }
    # GROSSBUCHSTABEN erzwingen – konsistent mit names(df) <- toupper(names(df))
    # oben. Kleinschreibung (z.B. df$Modell) wuerde neben einem bereits
    # vorhandenen "MODELL"-Feld eine zweite Spalte erzeugen;
    # SQLite ist case-insensitiv -> "duplicate column name: Modell"-Fehler.
    df[["REGION"]]   <- region_str
    df[["SZENARIO"]] <- szenario
    df[["MODELL"]]   <- modell
    df[["ZEITRAUM"]] <- zeitraum
    df[["BAUMART"]]  <- baumart
    
    # Stabiles Spaltenset je Kuerzel -> einheitliche Cache-Tabelle ueber alle
    # Regionen (z.B. NR-JSM ohne KLASSE/EXTRAPOLATIONSBEREICH). Fehlende
    # Soll-Spalten als NA ergaenzen, ueberzaehlige verwerfen.
    soll_cols <- names(wm_felder[[kuerzel]]$cols)
    for (sc in setdiff(soll_cols, names(df))) df[[sc]] <- NA
    df <- df[, c("MASTER_ID", soll_cols,
                 "REGION", "SZENARIO", "MODELL", "ZEITRAUM", "BAUMART"),
             drop = FALSE]
    
  tryCatch(
      wm_cache_import(kuerzel, region_str, szenario, modell, zeitraum, baumart,
                      df, quelle, mtime),
      error = function(e)
        message(kuerzel, ": Cache-Import \u00fcbersprungen (Schema-Drift): ", e$message))
    
    row     <- df[df$MASTER_ID == master_id, , drop = FALSE]
    log_txt <- paste0(kuerzel, ": IMPORT ", nrow(df), " Zeilen aus ",
                      basename(quelle), " (", secs(), "s)")
    if (nrow(row) == 0)
      return(list(row = NULL, log = paste0(log_txt, " - MASTER_ID nicht enthalten")))
    row <- row[1, , drop = FALSE]
    row$WM_KUERZEL <- kuerzel
    list(row = row, log = log_txt)
  })
  
  logs <- vapply(res, function(x) x$log, character(1))
  message("WM [", master_id, " | ", baumart, " | ", region_str, " | ",
          szenario, "/", modell, "/", zeitraum, "] gesamt ",
          round(as.numeric(difftime(Sys.time(), t_gesamt, units = "secs")), 1),
          "s\n  ", paste(logs, collapse = "\n  "))
  
  result_list <- Filter(Negate(is.null), lapply(res, function(x) x$row))
  if (length(result_list) == 0) return(NULL)
  data.table::rbindlist(result_list, fill = TRUE, use.names = TRUE) %>% as.data.frame()
}

# ── render_wm_block: HTML fuer Boden-Panel ----

render_wm_block <- function(wm_df) {
  if (is.null(wm_df) || nrow(wm_df) == 0) {
    return(shiny::tags$div(
      style = "flex:0 0 240px; min-width:200px;",
      shiny::tags$h6(style = "color:#795548; margin:0 0 6px; font-weight:bold;",
                     "\u25B6 Wirkmodelle"),
      shiny::tags$div(style = "font-size:11px; color:#999; font-style:italic;",
                      "Keine WM-Daten f\u00fcr diesen Punkt / Klimalauf.")))
  }
  zeilen <- lapply(names(wm_felder), function(kuerzel) {
    info    <- wm_felder[[kuerzel]]
    wm_rows <- wm_df[wm_df$WM_KUERZEL == kuerzel, , drop = FALSE]
    if (nrow(wm_rows) == 0) return(NULL)
    werte <- lapply(names(info$cols), function(col) {
      if (!col %in% names(wm_rows)) return(NULL)
      val <- wm_rows[[col]][1]
      if (is.na(val)) return(NULL)
      val_str <- if (col == "HG100" && is.numeric(val)) paste0(round(val/10,1), " m")
      else if (is.numeric(val)) round(val, 2)
      else as.character(val)
      shiny::tags$div(style = "font-size:11px; margin-bottom:1px; line-height:1.35;",
                      shiny::tags$span(style = "color:#666; min-width:140px; display:inline-block;",
                                       paste0(info$cols[[col]], ":")),
                      shiny::tags$span(style = "font-weight:500;", val_str))
    })
    werte <- Filter(Negate(is.null), werte)
    if (length(werte) == 0) return(NULL)
    shiny::tagList(
      shiny::tags$div(style = "font-size:10px; font-weight:700; text-transform:uppercase;
                               letter-spacing:.05em; color:#795548; margin:5px 0 2px 0;",
                      info$label),
      werte)
  })
  zeilen <- Filter(Negate(is.null), zeilen)
  shiny::tags$div(
    style = "flex:0 0 240px; min-width:200px;",
    shiny::tags$h6(style = "color:#795548; margin:0 0 6px; font-weight:bold;",
                   "\u25B6 Wirkmodelle"),
    if (length(zeilen) > 0) zeilen
    else shiny::tags$div(style = "font-size:11px; color:#999; font-style:italic;",
                         "Keine WM-Felder gefunden."))
}

# ── WM-Initialisierung ----

init_wm <- function(wm_dir = NULL) {
  WM_DIR <<- wm_dir
  message("WM-Pfad: ", if (!is.null(WM_DIR)) WM_DIR else "nicht konfiguriert")
  invisible(WM_DIR)
}