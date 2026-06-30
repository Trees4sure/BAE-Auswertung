# R/mod_boden.R ----
# Enthaelt alle Boden- und BZT-Funktionen.
# Abhaengigkeiten: BWI_Boden, BZE_Boden, NR_Boden (aus app.R/config.R)
#                  Boden_BZT.dir.BWIBZE, Boden_BZT.dir.NR (hier initialisiert)

# Globale Variablen als NULL vorbelegen, damit get_boden() sie findet
# auch bevor init_boden_schema_cache() aufgerufen wurde.
if (!exists("boden_schema_cache")) boden_schema_cache <- NULL
if (!exists("Boden_BZT.dir.BWIBZE")) Boden_BZT.dir.BWIBZE <- NULL
if (!exists("Boden_BZT.dir.NR"))     Boden_BZT.dir.NR     <- NULL

# ── Hilfsfunktionen ──────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

fmt_n <- function(n) {
  # Apostroph als Tausendertrennzeichen – kein Konflikt mit dt. Dezimalzeichen
  formatC(as.integer(n), format = "d", big.mark = "'")
}

# ── Schema-Cache (einmalig beim Start) ───────────────────────
# Vermeidet PRAGMA table_info-Abfragen bei jedem Klick

init_boden_schema_cache <- function(bwi_path = NULL, bze_path = NULL, nr_path = NULL) {
  cache_db <- function(db_path) {
    if (is.null(db_path) || length(db_path) == 0 || !file.exists(db_path[1]))
      return(NULL)
    con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db_path[1]),
                    error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(DBI::dbDisconnect(con))
    list(
      kat = tryCatch(
        DBI::dbGetQuery(con, "PRAGMA table_info('02_KARTIEREINHEITEN')")$name,
        error = function(e) character(0)),
      lp  = tryCatch(
        DBI::dbGetQuery(con, "PRAGMA table_info('03_LEITPROFILE')")$name,
        error = function(e) character(0))
    )
  }
  # Fallback: nimm globale BWI_Boden wenn kein expliziter Pfad uebergeben
  bwi <- bwi_path %||% grep("MRS_BWI\\.sqlite",
                            get0("BWI_Boden", envir = .GlobalEnv, inherits = FALSE),
                            value = TRUE)
  cache <- list(
    BWI = cache_db(bwi),
    BZE = cache_db(bze_path),
    NR  = cache_db(nr_path)
  )
  message("Boden-Schema gecacht: BWI=",
          length(cache$BWI$kat), " Kat-Spalten | ",
          length(cache$BWI$lp),  " LP-Spalten")
  cache
}

# ── BZT-Zuordnungsdaten laden (einmalig beim Start) ──────────

init_boden_bzt <- function(bzt_dir = "../../01_data/BZT_Mergings") {
  bzt_join_cols <- c("MASTER_ID", "STAOTYP", "SOEH_LNG", "NKST",
                     "Sub_feu_stf_back", "STGR_final")
  
  bwi <- tryCatch(
    readxl::read_xlsx(file.path(bzt_dir, "MRS_BZT.BWI_Zuordnung.xlsx")),
    error = function(e) { message("BZT BWI nicht geladen: ", e$message); NULL })
  bze <- tryCatch(
    readxl::read_xlsx(file.path(bzt_dir, "MRS_BZT.BZE_Zuordnung.xlsx")),
    error = function(e) { message("BZT BZE nicht geladen: ", e$message); NULL })
  nr  <- tryCatch(
    readxl::read_xlsx(file.path(bzt_dir, "MRS_BZT.NR_Zuordnung.xlsx")),
    error = function(e) { message("BZT NR nicht geladen:  ", e$message); NULL })
  
  if (!is.null(bze)) bze$NAEHR <- as.numeric(bze$NAEHR)
  
  bwibze <- if (!is.null(bwi) && !is.null(bze)) {
    dplyr::bind_rows(bwi, bze) %>%
      dplyr::select(dplyr::all_of(intersect(bzt_join_cols, names(.))))
  } else NULL
  
  nr_tbl <- if (!is.null(nr)) {
    nr %>% dplyr::select(dplyr::all_of(intersect(bzt_join_cols, names(nr))))
  } else NULL
  
  message("Boden-BZT-Join BWI+BZE: ", if (!is.null(bwibze)) nrow(bwibze) else 0, " Zeilen")
  message("Boden-BZT-Join NR:      ", if (!is.null(nr_tbl)) nrow(nr_tbl)  else 0, " Zeilen")
  
  list(BWIBZE = bwibze, NR = nr_tbl)
}

# Erste Standortform vor dem "/" einer kombinierten group_ID.
# "MV_MüS/BiS_1" -> "MV_MüS_1". Der Teil hinter "/" steht fuer die tieferen
# Schichten und darf fachlich NICHT gemittelt werden -> nur die Oberboden-Form
# dient als Ersatzprofil.
erste_soehform <- function(gid) {
  if (is.na(gid) || !grepl("/", gid, fixed = TRUE)) return(gid)
  teile <- strsplit(gid, "_", fixed = TRUE)[[1]]
  idx   <- grep("/", teile, fixed = TRUE)[1]
  teile[idx] <- sub("/.*$", "", teile[idx])
  paste(teile, collapse = "_")
}

# ── get_boden: Bodendaten fuer eine MASTER_ID laden ──────────

get_boden <- function(master_id, region = "BWI") {
  mid_str <- as.character(master_id)
  all_boden <- get0("BWI_Boden", envir = .GlobalEnv) %||% character(0)
  
  # DB-Datei und Schema auswaehlen
  if (region == "NR") {
    db     <- get0("NR_Boden",  envir = .GlobalEnv) %||% character(0)
    schema <- boden_schema_cache$NR
    
  } else if (startsWith(mid_str, "BZE")) {
    # BZE: eigene Variable, Fallback: suche MRS_BZE in BWI_Boden-Liste
    bze_direct <- get0("BZE_Boden", envir = .GlobalEnv) %||% character(0)
    db <- if (length(bze_direct) > 0 && file.exists(bze_direct[1])) {
      bze_direct
    } else {
      grep("MRS_BZE\\.sqlite", all_boden, value = TRUE)
    }
    schema <- boden_schema_cache$BZE %||% boden_schema_cache$BWI
    
  } else {
    db     <- grep("MRS_BWI\\.sqlite", all_boden, value = TRUE)
    schema <- boden_schema_cache$BWI
  }
  
  if (length(db) == 0 || !file.exists(db[1])) {
    message("Bodendatenbank nicht gefunden",
            " | MASTER_ID: ", mid_str,
            " | Region: ",    region,
            " | Gesucht: ",   if (length(db) > 0) db[1] else "(kein Pfad)")
    return(NULL)
  }
  
  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db[1]),
                  error = function(e) { message("DB: ", e$message); NULL })
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA encoding = 'UTF-8'")
  
  # Gecachte Spaltenlisten (kein PRAGMA-Roundtrip mehr)
  kat_cols_db <- if (!is.null(schema)) schema$kat else character(0)
  lp_cols_db  <- if (!is.null(schema)) schema$lp  else character(0)
  if (length(kat_cols_db) == 0)
    kat_cols_db <- tryCatch(
      DBI::dbGetQuery(con, "PRAGMA table_info('02_KARTIEREINHEITEN')")$name,
      error = function(e) character(0))
  if (length(lp_cols_db) == 0)
    lp_cols_db <- tryCatch(
      DBI::dbGetQuery(con, "PRAGMA table_info('03_LEITPROFILE')")$name,
      error = function(e) character(0))
  
  kat_gewuenscht <- c("MASTER_ID","BODTYP","NAEHR","WASSER","STAOAGG",
                      "SOEH_KRZ","SOEH_LNG","NFK_DEHNER_AUFLAGE",
                      "NFK_DEHNER","Tmax_NFK","group_ID")
  kat_select   <- intersect(kat_gewuenscht, kat_cols_db)
  hat_group_id <- "group_ID" %in% kat_select
  
  df_kat <- tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM '02_KARTIEREINHEITEN' WHERE MASTER_ID = '%s'",
      paste(kat_select, collapse = ", "), master_id)),
    error = function(e) { message("Kartiereinheiten: ", e$message); data.frame() })
  
  if (nrow(df_kat) == 0) return(NULL)
  if (!hat_group_id || length(lp_cols_db) == 0) return(df_kat)
  
  lp_gewuenscht <- c("group_ID","LP_ID","HORIZONT","STRATI","BODART",
                     "TIEFE_OG","TIEFE_UG","SAND","SCHLUFF","TON",
                     "FEINSAND","MITTELSAND","GROBSAND","SKELETT",
                     "TRD","SOC","CARBONAT","BASEN","CN",
                     "GRUNDH20","STAUH20","NFK_DEHNER")
  lp_select <- intersect(lp_gewuenscht, lp_cols_db)
  # group_ID-Aufloesung mit Fallback: kombinierte Form ohne eigenes Leitprofil
  # -> erste Standortform vor "/" verwenden. lp_ersatz markiert das fuer die Anzeige.
  kandidaten <- unique(c(df_kat$group_ID,
                         vapply(df_kat$group_ID, erste_soehform, character(1))))
  vorhanden  <- tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT group_ID FROM '03_LEITPROFILE' WHERE group_ID IN (%s)",
      paste0("'", kandidaten, "'", collapse = ", ")))$group_ID,
    error = function(e) character(0))
  
  df_kat$lookup_group_ID <- vapply(df_kat$group_ID, function(g) {
    if (g %in% vorhanden) g
    else { e <- erste_soehform(g); if (e %in% vorhanden) e else g }
  }, character(1))
  df_kat$lp_ersatz <- !is.na(df_kat$lookup_group_ID) &
    df_kat$lookup_group_ID != df_kat$group_ID
  
  ids <- paste0("'", unique(df_kat$lookup_group_ID), "'", collapse = ", ")
  
  df_lp <- tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM '03_LEITPROFILE' WHERE group_ID IN (%s)",
      paste(lp_select, collapse = ", "), ids)),
    error = function(e) { message("Leitprofile: ", e$message); data.frame() })
  
  if (nrow(df_lp) == 0) return(df_kat)
  
  num_cols   <- setdiff(names(df_lp)[sapply(df_lp, is.numeric)],
                        c("group_ID", "TIEFE_OG", "TIEFE_UG"))
  paste_cols <- names(df_lp)[!sapply(df_lp, is.numeric) & names(df_lp) != "group_ID"]
  
  lp_agg <- df_lp %>%
    dplyr::group_by(group_ID) %>%
    dplyr::summarise(
      n_Schichten  = dplyr::n(),
      Tiefe_OG_min = min(TIEFE_OG, na.rm = TRUE),
      Tiefe_UG_max = max(TIEFE_UG, na.rm = TRUE),
      dplyr::across(dplyr::all_of(num_cols),
                    ~ { v <- .x[!is.na(.x) & .x != -9999]
                    if (length(v) == 0) NA_real_ else round(mean(v), 2) }),
      dplyr::across(dplyr::all_of(paste_cols),
                    ~ paste(unique(na.omit(as.character(.x[.x != "NA"]))),
                            collapse = ", ")),
      .groups = "drop")
  
  result  <- dplyr::left_join(df_kat, lp_agg,
                              by = c("lookup_group_ID" = "group_ID"))
  bzt_tab <- if (region == "NR") Boden_BZT.dir.NR else Boden_BZT.dir.BWIBZE
  if (!is.null(bzt_tab))
    result <- dplyr::left_join(result, bzt_tab, by = "MASTER_ID")
  result
}

# ── get_bzt: BZT-Klimadaten fuer eine MASTER_ID + Klimalauf ──

bzt_csv_cache <- new.env(parent = emptyenv())

get_bzt <- function(master_id, region, szenario, modell, zeitraum) {
  if (!exists("BZT_CSV_DIR") || !dir.exists(BZT_CSV_DIR)) return(NULL)
  
  if (region == "BWI") {
    folder_name <- "BWI_BZE"; region_file <- "BWI-BZE"
  } else {
    nr_num      <- as.integer(gsub("[^0-9]", "", region))
    folder_name <- paste0("NR-", sprintf("%02d", nr_num))
    region_file <- folder_name
  }
  
  fname <- paste0("BZT_Zuordnung_", region_file, "_",
                  szenario, "_", modell, "_", zeitraum, ".csv")
  fpath <- file.path(BZT_CSV_DIR, folder_name, fname)
  if (!file.exists(fpath)) { message("BZT-CSV nicht gefunden: ", fname); return(NULL) }
  
  if (!exists(fname, envir = bzt_csv_cache)) {
    dt <- tryCatch(
      data.table::fread(fpath, encoding = "UTF-8", data.table = FALSE),
      error = function(e) { message("BZT-CSV Fehler: ", e$message); NULL })
    assign(fname, dt, envir = bzt_csv_cache)
    message("BZT-CSV geladen: ", fname, " (", nrow(dt), " Zeilen)")
  }
  
  dt  <- get(fname, envir = bzt_csv_cache)
  if (is.null(dt)) return(NULL)
  row <- dt[as.character(dt$MASTER_ID) == as.character(master_id), , drop = FALSE]
  if (nrow(row) == 0) NULL else row
}

# ── boden_zeile: HTML-Hilfsfunktion fuer Panel ───────────────

boden_zeile <- function(label, wert) {
  wert_str <- if (is.null(wert) || (length(wert) == 1 && is.na(wert))) "\u2013"
  else as.character(wert)
  shiny::tags$div(style = "font-size:12px; margin-bottom:2px; line-height:1.4;",
                  shiny::tags$span(style = "color:#666; min-width:90px; display:inline-block;",
                                   paste0(label, ":")),
                  shiny::tags$span(style = "font-weight:500;", wert_str))
}

# ── make_popup: Leaflet-Popup HTML ───────────────────────────

make_popup <- function(d, boden, bzt = NULL) {
  bzt_html <- if (!is.null(bzt) && nrow(bzt) > 0) {
    z <- bzt[1, ]
    is_bwi <- "KS_gl" %in% names(z)
    ks_str <- if (is_bwi && !is.na(z$pred_exp2))
      paste0(z$pred_exp2, if (!is.na(z$KS_gl) && nchar(z$KS_gl) > 0)
        paste0(" (KS: ", z$KS_gl, ")") else "")
    else if (!is.na(z$pred_exp2)) as.character(z$pred_exp2) else "\u2013"
    paste0("<hr style='margin:4px 0'>",
           "<b style='color:#1565C0'>&#9654; Standort (BZT)</b><br>",
           "<b>Klimastufe:</b> ", ks_str, "<br>",
           "<b>Info:</b> ", z$info_BAE %||% "\u2013", "<br>",
           "<b>Hangseite:</b> ", z$Hangseite %||% "\u2013",
           " | <b>Position:</b> ", z$Position %||% "\u2013", "<br>",
           if (is_bwi && !is.na(z$STGR_pv1))
             paste0("<b>STGR:</b> ", z$STGR_pv1, "<br>") else "")
  } else ""
  
  boden_html <- if (!is.null(boden) && nrow(boden) > 0) {
    b <- boden[1, ]
    paste0("<hr style='margin:4px 0'>",
           "<b style='color:#2E7D32'>&#9650; Bodendaten</b><br>",
           "<b>Bodentyp:</b> ",         b$BODTYP,              "<br>",
           if (!is.null(b$STAOTYP) && !is.na(b$STAOTYP))
             paste0("<b>Staotyp:</b> ", b$STAOTYP, "<br>") else "",
           "<b>N\u00e4hrkraft:</b> ",   b$NAEHR,               "<br>",
           "<b>Wasser:</b> ",           b$WASSER,              "<br>",
           "<b>Staoform kurz:</b> ",    b$SOEH_KRZ,            "<br>",
           "<b>NFK:</b> ",              b$NFK_DEHNER,
           " | <b>NFK (m.Aufl.):</b> ", b$NFK_DEHNER_AUFLAGE,  " mm<br>",
           "<b>Substrat:</b> ",         b$STRATI,              "<br>",
           "<b>Bodenart:</b> ",         b$BODART,              "<br>",
           "<b>Sand/Schluff/Ton:</b> ", b$SAND, "% / ", b$SCHLUFF, "% / ", b$TON, "%")
  } else "<hr><i style='color:grey'>Bodendaten: \u2013</i>"
  
  paste0("<b>MASTER_ID:</b> ", d$MASTER_ID, "<br>",
         "<b>Baumart:</b> ",   d$Baumart,   "<br>",
         "<b>TV:</b> ",        d$TV,        "<br>",
         "<b>Kategorie:</b> ", d$Kat,       "<br>",
         "<b>Klimalauf:</b> ", d$Szenario, " / ", d$Modell, " / ", d$Zeitraum,
         bzt_html, boden_html)
}

# ── render_standortblatt: PDF-Export via R Markdown ──────────
# Rendert standortblatt.Rmd (liegt neben app.R) zu einem temporaeren PDF und
# gibt dessen Pfad zurueck. Die eigentliche Aufbereitung (Deckblatt, TOC,
# Einleitung, Karte, Standortanalyse, Bodendaten) steckt in der Rmd.
#
# Die Rmd erhaelt Punkt, Boden, WM und alle Klimalaeufe via params; die
# Plot-Funktionen (heatmap_standort_ggplot etc.), kat_palette und DE_GRENZE
# werden ueber envir = new.env(parent = globalenv()) sichtbar gemacht.
#
# Benoetigt: rmarkdown + pandoc + LaTeX (tinytex). Fehlt eine Komponente,
# faellt die Funktion auf das alte gridExtra-PDF (generate_standortblatt) zurueck.

render_standortblatt <- function(d, boden, wm_df, alle_laeufe,
                                 region    = "BWI",
                                 de_grenze = NULL,
                                 rmd_path  = NULL,
                                 out_file  = NULL) {
  
  if (is.null(out_file)) out_file <- tempfile(fileext = ".pdf")
  de_grenze <- de_grenze %||% get0("DE_GRENZE", envir = .GlobalEnv)
  
  # Voraussetzungen pruefen – bei Fehlen sauberer Fallback auf gridExtra-PDF
  hat_rmd <- requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()
  if (!isTRUE(hat_rmd)) {
    message("rmarkdown/pandoc nicht verfuegbar - Fallback auf gridExtra-PDF.")
    return(generate_standortblatt(d, boden, wm_df, alle_laeufe,
                                  de_grenze, out_file))
  }
  
  # Rmd-Pfad: standardmaessig neben app.R (APP_DIR_ABS)
  if (is.null(rmd_path)) {
    base     <- get0("APP_DIR_ABS", envir = .GlobalEnv) %||% getwd()
    rmd_path <- file.path(base, "standortblatt.Rmd")
  }
  if (!file.exists(rmd_path)) {
    message("standortblatt.Rmd nicht gefunden (", rmd_path,
            ") - Fallback auf gridExtra-PDF.")
    return(generate_standortblatt(d, boden, wm_df, alle_laeufe,
                                  de_grenze, out_file))
  }
  
  # In ein beschreibbares Temp-Verzeichnis kopieren (Projektordner ggf. read-only)
  tmp_dir <- file.path(tempdir(), paste0("sb_", as.integer(Sys.time())))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  tmp_rmd <- file.path(tmp_dir, "standortblatt.Rmd")
  file.copy(rmd_path, tmp_rmd, overwrite = TRUE)
  
  # render() in tryCatch: scheitert es (z.B. kein LaTeX / kein tinytex),
  # faellt die Funktion sauber auf das gridExtra-PDF zurueck.
  render_ok <- tryCatch({
    rmarkdown::render(
      input         = tmp_rmd,
      output_file   = out_file,
      output_format = "pdf_document",
      params        = list(
        master_id   = as.character(d$MASTER_ID),
        baumart     = as.character(d$Baumart),
        region      = region,
        punkt       = d,
        boden       = boden,
        wm_df       = wm_df,
        alle_laeufe = alle_laeufe,
        de_grenze   = de_grenze
      ),
      envir             = new.env(parent = globalenv()),
      intermediates_dir = tmp_dir,
      knit_root_dir     = tmp_dir,
      quiet             = TRUE
    )
    TRUE
  }, error = function(e) {
    message("rmarkdown::render fehlgeschlagen: ", e$message,
            "\n-> Fallback auf gridExtra-PDF.")
    FALSE
  })
  
  if (!render_ok)
    return(generate_standortblatt(d, boden, wm_df, alle_laeufe,
                                  de_grenze, out_file))
  out_file
}

# ── generate_standortblatt: PDF-Export (Fallback, gridExtra) ─

generate_standortblatt <- function(d, boden, wm_df, alle_laeufe_df,
                                   de_grenze, out_file) {
  requireNamespace("gridExtra", quietly = TRUE)
  requireNamespace("grid",      quietly = TRUE)
  
  master_id <- as.character(d$MASTER_ID)
  baumart   <- as.character(d$Baumart)
  
  if (all(c("X_CENTROID", "Y_CENTROID") %in% names(d))) {
    lon_c <- as.numeric(d$X_CENTROID[1])
    lat_c <- as.numeric(d$Y_CENTROID[1])
  } else {
    koords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(d)))
    lon_c  <- koords[1]; lat_c <- koords[2]
  }
  
  delta <- 0.9
  p_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = de_grenze, fill = "#f0f0ee",
                     color = "#aaaaaa", linewidth = 0.3) +
    ggplot2::geom_point(data = data.frame(x = lon_c, y = lat_c),
                        ggplot2::aes(x = x, y = y),
                        color = "#A50026", size = 4, shape = 17) +
    ggplot2::coord_sf(xlim = c(lon_c-delta, lon_c+delta),
                      ylim = c(lat_c-delta*0.7, lat_c+delta*0.7), expand = FALSE) +
    ggplot2::labs(title = paste0("Lage: ", master_id), x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(plot.title   = ggplot2::element_text(face = "bold", size = 9),
                   panel.border = ggplot2::element_rect(fill = NA, color = "#cccccc"),
                   plot.background = ggplot2::element_rect(fill = "white", color = NA))
  
  bae_tbl <- if (!is.null(alle_laeufe_df) && nrow(alle_laeufe_df) > 0)
    alle_laeufe_df %>%
    dplyr::arrange(Szenario, Modell, Zeitraum) %>%
    dplyr::select(Szenario, Modell, Zeitraum, dplyr::starts_with("BAE_")) %>%
    as.data.frame()
  else data.frame(Info = "Keine BAE-Daten verf\u00fcgbar")
  
  ttheme_gruen  <- gridExtra::ttheme_minimal(
    base_size = 7,
    core      = list(bg_params = list(fill = c("white", "#f5f9f5"))),
    colhead   = list(bg_params = list(fill = "#2E7D32"),
                     fg_params = list(col  = "white", fontface = "bold")))
  ttheme_braun  <- gridExtra::ttheme_minimal(
    base_size = 7,
    core      = list(bg_params = list(fill = c("white", "#fff8f5"))),
    colhead   = list(bg_params = list(fill = "#795548"),
                     fg_params = list(col  = "white", fontface = "bold")))
  
  tbl_bae <- gridExtra::tableGrob(bae_tbl, rows = NULL, theme = ttheme_gruen)
  
  boden_rows <- if (!is.null(boden) && nrow(boden) > 0) {
    b <- boden[1, ]
    data.frame(
      Merkmal = c("Bodentyp","N\u00e4hrkraft","Wasser","Staoform kurz","Staoform lang",
                  "NFK (mm)","NFK m.Aufl.","Tmax NFK","Substrat","Bodenart",
                  "Tiefe (cm)","Schichten","TRD",
                  "Sand (%)","Schluff (%)","Ton (%)",
                  "Feinsand (%)","Mittelsand (%)","Grobsand (%)","Skelett (%)",
                  "SOC","Carbonat","Basen","C/N","Grundwasser","Stauwasser"),
      Wert = c(
        as.character(b$BODTYP    %||% "\u2013"), as.character(b$NAEHR   %||% "\u2013"),
        as.character(b$WASSER    %||% "\u2013"), as.character(b$SOEH_KRZ %||% "\u2013"),
        as.character(b$SOEH_LNG  %||% "\u2013"), as.character(b$NFK_DEHNER %||% "\u2013"),
        as.character(b$NFK_DEHNER_AUFLAGE %||% "\u2013"), as.character(b$Tmax_NFK %||% "\u2013"),
        as.character(b$STRATI    %||% "\u2013"), as.character(b$BODART %||% "\u2013"),
        paste0(b$Tiefe_OG_min %||% "?", " \u2013 ", b$Tiefe_UG_max %||% "?"),
        as.character(b$n_Schichten %||% "\u2013"), as.character(b$TRD %||% "\u2013"),
        as.character(b$SAND    %||% "\u2013"), as.character(b$SCHLUFF %||% "\u2013"),
        as.character(b$TON     %||% "\u2013"), as.character(b$FEINSAND %||% "\u2013"),
        as.character(b$MITTELSAND %||% "\u2013"), as.character(b$GROBSAND %||% "\u2013"),
        as.character(b$SKELETT %||% "\u2013"), as.character(b$SOC %||% "\u2013"),
        as.character(b$CARBONAT %||% "\u2013"), as.character(b$BASEN %||% "\u2013"),
        as.character(b$CN %||% "\u2013"), as.character(b$GRUNDH20 %||% "\u2013"),
        as.character(b$STAUH20 %||% "\u2013")),
      stringsAsFactors = FALSE)
  } else data.frame(Merkmal = "Keine Bodendaten", Wert = "\u2013")
  
  tbl_boden <- gridExtra::tableGrob(boden_rows, rows = NULL, theme = ttheme_gruen)
  
  wm_rows <- if (!is.null(wm_df) && nrow(wm_df) > 0) {
    rows <- dplyr::bind_rows(lapply(names(wm_felder), function(kuerzel) {
      info <- wm_felder[[kuerzel]]
      sub  <- wm_df[wm_df$WM_KUERZEL == kuerzel, , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      dplyr::bind_rows(lapply(names(info$cols), function(col) {
        if (!col %in% names(sub) || is.na(sub[[col]][1])) return(NULL)
        val <- sub[[col]][1]
        data.frame(
          Modell   = info$label,
          Kennzahl = info$cols[[col]],
          Wert     = if (col == "HG100") paste0(round(as.numeric(val)/10,1), " m")
          else as.character(round(as.numeric(val), 2)),
          stringsAsFactors = FALSE)
      }))
    }))
    if (is.null(rows) || nrow(rows) == 0)
      data.frame(Modell = "Keine WM-Daten", Kennzahl = "", Wert = "")
    else rows
  } else data.frame(Modell = "Keine WM-Daten", Kennzahl = "", Wert = "")
  
  tbl_wm <- gridExtra::tableGrob(wm_rows, rows = NULL, theme = ttheme_braun)
  
  titel_grob <- grid::textGrob(
    paste0("STANDORTBLATT MRS   |   MASTER_ID: ", master_id,
           "   |   Baumart: ", baumart,
           "   |   Erstellt: ", format(Sys.Date(), "%d.%m.%Y")),
    gp = grid::gpar(fontsize = 9, fontface = "bold", col = "#2E7D32"))
  linie  <- grid::linesGrob(
    x = grid::unit(c(0,1), "npc"), y = grid::unit(c(1,1), "npc"),
    gp = grid::gpar(col = "#2E7D32", lwd = 1.5))
  header <- gridExtra::arrangeGrob(titel_grob, linie, ncol = 1,
                                   heights = grid::unit(c(0.7, 0.05), "cm"))
  
  layout <- gridExtra::arrangeGrob(
    header,
    gridExtra::arrangeGrob(p_map, tbl_boden, ncol = 2, widths = c(1.1, 0.9)),
    gridExtra::arrangeGrob(tbl_bae, tbl_wm,  ncol = 2, widths = c(1.1, 0.9)),
    nrow = 3, heights = grid::unit(c(0.6, 12, 10), "cm"))
  
  pdf(out_file, width = 29.7/2.54, height = 21/2.54)
  on.exit({ if (dev.cur() > 1) dev.off() }, add = TRUE)
  tryCatch({
    grid::grid.newpage()
    grid::grid.draw(layout)
  }, error = function(e) {
    message("Standortblatt-Fehler: ", e$message)
    grid::grid.newpage()
    grid::grid.text(paste0("Fehler:\n", e$message),
                    gp = grid::gpar(col = "red", fontsize = 10))
  })
  invisible(out_file)
}