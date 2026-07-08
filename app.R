#########################################################
#                                                       #
# MRS Baumartenempfehlung                               #
# app.R  \u2013  nur UI + Server-Verkabelung                  #
# Alle Funktionen in R/mod_*.R                          #
#                                                       #
#########################################################

# ---- 0. Grunddaten & Setup ----

## ---- 0.1 Packages ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(DBI)
library(RSQLite)
library(htmlwidgets)
library(stringr)
library(readxl)
library(ggplot2)
library(DT)
library(tidyr)
library(gridExtra)
library(scales)
if (requireNamespace("base64enc", quietly = TRUE)) {
  library(base64enc)
} else {
  message("Paket 'base64enc' nicht installiert.")
  base64enc <- list(dataURI = function(...) stop("base64enc fehlt"))
}

## ---- 0.2 App-Verzeichnis verankern ----
# APP_DIR_ABS wird nur beim ERSTEN Durchlauf gesetzt (if-Guard) und ist
# danach unveraenderlicher Anker fuer ALLE Pfadkonstruktionen.
# Bei shiny::runApp("06_shiny") ist getwd() das App-Verzeichnis.
# Falls app.R zweimal gesourct wird (Shiny-Doppel-Source o.ae.), bleibt der
# Wert des ersten, korrekten Durchlaufs erhalten -> Pfade bleiben absolut korrekt.
if (!exists("APP_DIR_ABS")) {
  APP_DIR_ABS <- normalizePath(getwd(), mustWork = TRUE)
}
message("App-Verzeichnis: ", APP_DIR_ABS)

## ---- 0.3 Module laden (config.R, cache.R, R/mod_*.R) ----
# config.R und cache.R liegen NEBEN app.R (nicht in R/), werden also NICHT
# automatisch gesourct. Quelle ueber den absoluten App-Anker -> CWD-unabhaengig.
source(file.path(APP_DIR_ABS, "config.R"))
source(file.path(APP_DIR_ABS, "cache.R"))

## ---- 0.4 Pfade ----
# Projektstamm: Anzahl Ebenen ueber dem App-Verzeichnis ist je nach
# Installation unterschiedlich (z.B. .../BAE_Auswertung_app/BWI_Geo_BAE
# -> 1 Ebene, oder .../05_documents/BWI_Geo_BAE -> 2 Ebenen). Daher von
# APP_DIR_ABS aus aufwaerts suchen, bis ein Verzeichnis mit "01_data/"
# gefunden wird (max. 3 Ebenen, Fallback: 1 Ebene hoch).
# LOCAL_DIR aus config.R ("../../") wird hier durch den ABSOLUTEN Pfad ersetzt.
# Damit sind alle nachgelagerten file.path()-Aufrufe CWD-unabhaengig und das
# in der Vergangenheit haeufigste Bug-Muster (relative Pfade im reaktiven
# Kontext) ist ausgeschlossen.
LOCAL_DIR <- local({
  dir <- APP_DIR_ABS
  for (i in 1:3) {
    dir <- normalizePath(file.path(dir, ".."), mustWork = FALSE)
    if (dir.exists(file.path(dir, "01_data"))) return(dir)
  }
  normalizePath(file.path(APP_DIR_ABS, ".."), mustWork = FALSE)
})
message("Projektstamm: ", LOCAL_DIR,
        " [", if (dir.exists(LOCAL_DIR)) "OK" else "NICHT GEFUNDEN", "]")

# Lokaler Testdaten-Fallback fuer NR-Rohdaten + WM (create_testdata.R):
# greift nur, wenn weder S:\ noch E:\ verfuegbar sind (BAE_WM_DIR/WM_DIR NULL).
if (is.null(BAE_WM_DIR)) {
  test_bae <- file.path(LOCAL_DIR, "01_data/_extern_testdaten/Ergebnisse_BAE")
  if (dir.exists(test_bae)) {
    BAE_WM_DIR <- test_bae
    message("Testdaten-Fallback: BAE_WM_DIR = ", test_bae)
  }
}
if (is.null(WM_DIR)) {
  for (kand in c(file.path(LOCAL_DIR, "01_data/_extern_testdaten/Ergebnisse_WM"),
                 file.path(LOCAL_DIR, "04_results/WM"))) {
    if (dir.exists(kand)) {
      WM_DIR <- kand
      message("Testdaten-Fallback: WM_DIR = ", kand)
      break
    }
  }
}

## ---- 0.5 Daten laden (Boden, BZT, Geo/NR) ----

boden_dir  <- normalizePath(file.path(LOCAL_DIR, "01_data/Grundlagen/Bodendatenbank"),         mustWork = FALSE)
geo_dir    <- normalizePath(file.path(LOCAL_DIR, "01_data/Grundlagen/Geodaten"),         mustWork = FALSE)
result_dir <- normalizePath(file.path(LOCAL_DIR, "04_results"),         mustWork = FALSE)
bzt_dir    <- normalizePath(file.path(LOCAL_DIR, "01_data/BZT_Mergings") ,         mustWork = FALSE)

Boden.dir <- list.files(boden_dir, recursive = TRUE, full.names = TRUE)

BWI_Boden <- grep("MRS_BWI\\.sqlite", Boden.dir, value = TRUE)
BZE_Boden <- grep("MRS_BZE\\.sqlite", Boden.dir, value = TRUE)
NR_Boden  <- grep("MRS_NR\\.sqlite",  Boden.dir, value = TRUE)

BWI_Boden <- c(BWI_Boden, BZE_Boden)

BZT_CSV_DIR <- normalizePath(file.path(LOCAL_DIR, "01_data/Ergebnisse_BAE/BZT_BAE_Zuordnung"),         mustWork = FALSE)
KS_IMG_DIR  <- normalizePath(file.path(LOCAL_DIR, "01_data/Ergebnisse_BAE/BZT_Klimastufeneinteilung"), mustWork = FALSE)

message("boden_dir  : ", boden_dir,
        " [", if (dir.exists(boden_dir)) "OK" else "NICHT GEFUNDEN", "]")
message("SQLite BWI : ", if (length(grep("MRS_BWI", BWI_Boden)) > 0)
  grep("MRS_BWI\\.sqlite", BWI_Boden, value = TRUE)[1] else "(nicht gefunden)")
message("SQLite BZE : ", if (length(BZE_Boden) > 0) BZE_Boden[1] else "(nicht gefunden)")

### ---- 0.5.1 Boden-Schema-Cache (einmalig beim Start) ----
boden_schema_cache <- local({
  cache_db <- function(db_path) {
    if (length(db_path) == 0 || !file.exists(db_path[1])) return(NULL)
    con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db_path[1]),
                    error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    list(
      kat = tryCatch(
        DBI::dbGetQuery(con, "PRAGMA table_info('02_KARTIEREINHEITEN')")$name,
        error = function(e) character(0)),
      lp  = tryCatch(
        DBI::dbGetQuery(con, "PRAGMA table_info('03_LEITPROFILE')")$name,
        error = function(e) character(0))
    )
  }
  bwi_db <- grep("MRS_BWI", BWI_Boden, value = TRUE)
  list(BWI = cache_db(bwi_db),
       BZE = cache_db(BZE_Boden),
       NR  = cache_db(NR_Boden))
})
message("Boden-Schema gecacht: BWI=",
        length(boden_schema_cache$BWI$kat), " Kat-Spalten | ",
        length(boden_schema_cache$BWI$lp),  " LP-Spalten")

### ---- 0.5.2 BZT-Daten laden ----
bzt_data             <- init_boden_bzt(bzt_dir = bzt_dir)
Boden_BZT.dir.BWIBZE <- bzt_data$BWIBZE
Boden_BZT.dir.NR     <- bzt_data$NR

# project_root als local_dir uebergeben: absoluter Projektstamm-Pfad,
# wird in init_karte_daten() fuer den NR-Geodaten-Pfad benoetigt
# init_wm() ZUERST: haengt nur an WM_DIR (oben gesetzt) und ist unabhaengig von
# init_karte_daten(). So bleibt der WM-Pfad fuer den Karten-Klick auch dann
# initialisiert, falls init_karte_daten() einmal mit einem Fehler abbricht.
init_wm(wm_dir = WM_DIR)
init_karte_daten(geo_dir    = geo_dir,
                 result_dir = result_dir,
                 local_dir  = LOCAL_DIR)
init_app_cache()

## ---- 0.5.3 UI-Choices im App-Scope absichern ----
# Die Standortanalyse-Sidebar referenziert modell_choices/zeitraum_choices/
# rcp45_var_choices DIREKT beim UI-Aufbau (statische choices, nicht reaktiv).
# init_karte_daten() setzt sie zwar via <<-, aber falls eine aeltere
# R/mod_karte.R geladen ist (app.R aktualisiert, R/-Helfer nicht), existieren
# sie im UI-Scope nicht -> "Objekt 'modell_choices' nicht gefunden".
# Deshalb hier direkt aus klima_meta ableiten: sichtbar, debugbar und
# unabhaengig davon, was init_karte_daten() in welche Umgebung geschrieben hat.
if (!exists("modell_choices")   || length(modell_choices)   == 0)
  modell_choices   <- sort(unique(klima_meta$Modell))
if (!exists("zeitraum_choices") || length(zeitraum_choices) == 0)
  zeitraum_choices <- sort(unique(klima_meta$Zeitraum))
if (!exists("rcp45_var_choices") || length(rcp45_var_choices) == 0) {
  rcp45_var_choices <- if (exists("bae_variante")) {
    rc <- startsWith(as.character(klima_meta$Szenario), "RCP45")
    sort(unique(bae_variante(klima_meta$Szenario[rc], klima_meta$Modell[rc])))
  } else character(0)
}

## ---- 0.6 Globals fuer reaktive Funktionen spiegeln ----
# Mehrere Hilfsfunktionen in R/mod_*.R (z.B. get_boden(), get_standort_data())
# lesen Datenobjekte explizit aus .GlobalEnv. shiny::runApp() sourct app.R
# aber in eine Kind-Umgebung von .GlobalEnv -> dort definierte Variablen sind
# in .GlobalEnv sonst nicht sichtbar. Deshalb hier spiegeln.
for (.nm in c("APP_DIR_ABS", "LOCAL_DIR", "geo_dir", "result_dir",
              "BWI_Boden", "BZE_Boden", "NR_Boden",
              "BWI_GEO", "DE_GRENZE", "NR_GEO_ALL", "NR_UMKREIS_ALL")) {
  if (exists(.nm, inherits = FALSE)) {
    assign(.nm, get(.nm, inherits = FALSE), envir = .GlobalEnv)
  }
}
rm(.nm)


## ---- 0.7 CSS ----

app_css <- "
body { font-size: 13px; }
.sidebar-section {
  background: #f7f7f7; border-radius: 6px;
  padding: 10px 12px 8px 12px;
  margin-bottom: 8px; border: 1px solid #e8e8e8;
}
.section-title {
  font-size: 10px; font-weight: 700;
  text-transform: uppercase; letter-spacing: .07em;
  color: #2E7D32; margin: 0 0 8px 0;
  padding-bottom: 5px; border-bottom: 1px solid #ddd;
}
.treffer-box {
  background: #eaf4ea; border: 1px solid #c8e6c9;
  border-radius: 5px; padding: 6px 10px;
  font-size: 12px; color: #2E7D32;
  margin-bottom: 4px; line-height: 1.5;
}
.nav-tabs { border-bottom: 2px solid #2E7D32; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover {
  color: #fff !important; background: #2E7D32 !important;
  border-color: #2E7D32 !important;
}
.selectize-input  { font-size: 13px !important; min-height: 32px !important; }
.selectize-dropdown { font-size: 13px !important; }
.control-label { font-size: 12px !important; margin-bottom: 2px !important; color: #444; }
.radio label, .checkbox label { font-size: 12px; }
.form-group { margin-bottom: 6px !important; }
.btn { font-size: 12px !important; }
.irs--shiny .irs-single { font-size: 11px; }
.info-block { background:#f9f9f9; border-left:3px solid #2E7D32;
  padding:10px 14px; margin-bottom:14px; border-radius:0 6px 6px 0; }
.info-block h4 { color:#2E7D32; margin-top:0; }
"

# ---- 1. UI ----

## ---- 1.1 Sidebar (geteilt zwischen Tabs) ----

karte_sidebar <- sidebarPanel(width = 3,
                              
                              ### ---- 1.1.1 Klimalauf ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Datenquelle & Klimalauf"),
                                       
                                       # Datenquelle-Umschalter
                                       tags$label(class = "control-label", "Datenquelle:"),
                                       radioButtons("datenquelle", label = NULL,
                                                    choices  = c("BWI-BZE" = "BWI", "NR" = "NR"),
                                                    selected = "BWI", inline = TRUE),
                                       
                                       # NR-Selector (nur sichtbar bei NR)
                                       conditionalPanel(
                                         condition = "input.datenquelle == 'NR'",
                                         selectInput("nr_sel", "Nachbarschaftsregion:",
                                                     choices  = nr_choices,
                                                     selected = "NR01",
                                                     width    = "100%")
                                       ),
                                       
                                       hr(style = "margin:6px 0;"),
                                       
                                       selectInput("szenario", "Szenario:",
                                                   choices = szenario_choices, selected = szenario_choices[1],
                                                   width = "100%"),
                                       selectInput("modell",   "Klimamodell:", choices = character(0), width = "100%"),
                                       selectInput("zeitraum", "Zeitraum:",    choices = character(0), width = "100%")
                              ),
                              
                              ### ---- 1.1.2 Baumart & TV ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Baumart & TV"),
                                       selectizeInput("baumart_sel", "Baumart:",
                                                      choices  = baumart_choices,
                                                      selected = if ("Bu" %in% baumart_choices) "Bu" else baumart_choices[1],
                                                      multiple = TRUE,
                                                      options  = list(placeholder = "Baumart w\u00e4hlen...",
                                                                      plugins = list("remove_button"), maxOptions = 50),
                                                      width = "100%"),
                                       selectizeInput("tv_sel", "TV (Teilvorhaben):",
                                                      choices  = tv_choices,
                                                      selected = if ("2" %in% tv_choices) "2" else as.character(tv_choices[1]),
                                                      multiple = FALSE,
                                                      options  = list(placeholder = "TV w\u00e4hlen..."),
                                                      width = "100%")
                              ),
                              
                              ### ---- 1.1.3 Darstellung ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Darstellung"),
                                       
                                       # Farb-Modus
                                       tags$label(class = "control-label", "Einf\u00e4rbung:"),
                                       radioButtons("farb_modus", label = NULL,
                                                    choices  = c("Empfehlung" = "empfehlung",
                                                                 "Baumart"    = "baumart"),
                                                    selected = "empfehlung", inline = TRUE),
                                       
                                       # Bewertungsstufe (wird dynamisch aktualisiert)
                                       tags$label(class = "control-label", "Bewertungsstufe:"),
                                       radioButtons("stufe", label = NULL,
                                                    choices  = c("3-stufig" = "BAE_3ST",
                                                                 "4-stufig" = "BAE_4ST",
                                                                 "5-stufig" = "BAE_5ST"),
                                                    selected = "BAE_5ST", inline = TRUE),
                                       
                                       sliderInput("punktgroesse", "Punktgr\u00f6\u00dfe:",
                                                   min = 1, max = 10, value = 4, step = 0.5, width = "100%")
                              ),
                              
                              ### ---- 1.1.4 Trefferanzahl ----
                              uiOutput("treffer_box"),
                              
                              ### ---- 1.1.5 Karte laden ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Karte laden"),
                                       actionButton("run_karte", "Karte erstellen",
                                                    icon  = icon("play"),
                                                    style = paste("width:100%; background:#2E7D32;",
                                                                  "color:white; font-weight:bold;",
                                                                  "margin-bottom:4px;")),
                                       tags$small(style = "color:#aaa; font-size:10px; display:block;",
                                                  "Daten werden erst nach Klick geladen")
                              ),
                              
                              ### ---- 1.1.6 Export ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Export"),
                                       downloadButton("save_html", "HTML speichern",
                                                      style = "width:100%; margin-bottom:5px;"),
                                       downloadButton("save_png", "PNG speichern",
                                                      style = "width:100%;"),
                                       tags$small(style = "color:#aaa; font-size:10px; margin-top:3px; display:block;",
                                                  "PNG-Download der aktuellen Karte"),
                                       tags$hr(style = "margin:8px 0 6px;"),
                                       actionButton("open_schleife", "Als Schleife abspeichern",
                                                    icon  = icon("layer-group"),
                                                    style = paste("width:100%; background:#00695C;",
                                                                  "color:white; font-weight:bold;")),
                                       tags$small(style = "color:#aaa; font-size:10px; margin-top:3px; display:block;",
                                                  "Alle Baumarten × Stufen ins Ergebnisverzeichnis rendern")
                              ),
                              
                              ### ---- 1.1.7 Legende (reaktiv je Farb-Modus) ----
                              tags$div(class = "sidebar-section",
                                       tags$p(class = "section-title", "\u25B6 Legende"),
                                       uiOutput("legende_ui")
                              )
)

## ---- 1.2 UI-Zusammenbau (navbarPage) ----

ui <- tagList(
  # tags$head() muss ausserhalb von navbarPage() stehen –
  # sonst: "Navigation containers expect a collection of tabPanel()s"
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$style(HTML(app_css))
  ),
  navbarPage(
    title       = "MRS Baumartenempfehlung",
    id          = "nav",
    collapsible = TRUE,
    theme       = NULL,
    
    
    ### ---- 1.2.1 Tab 1: Karte ----
    tabPanel(
      title = tagList(icon("map"), " Karte"),
      sidebarLayout(
        karte_sidebar,
        mainPanel(width = 9,
                  # Status/Fehler aus filtered() – ohne diesen Output verschwinden
                  # validate()-Meldungen (z.B. "Keine NR-CSV gefunden") unsichtbar
                  # in tryCatch-Konsumenten und die Karte bleibt kommentarlos leer.
                  uiOutput("karte_status"),
                  div(style = "position:relative;",
                      leafletOutput("map", height = "68vh"),
                      # Reset-Button oben rechts in der Karte
                      absolutePanel(
                        top = 10, right = 10, width = "auto",
                        actionButton("map_reset", "", icon = icon("crosshairs"),
                                     title = "Kartenausschnitt zur\u00fccksetzen",
                                     style = "background:white; border:1px solid #ccc;
                                  padding:5px 8px; border-radius:4px;
                                  box-shadow:0 1px 3px rgba(0,0,0,.2);")
                      )
                  ),
                  uiOutput("boden_panel")
        )
      )
    ),
    
    ### ---- 1.2.2 Tab 2: Analyse ----
    tabPanel(
      title = tagList(icon("chart-bar"), " Analyse"),
      sidebarLayout(
        karte_sidebar,
        mainPanel(width = 9,
                  uiOutput("analyse_header"),
                  hr(style = "margin:8px 0 14px 0; border-color:#e0e0e0;"),
                  fluidRow(
                    column(7,
                           div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                         letter-spacing:.06em; color:#2E7D32; margin-bottom:6px;",
                               "\u25B6 Anteil je Empfehlungskategorie"),
                           plotOutput("analyse_balken", height = "320px")
                    ),
                    column(5,
                           div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                         letter-spacing:.06em; color:#2E7D32; margin-bottom:6px;",
                               "\u25B6 Export & Kennzahlen"),
                           downloadButton("download_kreuztab", "Kreuztabelle CSV",
                                          style = "width:100%; margin-bottom:6px;"),
                           downloadButton("download_stats", "Statistik CSV",
                                          style = "width:100%;"),
                           br(), br(),
                           uiOutput("kennzahlen_boxes")
                    )
                  ),
                  hr(style = "margin:10px 0; border-color:#e0e0e0;"),
                  div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                     letter-spacing:.06em; color:#2E7D32; margin-bottom:6px;",
                      "\u25B6 Kreuztabelle: Baumart \u00d7 Kategorie"),
                  DT::dataTableOutput("analyse_kreuztab"),
                  br(),
                  div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                     letter-spacing:.06em; color:#2E7D32; margin-bottom:6px;",
                      "\u25B6 Deskriptive Statistik (Kategorie numerisch)"),
                  DT::dataTableOutput("analyse_stats")
        )
      )
    ),
    
    ### ---- 1.2.3 Tab 3: Vergleich (Phase 5) ----
    tabPanel(
      title = tagList(icon("code-branch"), " Vergleich"),
      fluidRow(
        column(3,
               div(class = "sidebar-section",
                   tags$p(class = "section-title",
                          icon("circle", style="color:#1565C0; font-size:8px;"),
                          " Referenz (A)"),
                   selectInput("vgl_sz_a",  "Szenario:",
                               choices = szenario_choices, selected = szenario_choices[1],
                               width = "100%"),
                   selectInput("vgl_mod_a", "Klimamodell:", choices = character(0), width = "100%"),
                   selectInput("vgl_zr_a",  "Zeitraum:",   choices = character(0), width = "100%")
               ),
               div(class = "sidebar-section",
                   tags$p(class = "section-title",
                          icon("circle", style="color:#B71C1C; font-size:8px;"),
                          " Vergleich (B)"),
                   selectInput("vgl_sz_b",  "Szenario:",
                               choices = szenario_choices, selected = szenario_choices[1],
                               width = "100%"),
                   selectInput("vgl_mod_b", "Klimamodell:", choices = character(0), width = "100%"),
                   selectInput("vgl_zr_b",  "Zeitraum:",   choices = character(0), width = "100%")
               ),
               div(class = "sidebar-section",
                   tags$p(class = "section-title", "\u25B6 Filter"),
                   selectizeInput("vgl_baumart", "Baumart:",
                                  choices  = baumart_choices,
                                  selected = if ("Bu" %in% baumart_choices) "Bu"
                                  else baumart_choices[1],
                                  multiple = TRUE,
                                  options  = list(placeholder = "Baumart...",
                                                  plugins = list("remove_button")),
                                  width = "100%"),
                   # selectizeInput("sa_tv", "TVs:",
                   #                choices  = tv_choices,
                   #                selected = tv_choices,   # nur BWI-TVs
                   #                multiple = TRUE,
                   selectizeInput("sa_tv", "TVs:",
                                  choices  = tv_bezeichnung,   # alle 9 TVs unabhängig vom BWI-Subset
                                  selected = tv_bezeichnung,   # alle vorausgewählt
                                  multiple = TRUE,
                                  options  = list(placeholder = "TV...",
                                                  plugins = list("remove_button")),
                                  width = "100%"),
                   tags$label(class = "control-label", "Bewertungsstufe:"),
                   radioButtons("vgl_stufe", label = NULL,
                                choices  = c("3-stufig" = "BAE_3ST",
                                             "4-stufig" = "BAE_4ST",
                                             "5-stufig" = "BAE_5ST"),
                                selected = "BAE_4ST", inline = TRUE),
                   checkboxInput("vgl_nur_aenderung",
                                 "Nur ver\u00e4nderte Punkte anzeigen",
                                 value = TRUE),
                   actionButton("vgl_run", "Vergleich starten",
                                icon  = icon("play"),
                                style = "width:100%; background:#2E7D32; color:white;
                                font-weight:bold; margin-top:6px;")
               ),
               uiOutput("vgl_summary_box")
        ),
        column(9,
               div(style = "position:relative;",
                   leafletOutput("map_vgl", height = "55vh"),
                   absolutePanel(top = 10, right = 10, width = "auto",
                                 actionButton("map_vgl_reset", "", icon = icon("crosshairs"),
                                              title = "Zur\u00fccksetzen",
                                              style = "background:white; border:1px solid #ccc;
                                  padding:5px 8px; border-radius:4px;
                                  box-shadow:0 1px 3px rgba(0,0,0,.2);")
                   )
               ),
               hr(style = "margin:8px 0; border-color:#e0e0e0;"),
               div(style = "font-size:11px; font-weight:700; text-transform:uppercase;
                     letter-spacing:.06em; color:#2E7D32; margin-bottom:6px;",
                   "\u25B6 Kategorie\u00e4nderungen"),
               DT::dataTableOutput("vgl_tabelle"),
               br(),
               downloadButton("vgl_download", "Differenztabelle CSV",
                              style = "font-size:12px;")
        )
      )
    ),
    
    ### ---- 1.2.4 Tab 5: Standortanalyse ----
    tabPanel(
      title = tagList(icon("magnifying-glass-chart"), " Standortanalyse"),
      fluidRow(
        column(3,
               #### ---- 1.2.4.1 Aktiver Punkt ----
               uiOutput("sa_punkt_info"),
               hr(style = "margin:8px 0;"),
               #### ---- 1.2.4.2 MASTER_ID direkt waehlen ----
               div(class = "sidebar-section",
                   tags$p(class = "section-title", "\u25B6 Standort w\u00e4hlen"),
                   tags$small(style="color:#888;display:block;margin-bottom:6px;",
                              "Alternativ zu Kartenklick: direkt ausw\u00e4hlen"),
                   radioButtons("sa_region_filter", "Region:",
                                choices  = c("BWI", "BZE", "NR"),
                                selected = "BWI", inline = TRUE),
                   conditionalPanel(
                     condition = "input.sa_region_filter == 'NR'",
                     selectInput("sa_nr_filter", "NR:",
                                 choices  = paste0("NR", sprintf("%02d", 1:11)),
                                 selected = "NR01", width = "100%")
                   ),
                   conditionalPanel(
                     condition = "input.sa_region_filter != 'NR'",
                     selectInput("sa_bl_filter", "Bundesland:",
                                 choices  = c("(alle)" = "", bundesland_map),
                                 selected = "", width = "100%")
                   ),
                   selectizeInput("sa_master_id_sel", "MASTER_ID:",
                                  choices  = NULL,
                                  selected = NULL,
                                  options  = list(
                                    placeholder    = "Suchen, tippen oder ausw\u00e4hlen...",
                                    maxOptions     = 200,
                                    # create = TRUE: getippte MASTER_ID wird auch
                                    # ohne Klick auf einen Listeneintrag uebernommen
                                    # (sonst verwirft selectize den Text beim Blur,
                                    #  das Feld leert sich und der Button tut nichts).
                                    create         = TRUE,
                                    createOnBlur   = TRUE,
                                    selectOnTab    = TRUE,
                                    openOnFocus    = TRUE
                                  ),
                                  width = "100%"),
                   actionButton("sa_load_from_sel", "Diesen Standort laden",
                                icon  = icon("location-dot"),
                                style = "width:100%; background:#1565C0; color:white;
                                font-size:11px; margin-top:4px;")
               ),
               hr(style = "margin:8px 0;"),
               #### ---- 1.2.4.3 Filter ----
               div(class = "sidebar-section",
                   tags$p(class = "section-title", "\u25B6 Filter"),
                   selectizeInput("sa_baumart", "Baumarten:",
                                  choices  = baumart_choices,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options  = list(placeholder = "leer = alle",
                                                  plugins = list("remove_button"))),
                   selectizeInput("sa_tv", "TVs:",
                                  choices  = tv_choices,
                                  selected = tv_choices,   # alle vorausgewaehlt
                                  multiple = TRUE,
                                  options  = list(placeholder = "leer = alle",
                                                  plugins = list("remove_button"))),
                   selectizeInput("sa_modell", "Modelle:",
                                  choices  = modell_choices,
                                  selected = modell_choices,   # alle vorausgewaehlt
                                  multiple = TRUE,
                                  options  = list(placeholder = "leer = alle",
                                                  plugins = list("remove_button"))),
                   selectizeInput("sa_szenario", "Szenarien (RCP):",
                                  choices  = szenario_choices,
                                  selected = szenario_choices,   # alle vorausgewaehlt
                                  multiple = TRUE,
                                  options  = list(placeholder = "leer = alle",
                                                  plugins = list("remove_button"))),
                   selectizeInput("sa_zeitraum", "Zeiträume:",
                                  choices  = zeitraum_choices,
                                  selected = zeitraum_choices,   # alle vorausgewaehlt
                                  multiple = TRUE,
                                  options  = list(placeholder = "leer = alle",
                                                  plugins = list("remove_button"))),
                   # RCP45-Varianten (Basis/v2/v3) explizit ein-/ausschalten.
                   # Nur einblenden, wenn es ueberhaupt Varianten gibt.
                   if (length(rcp45_var_choices) > 1)
                     checkboxGroupInput(
                       "sa_rcp45_var", "RCP45-Varianten:",
                       choices  = setNames(
                         rcp45_var_choices,
                         ifelse(rcp45_var_choices == "Basis",
                                "RCP45 (Basis)",
                                paste0("RCP45_", rcp45_var_choices))),
                       selected = rcp45_var_choices,
                       inline   = TRUE),
                   tags$label(class = "control-label", "Bewertungsstufe:"),
                   radioButtons("sa_stufe", label = NULL,
                                choices  = c("3-stufig"="BAE_3ST","4-stufig"="BAE_4ST",
                                             "5-stufig"="BAE_5ST"),
                                selected = "BAE_4ST", inline = TRUE),
                   tags$label(class = "control-label", "Ansicht:"),
                   radioButtons("sa_ansicht", label = NULL,
                                choices  = c(
                                  "Heatmap (Baumart \u00d7 TV)"  = "heatmap",
                                  "Heatmap Zukunft (RCP45/RCP85)" = "heatmap_zukunft",
                                  "Balken: nach Szenario"         = "szenario",
                                  "Balken: nach Zeitraum"         = "zeitraum",
                                  "Balken: nach Modell"           = "modell",
                                  "Grid (Szen. \u00d7 Zeitr.)"  = "grid"),
                                selected = "heatmap"),
                   actionButton("sa_run", "Analyse starten",
                                icon  = icon("play"),
                                style = "width:100%; background:#2E7D32; color:white;
                                font-weight:bold; margin-top:8px;")
               ),
               #### ---- 1.2.4.4 Download ----
               div(class = "sidebar-section",
                   tags$p(class = "section-title", "\u25B6 Export"),
                   downloadButton("sa_download_png", "Plot als PNG",
                                  style = "width:100%; margin-bottom:5px;"),
                   downloadButton("sa_download_csv", "Daten als CSV",
                                  style = "width:100%;")
               )
        ),
        column(9,
               #### ---- 1.2.4.5 Boden-Kontext ----
               uiOutput("sa_boden_kontext"),
               hr(style = "margin:6px 0 10px 0; border-color:#e0e0e0;"),
               #### ---- 1.2.4.6 Hauptplot ----
               plotOutput("sa_plot", height = "580px")
        )
      )
    ),
    
    ### ---- 1.2.5 Tab 4: Info ----
    tabPanel(
      title = tagList(icon("info-circle"), " Info"),
      fluidRow(
        column(6,
               # Legende
               div(class = "info-block", style = "margin:20px 10px 10px 20px;",
                   tags$h4("\u25B6 Empfehlungskategorien"),
                   lapply(names(kat_palette), function(k) {
                     tags$div(style = "display:flex; align-items:center; margin-bottom:6px;",
                              tags$div(style = paste0("width:14px; height:14px; border-radius:50%;",
                                                      " background:", kat_palette[k],
                                                      "; margin-right:9px; flex-shrink:0;")),
                              tags$span(k, style = "font-size:13px;"))
                   })
               ),
               
               # TV-Bezeichnungen
               div(class = "info-block", style = "margin:10px 10px 10px 20px;",
                   tags$h4("\u25B6 Teilvorhaben (TV)"),
                   tags$table(style = "width:100%; border-collapse:collapse; font-size:12px;",
                              tags$thead(tags$tr(
                                tags$th(style = "text-align:left; padding:4px 8px; border-bottom:1px solid #ddd;", "TV"),
                                tags$th(style = "text-align:left; padding:4px 8px; border-bottom:1px solid #ddd;", "Institution")
                              )),
                              tags$tbody(
                                lapply(seq_along(tv_bezeichnung), function(i) {
                                  tags$tr(
                                    tags$td(style = "padding:3px 8px; border-bottom:1px solid #f0f0f0;",
                                            names(tv_bezeichnung)[i]),
                                    tags$td(style = "padding:3px 8px; border-bottom:1px solid #f0f0f0; color:#444;",
                                            tv_bezeichnung[i])
                                  )
                                })
                              )
                   )
               )
        ),
        
        column(6,
               # Datei-Schema
               div(class = "info-block", style = "margin:20px 20px 10px 10px;",
                   tags$h4("\u25B6 Dateistruktur BWI"),
                   tags$p(style = "font-size:12px;",
                          "CSV-Namensschema der aggregierten BWI-Ergebnisse:"),
                   tags$pre(style = "font-size:11px; background:#f0f0f0; padding:8px; border-radius:4px;",
                            "BAE_{Szenario}_{Modell}_{Zeitraum}.csv\nz.B.: BAE_RCP45_ECECMO_1961-1990.csv"),
                   tags$p(style = "font-size:12px; margin-top:10px;",
                          "Verf\u00fcgbare BAE-Spalten je Datei-Typ:"),
                   tags$table(style = "width:100%; border-collapse:collapse; font-size:11px;",
                              tags$thead(tags$tr(
                                tags$th(style = "text-align:left; padding:3px 6px; border-bottom:1px solid #ddd;", "Typ"),
                                tags$th(style = "text-align:left; padding:3px 6px; border-bottom:1px solid #ddd;", "Spalten")
                              )),
                              tags$tbody(lapply(list(
                                list("4st",     "BAE_4st, BAE_3st"),
                                list("5st",     "BAE_5st, BAE_4st, BAE_3st"),
                                list("KM",      "BAE_5st, BAE_4st, BAE_3st"),
                                list("BAE20/21","BAE_7st, BAE_5st, BAE_4st, BAE_3st"),
                                list("3st",     "BAE_3st"),
                                list("kor/KH",  "BAE_4st, BAE_3st")
                              ), function(r) {
                                tags$tr(
                                  tags$td(style = "padding:2px 6px; border-bottom:1px solid #f0f0f0; font-weight:500;", r[[1]]),
                                  tags$td(style = "padding:2px 6px; border-bottom:1px solid #f0f0f0; color:#555;",      r[[2]])
                                )
                              }))
                   )
               ),
               
               # Datenquellen
               div(class = "info-block", style = "margin:10px 20px 10px 10px;",
                   tags$h4("\u25B6 Datenquellen"),
                   tags$ul(style = "font-size:12px; padding-left:18px;",
                           tags$li("BAE-Ergebnisse: lokal \u2013 04_results/BAE/BWI/"),
                           tags$li("Geodaten BWI-BZE: 01_data/Grundlagen/Geodaten/"),
                           tags$li("Bodendatenbank: 01_data/Grundlagen/Bodendatenbank/"),
                           tags$li("WM / NR-Rohdaten: extern via config.R (S:\\ oder E:\\)")
                   ),
                   tags$p(style = "font-size:11px; color:#999; margin-top:8px;",
                          paste0("App-Version: Phase 1 | Stand: ",
                                 format(Sys.Date(), "%d.%m.%Y")))
               )
        )
      )
    ),
    
    ### ---- 1.2.6 Tab: Klimakarten ----
    tabPanel(
      title = tagList(icon("image"), " Klimakarten"),
      fluidRow(
        column(3,
               div(class = "sidebar-section", style = "margin:20px 10px 10px 20px;",
                   tags$p(class = "section-title", "\u25B6 Klimakarte"),
                   tags$label(class = "control-label", "Kartentyp:"),
                   radioButtons("ks_typ", label = NULL,
                                choices = c(
                                  "Klimastufenmodell"          = "kfr_exp",
                                  "Klimastufe MV (A1b)"        = "kfr_new",
                                  "KS-Gliederung Schlutow"     = "ks_gl"
                                ),
                                selected = "kfr_exp"),
                   hr(style = "margin:8px 0;"),
                   tags$p(style = "font-size:11px; color:#888; margin:0;",
                          icon("info-circle"),
                          " Klimalauf-Auswahl aus Tab \u2018Karte\u2019 wird \u00fcbernommen."),
                   tags$p(style = "font-size:11px; color:#888; margin-top:4px;",
                          "Nur f\u00fcr BWI-BZE verf\u00fcgbar.")
               )
        ),
        column(9,
               div(style = "margin:20px 20px 10px 10px;",
                   uiOutput("ks_bild_ui")
               )
        )
      )
    )
  )  # end navbarPage
)  # end tagList

# ---- 2. Server ----

server <- function(input, output, session) {
  
  ## ---- 2.1 Caches & reaktive Grundwerte ----
  boden_cache    <- reactiveValues(data = list())
  wm_cache       <- reactiveValues(data = list())
  bzt_cache      <- reactiveValues(data = list())
  selected_punkt <- reactiveVal(NULL)
  
  # CSV-Cache: Rohdaten nur neu lesen wenn sich Klimalauf ändert,
  # NICHT bei Baumart/TV/Stufen-Wechsel
  ## ---- 2.2 Reaktive Daten (csv_raw, geo_daten, boden_region) ----
  
  csv_raw <- reactive({
    req(input$szenario, input$modell, input$zeitraum)
    row_meta <- klima_meta %>%
      filter(Szenario == input$szenario,
             Modell   == input$modell,
             Zeitraum == input$zeitraum)
    validate(need(nrow(row_meta) == 1,
                  paste0("Keine eindeutige CSV: ", input$szenario,
                         " / ", input$modell, " / ", input$zeitraum)))
    message("Lese CSV: ", basename(row_meta$file))
    data.table::fread(row_meta$file,
                      colClasses = list(character = "MASTER_ID"))
  })
  
  ### ---- 2.2.1 Reaktive Geodaten-Quelle ----
  geo_daten <- reactive({
    if (isTRUE(input$datenquelle == "NR")) {
      req(!is.null(NR_GEO_ALL), input$nr_sel)
      NR_GEO_ALL %>% filter(NR_ID == input$nr_sel)
    } else {
      BWI_GEO
    }
  })
  
  ### ---- 2.2.2 Reaktive Boden-DB ----
  boden_region <- reactive({
    if (isTRUE(input$datenquelle == "NR")) "NR" else "BWI"
  })
  
  ## ---- 2.3 Kaskadierung: Szenario -> Modell -> Zeitraum ----
  
  observeEvent(input$szenario, {
    mod <- klima_meta %>% filter(Szenario == input$szenario) %>%
      pull(Modell) %>% unique() %>% sort()
    updateSelectInput(session, "modell", choices = mod, selected = mod[1])
  }, ignoreNULL = TRUE)
  
  observeEvent(input$modell, {
    req(input$szenario)
    zr <- klima_meta %>%
      filter(Szenario == input$szenario, Modell == input$modell) %>%
      pull(Zeitraum) %>% unique() %>% sort()
    updateSelectInput(session, "zeitraum", choices = zr, selected = zr[1])
  }, ignoreNULL = TRUE)
  
  ## ---- 2.4 filtered(): Geodaten + BAE-Join ----
  
  filtered_raw <- eventReactive(input$run_karte, {
    req(input$szenario, input$modell, input$zeitraum,
        input$baumart_sel, input$tv_sel, input$stufe)
    
    is_nr <- isTRUE(input$datenquelle == "NR")
    
    ### ---- 2.4.1 CSV laden (BWI / NR) ----
    if (is_nr) {
      # NR: Pfad direkt konstruieren – kein Scan beim Start
      # Schema: {BAE_WM_DIR}/BAE_{TV}_{Stufe}/{NR}/{Szenario}/{Modell}/{Zeitraum}/
      #         BAE_{TV}_{Stufe}_{NR}_{Szenario}_{Modell}_{Zeitraum}_{Baumart}.csv
      validate(need(!is.null(BAE_WM_DIR) && dir.exists(BAE_WM_DIR),
                    "NR-Pfad nicht konfiguriert. Bitte BAE_WM_DIR in config.R setzen."))
      validate(need(!is.null(NR_GEO_ALL),
                    "NR-Geodaten nicht gefunden (Shapefile)."))
      req(input$nr_sel)
      
      # NR-Ergebnisdateien per list.files + grep holen. Ordner-/Dateinamen
      # variieren (Modellspez. KH/KM/AltBA..., NR-Token gross/klein), daher
      # kein starres Pfad-Bauen.
      # Struktur: {BAE_WM_DIR}/BAE_{TV}_..._{stufe}/{NR}/{Szen}/{Modell}/{Zeitr}/
      #   Datei:  BAE_{TV}_..._{stufe}_{nrNN}_{Szen}_{Modell}_{Zeitr}_{Baumart}.csv
      tv_pad    <- sprintf("%02d", as.integer(input$tv_sel))
      stufe_suf <- tolower(sub("^BAE_", "", input$stufe))   # "BAE_5ST" -> "5st"
      
      # 1) TV-Ordner (eine Ebene) auf gewaehlten TV [+ Stufe] eingrenzen.
      tv_dirs <- list.dirs(BAE_WM_DIR, recursive = FALSE, full.names = TRUE)
      tv_dirs <- tv_dirs[grepl(paste0("^BAE_", tv_pad, "_"),
                               basename(tv_dirs), ignore.case = TRUE)]
      tv_stufe <- tv_dirs[grepl(paste0("_", stufe_suf, "$"),
                                basename(tv_dirs), ignore.case = TRUE)]
      if (length(tv_stufe) > 0) tv_dirs <- tv_stufe   # Stufe nur weich filtern
      
      validate(need(length(tv_dirs) > 0,
                    paste0("Kein BAE-Ordner f\u00fcr TV", input$tv_sel,
                           " unter ", BAE_WM_DIR)))
      
      # 2) In den NR-Unterordner absteigen (Ordnername case-insensitiv).
      nr_dirs <- unlist(lapply(tv_dirs, function(d) {
        sub <- list.dirs(d, recursive = FALSE, full.names = TRUE)
        sub[grepl(paste0("^", input$nr_sel, "$"),
                  basename(sub), ignore.case = TRUE)]
      }))
      validate(need(length(nr_dirs) > 0,
                    paste0("Kein Ordner ", input$nr_sel,
                           " unter den TV", input$tv_sel, "-Ordnern gefunden.")))
      
      # 3) Direkt in den Klimalauf-Unterordner {Szen}/{Modell}/{Zeitraum} steigen
      #    (bestaetigte Struktur) und nur dort listen - kein rekursiver Scan ueber
      #    den gesamten NR-Teilbaum (alle 38 Klimalaeufe x Baumarten).
      leaf_dirs <- file.path(nr_dirs, input$szenario, input$modell, input$zeitraum)
      leaf_dirs <- leaf_dirs[dir.exists(leaf_dirs)]
      alle_csv  <- if (length(leaf_dirs) > 0)
        list.files(leaf_dirs, pattern = "\\.csv$", full.names = TRUE) else character(0)
      # Fallback: falls die erwartete Struktur mal nicht passt, doch rekursiv.
      if (length(alle_csv) == 0)
        alle_csv <- list.files(nr_dirs, pattern = "\\.csv$",
                               recursive = TRUE, full.names = TRUE)
      
      # 4) Per grep auf NR / Szenario / Modell / Zeitraum / Baumart filtern.
      ba_pat <- paste0("_(", paste(input$baumart_sel, collapse = "|"), ")\\.csv$")
      bn     <- basename(alle_csv)
      match_files <- unique(alle_csv[
        grepl(paste0("_", input$nr_sel,    "_"), bn, ignore.case = TRUE) &
          grepl(paste0("_", input$szenario,  "_"), bn) &
          grepl(paste0("_", input$modell,    "_"), bn) &
          grepl(paste0("_", input$zeitraum,  "_"), bn) &
          grepl(ba_pat, bn)
      ])
      
      message("NR-CSV-Treffer: ", length(match_files),
              if (length(match_files) > 0)
                paste0(" (z.B. ", basename(match_files[1]), ")") else "")
      
      validate(need(length(match_files) > 0,
                    paste0("Keine NR-CSV gefunden f\u00fcr: ",
                           input$nr_sel, " / TV", input$tv_sel, " / ",
                           input$szenario, " / ", input$modell, " / ",
                           input$zeitraum, " / ",
                           paste(input$baumart_sel, collapse = ", "))))
      
      # Typ-Normalisierung: BAE_*-Spalten koennen in NR-Dateien unterschiedliche
      # Typen haben (integer vs. character) -> "Can't combine BAE_4ST integer and character".
      # Fix: alle BAE_*-Spalten vor dem Binden explizit auf character setzen.
      df_raw <- data.table::rbindlist(
        lapply(match_files, function(f) {
          dt <- data.table::fread(f, fill = TRUE)
          if ("MASTER_ID" %in% names(dt))
            data.table::set(dt, j = "MASTER_ID", value = as.character(dt$MASTER_ID))
          bae_cols <- grep("^BAE_", names(dt), ignore.case = TRUE, value = TRUE)
          for (col in bae_cols)
            data.table::set(dt, j = col, value = as.character(dt[[col]]))
          # Baumart steckt nur im Dateinamen (..._{Baumart}.csv), nicht in den
          # Daten -> als Spalte mitfuehren, damit sie nach dem Binden vorhanden ist.
          dt$Baumart <- sub("\\.csv$", "", sub(".*_", "", basename(f)))
          dt
        }),
        fill = TRUE
      )
    } else {
      # BWI: Pre-aggregierte CSV
      # csv_raw() ist gecacht – wird nur bei Klimalauf-Änderung neu gelesen
      df_raw <- csv_raw()
    }
    
    ### ---- 2.4.2 Dynamische Stufenerkennung ----
    verfuegbar <- intersect(
      c("BAE_3ST", "BAE_4ST", "BAE_5ST", "BAE_7ST"),
      toupper(names(df_raw))
    )
    stufe_labels <- c("BAE_3ST" = "3-stufig", "BAE_4ST" = "4-stufig",
                      "BAE_5ST" = "5-stufig", "BAE_7ST" = "7-stufig")
    # choices als benannter Vektor Label -> Wert (wie im Ausgangs-UI):
    # names = Anzeige ("5-stufig"), Werte = Rueckgabe ("BAE_5ST"). NICHT
    # stufe_labels[verfuegbar] direkt nehmen - das ist invertiert (Name=BAE_5ST,
    # Wert="5-stufig") und liefert input$stufe = "5-stufig", das nie in
    # verfuegbar liegt -> bae_col fiele immer auf die letzte Stufe zurueck.
    updateRadioButtons(session, "stufe",
                       choices  = setNames(verfuegbar, unname(stufe_labels[verfuegbar])),
                       selected = if (input$stufe %in% verfuegbar) input$stufe
                       else verfuegbar[length(verfuegbar)])
    
    bae_col <- if (input$stufe %in% verfuegbar) input$stufe else
      verfuegbar[length(verfuegbar)]
    
    ### ---- 2.4.3 Filtern & Baumart-Spalte vereinheitlichen ----
    # Die Roh-CSVs tragen die Filterkriterien (TV, Baumart, Region, Klimalauf)
    # nur im Dateinamen, nicht als Spalten. In den Daten stehen MASTER_ID +
    # Stufenspalten (BAE_3st/4st/5st, je nach Modell) + modellspezifische Extras.
    #  - NR: Baumart wurde beim Laden aus dem Dateinamen als Spalte ergaenzt,
    #        eine TV-Spalte gibt es nicht.
    #  - BWI: das vor-aggregierte CSV (csv_raw) enthaelt BAUMART und TV als Spalten.
    # Daher Filter/Rename nur anwenden, wenn die jeweilige Spalte existiert.
    df <- df_raw %>% rename_with(toupper)
    if ("BAUMART" %in% names(df) && !is_nr)
      df <- df %>% filter(BAUMART %in% input$baumart_sel)
    if ("TV" %in% names(df))
      df <- df %>% filter(TV %in% as.integer(input$tv_sel))
    if ("BAUMART" %in% names(df)) {
      df <- df %>% rename(Baumart = BAUMART)
    } else if (!"Baumart" %in% names(df)) {
      df$Baumart <- if (length(input$baumart_sel) == 1) input$baumart_sel
      else NA_character_
    }
    df <- df %>%
      mutate(Szenario    = input$szenario,
             Modell      = input$modell,
             Zeitraum    = input$zeitraum,
             Datenquelle = if (is_nr) input$nr_sel else "BWI-BZE")
    
    validate(need(nrow(df) > 0, "Keine Daten f\u00fcr die gew\u00e4hlte Kombination."))
    
    ### ---- 2.4.4 Dedup vor Geo-Join ----
    # TV ist auf der Karte einwertig (Mehrfach-TV nur in Standortanalyse/Vergleich),
    # daher keine TV-Zusammenfassung mehr noetig. Ein distinct() je MASTER_ID+
    # Baumart genuegt als Schutz vor einem kartesischen Geo-Join.
    df <- df %>% distinct(MASTER_ID, Baumart, .keep_all = TRUE)
    
    ### ---- 2.4.5 Geo-Join (abgesichert) ----
    geo <- geo_daten()
    joined <- tryCatch({
      res <- geo %>%
        left_join(df, by = "MASTER_ID") %>%
        filter(!is.na(Baumart))
      # Sicherheitsnetz falls trotzdem Zeilen vervielfacht
      if (nrow(res) > nrow(geo) * max(length(input$baumart_sel), 1) * 3) {
        res <- res %>% group_by(MASTER_ID, Baumart) %>% slice(1) %>% ungroup()
      }
      res
    }, error = function(e) {
      validate(need(FALSE, paste0(
        "Geo-Join fehlgeschlagen (m\u00f6glicherweise doppelte MASTER_IDs).\n",
        conditionMessage(e))))
    })
    
    validate(need(nrow(joined) > 0,
                  "Join ohne Treffer \u2013 MASTER_ID-\u00dcbereinstimmung pr\u00fcfen."))
    
    ### ---- 2.4.6 Rohdaten zurueckgeben (Einfaerbung erfolgt in filtered()) ----
    # Einfaerbung bewusst NICHT hier: filtered_raw() ist an input$run_karte
    # gebunden und laedt die (schweren) CSV/NR-Daten. Die Farbe haengt nur an
    # input$stufe / input$farb_modus und wird in filtered() live nachgerechnet,
    # ohne die Daten erneut zu laden.
    joined
  })

  ## ---- 2.4.7 Einfaerbung (live, ohne Reload) ----
  # Leichtes reactive() ueber filtered_raw(): reagiert zusaetzlich auf
  # input$stufe und input$farb_modus, damit ein Stufen-/Farb-Moduswechsel die
  # Karte SOFORT umfaerbt. filtered_raw() bleibt gecacht (nur "Karte erstellen"
  # laedt neu). Alle bisherigen Aufrufer nutzen unveraendert filtered().
  filtered <- reactive({
    joined <- filtered_raw()
    if (isTRUE(input$farb_modus == "baumart")) {
      baumarten <- sort(unique(joined$Baumart))
      ba_farben <- setNames(baumart_farben_basis[seq_along(baumarten)], baumarten)
      joined <- joined %>%
        mutate(Kat   = Baumart,
               Farbe = unname(ba_farben[Baumart]))
    } else {
      verfuegbar <- intersect(c("BAE_3ST", "BAE_4ST", "BAE_5ST", "BAE_7ST"),
                              names(joined))
      bae_col    <- if (input$stufe %in% verfuegbar) input$stufe
                    else verfuegbar[length(verfuegbar)]
      joined <- joined %>%
        mutate(Kat   = map_stufe(.data[[bae_col]], bae_col),
               Farbe = dplyr::coalesce(unname(kat_palette[Kat]), "#B0B0B0"))
    }
    joined
  })
  
  ## ---- 2.5 Panel-Reset bei Filteraenderung ----
  observeEvent(
    list(input$szenario, input$modell, input$zeitraum,
         input$baumart_sel, input$tv_sel, input$stufe,
         input$farb_modus, input$datenquelle, input$nr_sel),
    { selected_punkt(NULL) }, ignoreInit = TRUE
  )
  
  ## ---- 2.6 Trefferanzahl ----
  output$treffer_box <- renderUI({
    df <- tryCatch(filtered(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    n_punkte  <- fmt_n(nrow(df))
    n_ba      <- length(unique(df$Baumart))
    tv_labels <- names(tv_bezeichnung)[tv_bezeichnung %in% input$tv_sel]
    tv_kurz   <- gsub(":.*", "", tv_labels)
    
    tags$div(class = "treffer-box",
             tags$b(paste0("N = ", n_punkte, " Punkte")), tags$br(),
             paste0(n_ba, " Baumart", if (n_ba != 1) "en", "  |  ",
                    paste(tv_kurz, collapse = ", "))
    )
  })
  
  ## ---- 2.7 Legende (reaktiv je Farb-Modus) ----
  output$legende_ui <- renderUI({
    df <- tryCatch(filtered(), error = function(e) NULL)
    
    if (!is.null(df) && isTRUE(input$farb_modus == "baumart")) {
      baumarten <- sort(unique(df$Baumart))
      ba_farben <- setNames(baumart_farben_basis[seq_along(baumarten)], baumarten)
      lapply(baumarten, function(ba) {
        tags$div(style = "display:flex; align-items:center; margin-bottom:4px;",
                 tags$div(style = paste0("width:12px; height:12px; border-radius:50%;",
                                         " background:", ba_farben[ba],
                                         "; margin-right:7px; flex-shrink:0;")),
                 tags$span(ba, style = "font-size:11px;"))
      })
    } else {
      lapply(names(kat_palette), function(k) {
        tags$div(style = "display:flex; align-items:center; margin-bottom:4px;",
                 tags$div(style = paste0("width:12px; height:12px; border-radius:50%;",
                                         " background:", kat_palette[k],
                                         "; margin-right:7px; flex-shrink:0;")),
                 tags$span(k, style = "font-size:11px;"))
      })
    }
  })
  
  ## ---- 2.8 Karten-Status ----
  # Einziger Output ohne tryCatch um filtered(): hier erscheinen die
  # validate()-Meldungen (z.B. "Keine NR-CSV gefunden für: ...") sichtbar.
  output$karte_status <- renderUI({
    filtered()
    NULL
  })
  
  ## ---- 2.9 NR-Auswahl: Vorschau + Zoom ----
  # Reagiert direkt auf Datenquelle-/NR-Wechsel, damit die Auswahl sichtbar
  # etwas tut, bevor "Karte erstellen" geklickt wird: Es wird NUR der 25-km-
  # Umriss der Nachbarschaftsregion gezeichnet (Gruppe "nr_preview") und darauf
  # gezoomt. Die einzelnen Standortpolygone (zehntausende je NR) zeichnet erst
  # der Haupt-Render nach "Karte erstellen" - sonst blockiert allein die Auswahl.
  observeEvent(list(input$datenquelle, input$nr_sel), {
    proxy <- leafletProxy("map") %>%
      clearGroup("nr_preview") %>% clearGroup("nr_punkte")
    if (!isTRUE(input$datenquelle == "NR")) return(invisible(NULL))
    req(input$nr_sel)
    
    if (is.null(NR_UMKREIS_ALL)) {
      showNotification("NR-Umkreise (Shapefile) nicht geladen – siehe Konsole.",
                       type = "error", duration = 8)
      return(invisible(NULL))
    }
    umkreis <- NR_UMKREIS_ALL %>% filter(NR_ID == input$nr_sel)
    if (nrow(umkreis) == 0) {
      showNotification(paste0(
        "Kein Umkreis-Polygon für ", input$nr_sel, ". Vorhanden: ",
        paste(sort(unique(NR_UMKREIS_ALL$NR_ID)), collapse = ", ")),
        type = "warning", duration = 10)
      return(invisible(NULL))
    }
    bb <- sf::st_bbox(umkreis)
    proxy %>%
      addPolygons(data = umkreis, fill = FALSE, color = "#2E7D32",
                  weight = 2, dashArray = "6", group = "nr_preview") %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }, ignoreInit = TRUE)
  
  ## ---- 2.10 Basiskarte ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  ## ---- 2.11 Karte-Reset ----
  observeEvent(input$map_reset, {
    leafletProxy("map") %>% setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  ## ---- 2.12 Marker rendern (BWI / NR) ----
  observe({
    df <- filtered()
    req(nrow(df) > 0)
    is_nr  <- isTRUE(input$datenquelle == "NR")
    r      <- input$punktgroesse %||% 4
    
    popups <- paste0(
      "<b>MASTER_ID:</b> ", df$MASTER_ID, "<br>",
      "<b>Baumart:</b> ",   df$Baumart,   "<br>",
      "<b>TV:</b> ",        df$TV,        "<br>",
      "<b>Kategorie:</b> ", df$Kat,       "<br>",
      "<b>Klimalauf:</b> ", df$Szenario, " / ", df$Modell, " / ", df$Zeitraum,
      if (is_nr) paste0("<br><b>NR:</b> ", df$Datenquelle) else "", "<br>",
      "<i style='color:grey'>Klick l\u00e4dt Bodendaten...</i>"
    )
    
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    if (is_nr) {
      # Geometrietyp der gefilterten NR-Daten bestimmen.
      # Reales GEO_NR.shp kann POINT-Geometrie enthalten (keine Flaechen-Polygone).
      # In diesem Fall addCircleMarkers statt addPolygons verwenden.
      # Enthaelt filtered() noch veraltete BWI-Punktdaten (Nutzer hat Datenquelle
      # gewechselt ohne run_karte zu druecken), werden diese ebenfalls als Punkte
      # gerendert – der Nutzer sieht die alten Punkte bis er neu laedt.
      nr_geom_type <- unique(as.character(sf::st_geometry_type(df)))
      proxy <- proxy %>% clearGroup("nr_preview")
      if (any(grepl("POLYGON", nr_geom_type, ignore.case = TRUE))) {
        proxy <- proxy %>%
          addPolygons(
            data        = df,
            fillColor   = df$Farbe,
            fillOpacity = 0.75,
            color       = "#555555",
            weight      = 0.6,
            popup       = popups,
            layerId     = df$MASTER_ID,
            group       = df$Baumart
          )
      } else {
        # NR-Shapefile mit POINT-Geometrie (Realdaten)
        coords_nr <- sf::st_coordinates(df)
        proxy <- proxy %>%
          addCircleMarkers(
            lng = coords_nr[, 1], lat = coords_nr[, 2],
            color = df$Farbe, fillColor = df$Farbe,
            fillOpacity = 0.8, radius = r, stroke = FALSE,
            popup = popups, layerId = df$MASTER_ID, group = df$Baumart
          )
      }
      bb <- sf::st_bbox(df)
      proxy <- proxy %>%
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    } else {
      # BWI: Punkte (kein Clustering – Clustering blockiert Popup-Update nach Klick)
      coords <- sf::st_coordinates(df)
      proxy <- proxy %>%
        addCircleMarkers(
          lng = coords[, 1], lat = coords[, 2],
          color = df$Farbe, fillColor = df$Farbe,
          fillOpacity = 0.8, radius = r, stroke = FALSE,
          popup = popups, layerId = df$MASTER_ID, group = df$Baumart
        )
    }
    
    proxy %>%
      addLayersControl(
        overlayGroups = unique(df$Baumart),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  ## ---- 2.13 Klick-Handler: Bodendaten laden ----
  ### ---- 2.13.1 Gemeinsamer Klick-Handler (BWI + NR) ----
  handle_punkt_click <- function(clicked_id) {
    master_id <- as.character(clicked_id)
    # NULL oder "" → Cluster-Klick (kein einzelner Marker), ignorieren
    if (is.null(master_id) || nchar(master_id) == 0) return(invisible(NULL))
    
    message("Punkt geklickt: ", master_id)
    
    # Bodendaten laden – Fehler dürfen Handler nicht abbrechen
    if (is.null(boden_cache$data[[master_id]])) {
      withProgress(message = paste0("Lade Bodendaten: ", master_id), value = 0.5, {
        boden_cache$data[[master_id]] <- tryCatch(
          get_boden(master_id, region = boden_region()),
          error = function(e) {
            message("Bodendaten Fehler: ", e$message)
            NULL
          }
        )
      })
    }
    
    # filtered() direkt – KEIN tryCatch, damit req()-Bedingungen propagieren
    df    <- filtered()
    d_mid <- df %>% filter(as.character(MASTER_ID) == master_id)
    if (nrow(d_mid) == 0) {
      message("MASTER_ID nicht in gefilterten Daten: ", master_id)
      return(invisible(NULL))
    }
    
    boden      <- boden_cache$data[[master_id]]
    # Koordinaten: bei NR X_CENTROID/Y_CENTROID aus Attributen nutzen,
    # bei BWI st_coordinates direkt auf Punkt-Geometrie
    if (isTRUE(input$datenquelle == "NR") &&
        all(c("X_CENTROID", "Y_CENTROID") %in% names(d_mid))) {
      coords <- c(as.numeric(d_mid$X_CENTROID[1]),
                  as.numeric(d_mid$Y_CENTROID[1]))
    } else {
      coords <- sf::st_coordinates(d_mid[1, ])[1, ]
    }
    # BZT-Daten laden (klimalauf-spezifisch, lazy, gecacht)
    bzt_key    <- paste(master_id, input$szenario, input$modell, input$zeitraum, sep = "|")
    bzt_region <- if (isTRUE(input$datenquelle == "NR")) input$nr_sel else "BWI"
    if (is.null(bzt_cache$data[[bzt_key]])) {
      bzt_cache$data[[bzt_key]] <- tryCatch(
        get_bzt(master_id = master_id, region    = bzt_region,
                szenario  = input$szenario, modell = input$modell,
                zeitraum  = input$zeitraum),
        error = function(e) { message("BZT Fehler: ", e$message); NULL }
      )
    }
    bzt        <- bzt_cache$data[[bzt_key]]
    popup_html <- make_popup(d_mid[1, ], boden, bzt = bzt)
    
    # WM-Daten laden (lazy, gecacht)
    wm_key <- paste(master_id, d_mid$Baumart[1],
                    input$szenario, input$modell, input$zeitraum, sep = "|")
    if (is.null(wm_cache$data[[wm_key]])) {
      wm_region <- if (isTRUE(input$datenquelle == "NR")) input$nr_sel else "BWI-BZE"
      # tryCatch: WM-Fehler duerfen handle_punkt_click nicht abbrechen –
      # sonst wird selected_punkt() nie gesetzt und das Panel bleibt leer
      wm_cache$data[[wm_key]] <- tryCatch(
        get_wm(master_id = master_id,
               baumart   = d_mid$Baumart[1],
               szenario  = input$szenario,
               modell    = input$modell,
               zeitraum  = input$zeitraum,
               region    = wm_region),
        error = function(e) {
          message("WM-Fehler (ignoriert): ", e$message)
          NULL
        }
      )
    }
    
    # Karte ZUERST updaten (wie save_20260504.R: Map-Update vor Panel-Update).
    # WICHTIG: Dispatch auf den tatsaechlichen Geometrietyp von d_mid, NICHT
    # auf input$datenquelle. Wenn der Nutzer die Datenquelle wechselt ohne
    # run_karte zu druecken, haelt filtered() noch die alten Daten; der Klick
    # auf einen sichtbaren Altmarker kombiniert mit is_nr=TRUE wuerde sonst
    # addPolygons auf POINT-Geometrie aufrufen -> to_ring.default-Crash +
    # selected_punkt() wird nie gesetzt -> Panel bleibt leer.
    # Auerdem: reale NR-Daten koennen selbst POINT-Geometrie enthalten.
    d_geom_type <- unique(as.character(sf::st_geometry_type(d_mid[1, ])))
    if (any(grepl("POLYGON", d_geom_type, ignore.case = TRUE))) {
      leafletProxy("map") %>%
        addPolygons(data = d_mid[1, ],
                    fillColor = d_mid$Farbe[1], fillOpacity = 0.85,
                    color = "#2E7D32", weight = 2,
                    popup = popup_html,
                    layerId = master_id, group = d_mid$Baumart[1])
    } else {
      leafletProxy("map") %>%
        addCircleMarkers(
          lng = coords[1], lat = coords[2],
          color = d_mid$Farbe[1], fillColor = d_mid$Farbe[1],
          fillOpacity = 0.8, radius = input$punktgroesse %||% 4,
          stroke = FALSE, popup = popup_html,
          layerId = master_id, group = d_mid$Baumart[1])
    }
    
    # Dann Panel setzen
    selected_punkt(list(
      punkt = d_mid[1, ],
      boden = boden,
      wm    = wm_cache$data[[wm_key]],
      bzt   = bzt
    ))
  }  # end handle_punkt_click
  
  # BWI: CircleMarker-Klick
  # KEIN ignoreNULL=FALSE: as.character(NULL)=character(0), nchar()=integer(0),
  # integer(0)>0=logical(0) -> if(logical(0)) wirft "argument is of length zero"
  # -> Observer bricht ab. Stattdessen req(click$id) wie save_20260504.R.
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    handle_punkt_click(as.character(click$id))
  })
  
  # NR: Polygon-Klick (analog)
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click$id)
    handle_punkt_click(as.character(click$id))
  })
  
  ## ---- 2.14 Info-Panel unter Karte ----
  output$boden_panel <- renderUI({
    sel <- selected_punkt()
    
    if (is.null(sel)) {
      return(tags$div(
        style = paste("margin-top:10px; padding:9px 14px; border-radius:6px;",
                      "background:#f5f5f5; color:#999; font-size:12px;"),
        icon("hand-pointer"),
        " Punkt anklicken f\u00fcr Standort- und Bodendaten."))
    }
    
    d     <- sel$punkt
    boden <- sel$boden
    wm_df <- sel$wm    # kann NULL sein wenn kein WM_DIR
    bzt   <- sel$bzt   # kann NULL sein wenn BZT_CSV_DIR nicht konfiguriert
    
    # BZT-Standortdaten aufbereiten
    bzt_ok  <- !is.null(bzt) && nrow(bzt) > 0
    is_bwi  <- isFALSE(input$datenquelle == "NR")
    ks_label <- if (bzt_ok) {
      z <- bzt[1, ]
      if (is_bwi && "KS_gl" %in% names(z) && !is.na(z$KS_gl) && nchar(z$KS_gl) > 0)
        paste0(z$pred_exp2 %||% "\u2013", "  (KS-Gl: ", z$KS_gl, ")")
      else as.character(z$pred_exp2 %||% "\u2013")
    } else "\u2013"
    
    block_standort <- tags$div(
      style = "flex:0 0 230px; min-width:190px;",
      tags$h6(style = "color:#2E7D32; margin:0 0 6px; font-weight:bold;",
              "\u25B6 Standort"),
      boden_zeile("MASTER_ID",   d$MASTER_ID),
      boden_zeile("Baumart",     d$Baumart),
      boden_zeile("TV",          d$TV),
      boden_zeile("Kategorie",   d$Kat),
      boden_zeile("Szenario",    d$Szenario),
      boden_zeile("Modell",      d$Modell),
      boden_zeile("Zeitraum",    d$Zeitraum),
      if (bzt_ok) tagList(
        tags$hr(style = "margin:4px 0; border-color:#ddd;"),
        boden_zeile("Klimastufe", ks_label),
        boden_zeile("Info BAE",   bzt[1, "info_BAE"] %||% "\u2013"),
        boden_zeile("Hangseite",  bzt[1, "Hangseite"] %||% "\u2013"),
        boden_zeile("Position",   bzt[1, "Position"]  %||% "\u2013")
      )
    )
    
    if (!is.null(boden) && nrow(boden) > 0) {
      b <- boden[1, ]
      block_bodentyp <- tags$div(
        style = "flex:0 0 220px; min-width:180px;",
        tags$h6(style = "color:#2E7D32; margin:0 0 6px; font-weight:bold;",
                "\u25B6 Bodentyp"),
        boden_zeile("Bodentyp",      b$BODTYP),
        if (!is.null(b$STAOTYP) && !is.na(b$STAOTYP))
          boden_zeile("Staotyp",       b$STAOTYP),
        boden_zeile("N\u00e4hrkraft", b$NAEHR),
        if (!is.null(b$NKST) && !is.na(b$NKST))
          boden_zeile("NKST",          b$NKST),
        boden_zeile("Wasser",        b$WASSER),
        boden_zeile("Staoform kurz", b$SOEH_KRZ),
        boden_zeile("Staoform lang", b$SOEH_LNG),
        boden_zeile("NFK (mm)",      b$NFK_DEHNER),
        boden_zeile("NFK m.Aufl.",   b$NFK_DEHNER_AUFLAGE),
        boden_zeile("Tmax NFK",      b$Tmax_NFK),
        if (!is.null(b$STAOAGG) && !is.na(b$STAOAGG))
          boden_zeile("Stao-Agg",      b$STAOAGG),
        if (!is.null(b$STGR_final) && !is.na(b$STGR_final))
          boden_zeile("STGR final",    b$STGR_final),
        boden_zeile("Substrat",      b$STRATI),
        boden_zeile("Bodenart",      b$BODART),
        if (!is.null(b$Sub_feu_stf_back) && !is.na(b$Sub_feu_stf_back))
          boden_zeile("Substratfeuchte", b$Sub_feu_stf_back)
      )
      block_profil <- tags$div(
        style = "flex:0 0 200px; min-width:160px;",
        tags$h6(style = "color:#2E7D32; margin:0 0 6px; font-weight:bold;",
                "\u25B6 Profil"),
        if (isTRUE(b$lp_ersatz))
          tags$div(
            style = "font-size:11px; color:#b26a00; font-style:italic;
                     margin:0 0 5px; line-height:1.35;",
            paste0("Profil (", b$group_ID, ") nicht vorhanden. ",
                   b$lookup_group_ID, " zeigt diese Werte:")),
        boden_zeile("Tiefe (cm)",  paste0(b$Tiefe_OG_min, " \u2013 ", b$Tiefe_UG_max)),
        boden_zeile("Schichten",   b$n_Schichten),
        boden_zeile("TRD",         b$TRD),
        boden_zeile("Grundwasser", b$GRUNDH20),
        boden_zeile("Stauwasser",  b$STAUH20),
        boden_zeile("SOC",         b$SOC)
      )
      block_koernung <- tags$div(
        style = "flex:0 0 200px; min-width:160px;",
        tags$h6(style = "color:#2E7D32; margin:0 0 6px; font-weight:bold;",
                "\u25B6 K\u00f6rnung (\u00d8 %)"),
        boden_zeile("Sand",      paste0(b$SAND,       " %")),
        boden_zeile("Schluff",   paste0(b$SCHLUFF,    " %")),
        boden_zeile("Ton",       paste0(b$TON,        " %")),
        boden_zeile("Feinsand",  paste0(b$FEINSAND,   " %")),
        boden_zeile("Mittelsand",paste0(b$MITTELSAND, " %")),
        boden_zeile("Grobsand",  paste0(b$GROBSAND,  " %")),
        boden_zeile("Skelett",   paste0(b$SKELETT,    " %"))
      )
      block_chemie <- tags$div(
        style = "flex:0 0 200px; min-width:160px;",
        tags$h6(style = "color:#2E7D32; margin:0 0 6px; font-weight:bold;",
                "\u25B6 Chemie (\u00d8)"),
        boden_zeile("SOC",      b$SOC),
        boden_zeile("Carbonat", b$CARBONAT),
        boden_zeile("Basen",    b$BASEN),
        boden_zeile("C/N",      b$CN)
      )
      # Block BZT-Maximale (nur BWI/BZE: BZT_Combine + RBU..HBU)
      bzt_cols_all <- c("RBU","REI","SEI","TEI","GKI","ELA","WTA","GDG","GFI","GBI","BAH","HBU")
      block_bzt <- if (bzt_ok && is_bwi) {
        z <- bzt[1, ]
        stgr_str <- if ("STGR_pv1"  %in% names(z) && !is.na(z$STGR_pv1))  z$STGR_pv1  else
          if ("STGR_Kart" %in% names(z) && !is.na(z$STGR_Kart)) z$STGR_Kart else "\u2013"
        bzt_max_zeilen <- lapply(bzt_cols_all, function(col) {
          if (!col %in% names(z)) return(NULL)
          val <- z[[col]]
          if (is.na(val)) return(NULL)
          boden_zeile(col, val)
        })
        bzt_max_zeilen <- Filter(Negate(is.null), bzt_max_zeilen)
        tags$div(
          style = "flex:0 0 210px; min-width:180px;",
          tags$h6(style = "color:#1565C0; margin:0 0 6px; font-weight:bold;",
                  "\u25B6 BZT-Maximale"),
          if ("BZT_Combine" %in% names(z) && !is.na(z$BZT_Combine) && nchar(z$BZT_Combine) > 0)
            boden_zeile("BZT-Auswahl", z$BZT_Combine),
          boden_zeile("STGR", stgr_str),
          if (length(bzt_max_zeilen) > 0) bzt_max_zeilen
          else tags$div(style="font-size:11px;color:#aaa;", "Keine BZT-Werte")
        )
      } else NULL
      
      inhalt <- tagList(block_standort, block_bodentyp, block_profil,
                        block_koernung, block_chemie,
                        if (!is.null(block_bzt)) block_bzt,
                        render_wm_block(wm_df))
    } else {
      inhalt <- tagList(
        block_standort,
        tags$div(style = "flex:1; color:#999; font-size:13px; padding-top:18px;",
                 icon("exclamation-circle"),
                 " Keine Bodendaten f\u00fcr diese MASTER_ID."),
        render_wm_block(wm_df)
      )
    }
    
    tags$div(
      style = paste("margin-top:10px; padding:12px 16px; border-radius:6px;",
                    "border:1px solid #ddd; background:#fafafa;"),
      # Export-Button — nur wenn Punkt ausgewählt
      div(style = "display:flex; justify-content:flex-end; margin-bottom:8px;",
          downloadButton("download_standortblatt",
                         "Standortblatt PDF",
                         icon  = icon("file-pdf"),
                         style = "font-size:11px; padding:4px 10px;
                                background:#2E7D32; color:white; border:none;")
      ),
      tags$div(style = "display:flex; flex-wrap:wrap; gap:22px; align-items:flex-start;",
               inhalt)
    )
  })
  
  ## ---- 2.15 Export HTML ----
  output$save_html <- downloadHandler(
    filename = function() {
      paste0("BAE_", input$szenario, "_", input$modell, "_",
             input$zeitraum, "_", input$stufe, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      df     <- filtered()
      coords <- sf::st_coordinates(df)
      popups <- paste0(
        "<b>MASTER_ID:</b> ", df$MASTER_ID, "<br>",
        "<b>Baumart:</b> ",   df$Baumart,   "<br>",
        "<b>TV:</b> ",        df$TV,        "<br>",
        "<b>Kategorie:</b> ", df$Kat,       "<br>",
        "<b>Klimalauf:</b> ", df$Szenario, " / ", df$Modell, " / ", df$Zeitraum
      )
      m <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
        addCircleMarkers(
          lng = coords[, 1], lat = coords[, 2],
          color = df$Farbe, fillColor = df$Farbe,
          fillOpacity = 0.8, radius = input$punktgroesse %||% 4,
          stroke = FALSE, popup = popups, group = df$Baumart
        ) %>%
        addLayersControl(overlayGroups = unique(df$Baumart),
                         options = layersControlOptions(collapsed = FALSE))
      htmlwidgets::saveWidget(m, file = file, selfcontained = TRUE)
    }
  )
  
  ## ---- 2.16 Export PNG (ggplot) ----
  # PNG-Download der aktuellen Karte (Browser-Download, analog zu save_html).
  # Die Karte wird als ggplot mit geom_sf neu gerendert (unabhaengig vom
  # interaktiven Leaflet) und direkt in die vom Browser gelieferte Datei
  # geschrieben - kein serverseitiges Verzeichnis mehr.
  output$save_png <- downloadHandler(
    filename = function() {
      paste0("BAE_", input$szenario, "_", input$modell, "_",
             input$zeitraum, "_", input$stufe, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
    df <- filtered()
    validate(need(!is.null(df) && nrow(df) > 0,
                  "Bitte zuerst Filter wählen und 'Karte erstellen' klicken."))

    withProgress(message = "Erstelle Karte...", value = 0.2, {
      is_nr <- isTRUE(input$datenquelle == "NR")
      if (is_nr) {
        df_plot <- sf::st_drop_geometry(df)        # NR: geom_sf() braucht kein lon/lat
      } else {
        coords  <- sf::st_coordinates(df)          # BWI: Punkte, 1:1-Verhältnis, sicher
        df_plot <- st_drop_geometry(df) %>%
          mutate(lon = coords[,1], lat = coords[,2])
      }
      
      # Farben und Legende je Modus
      if (isTRUE(input$farb_modus == "baumart")) {
        baumarten <- sort(unique(df_plot$Baumart))
        farben    <- setNames(baumart_farben_basis[seq_along(baumarten)], baumarten)
        color_aes <- "Baumart"
        legend_name <- "Baumart"
      } else {
        kat_order   <- names(kat_palette)
        kat_present <- kat_order[kat_order %in% unique(df_plot$Kat)]
        farben      <- kat_palette[kat_present]
        color_aes   <- "Kat"
        legend_name <- "Empfehlung"
      }
      
      baumart_str  <- paste(sort(input$baumart_sel), collapse = ", ")
      tv_str       <- paste(names(tv_bezeichnung)[tv_bezeichnung %in% input$tv_sel],
                            collapse = ", ")
      stufe_label  <- c(BAE_3ST = "3-stufig", BAE_4ST = "4-stufig",
                        BAE_5ST = "5-stufig", BAE_7ST = "7-stufig")[input$stufe]
      
      subtitle_txt <- paste0(input$szenario, "  |  ", input$modell, "  |  ",
                             input$zeitraum, "  |  ", stufe_label %||% input$stufe,
                             "  \u2013  Teilvorhaben: ", tv_str)
      caption_txt  <- paste0("Baumart: ", baumart_str, "   \u2022   N = ",
                             fmt_n(nrow(df_plot)), " Punkte")
      
      incProgress(0.3, detail = "Rendere Plot...")
      
      is_nr      <- isTRUE(input$datenquelle == "NR")
      nr_label   <- if (is_nr) paste0("  |  ", input$nr_sel) else ""
      dq_label   <- if (is_nr) input$nr_sel else "BWI-BZE"

      # Kartenausschnitt: BWI bundesweit (feste Deutschland-Grenzen); NR auf die
      # Region zoomen (Bounding-Box + Rand), sonst verschwindet die NR als
      # winziger Fleck auf der Deutschlandkarte. DE_GRENZE bleibt Hintergrund.
      if (is_nr) {
        bb   <- sf::st_bbox(sf::st_transform(df, 4326))
        padx <- max(as.numeric(bb["xmax"] - bb["xmin"]) * 0.1, 0.05)
        pady <- max(as.numeric(bb["ymax"] - bb["ymin"]) * 0.1, 0.05)
        karte_xlim <- as.numeric(c(bb["xmin"] - padx, bb["xmax"] + padx))
        karte_ylim <- as.numeric(c(bb["ymin"] - pady, bb["ymax"] + pady))
      } else {
        karte_xlim <- c(5.7, 15.2)
        karte_ylim <- c(47.1, 55.2)
      }

      p <- ggplot() +
        geom_sf(data = DE_GRENZE, fill = "#f4f4f2",
                color = "#aaaaaa", linewidth = 0.35) +
        {
          if (is_nr) {
            geom_sf(data = df, aes(fill = .data[[color_aes]]),
                    color = "#555555", linewidth = 0.3, alpha = 0.80)
          } else {
            geom_point(data = df_plot,
                       aes(x = lon, y = lat, color = .data[[color_aes]]),
                       size = (input$punktgroesse %||% 4) * 0.55,
                       alpha = 0.80, shape = 16)
          }
        } +
        {
          if (is_nr) {
            scale_fill_manual(name = legend_name, values = farben,
                              breaks = names(farben),
                              guide = guide_legend(
                                override.aes = list(alpha = 1), ncol = 1))
          } else {
            scale_color_manual(name = legend_name, values = farben,
                               breaks = names(farben),
                               guide = guide_legend(
                                 override.aes = list(size = 3.5, alpha = 1), ncol = 1))
          }
        } +
        coord_sf(xlim = karte_xlim, ylim = karte_ylim, expand = FALSE) +
        labs(title    = "Baumartenempfehlung MRS",
             subtitle = paste0(subtitle_txt, nr_label),
             caption  = paste0(caption_txt, "  \u2022  ", dq_label),
             x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 3)),
          plot.subtitle    = element_text(size = 9.5, color = "#444444", margin = margin(b = 8)),
          plot.caption     = element_text(size = 8, color = "#888888", hjust = 0, margin = margin(t = 6)),
          legend.position  = "right",
          legend.title     = element_text(face = "bold", size = 9),
          legend.text      = element_text(size = 8.5),
          legend.key.size  = unit(0.45, "cm"),
          legend.background = element_rect(fill = "white", color = "#dddddd", linewidth = 0.3),
          legend.margin    = margin(6, 8, 6, 8),
          panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.2),
          panel.grid.minor = element_blank(),
          axis.text        = element_text(size = 7, color = "#aaaaaa"),
          plot.background  = element_rect(fill = "white", color = NA),
          plot.margin      = margin(10, 10, 8, 10)
        )
      
      incProgress(0.4, detail = "Speichern...")
      ggplot2::ggsave(filename = file, plot = p,
                      width = 28, height = 24, units = "cm", dpi = 300)
    })
    }
  )

  ## ---- 2.16b Schleifen-Export (Batch-PNG ins Ergebnisverzeichnis) ----
  # "Als Schleife abspeichern": rendert je gewaehlter Baumart x Stufe eine Karte
  # (gleicher Look wie output$save_png, aber ohne Browser-Download) und legt sie
  # unter 04_results/BAE_Auswertung/maps/<TV>/ ab. Die sichtbare Karte bleibt
  # unangetastet - geladen wird unabhaengig von input$run_karte / input$baumart_sel.
  observeEvent(input$open_schleife, {
    tv_label <- names(tv_bezeichnung)[tv_bezeichnung == input$tv_sel]
    if (length(tv_label) == 0) tv_label <- paste0("TV", input$tv_sel)
    showModal(modalDialog(
      title = "Karten als Schleife speichern", size = "m", easyClose = TRUE,
      tags$p(tags$b("Teilvorhaben: "), tv_label, tags$br(),
             tags$b("Datenquelle: "),
             if (isTRUE(input$datenquelle == "NR")) input$nr_sel else "BWI-BZE"),
      radioButtons("schleife_ba_modus", "Baumart:",
                   choices = c("Alle Baumarten" = "alle", "Auswahl" = "auswahl"),
                   selected = "alle", inline = TRUE),
      conditionalPanel(
        condition = "input.schleife_ba_modus == 'auswahl'",
        selectizeInput("schleife_ba", label = NULL,
                       choices = baumart_choices, selected = input$baumart_sel,
                       multiple = TRUE,
                       options = list(placeholder = "Baumart(en) wählen...",
                                      plugins = list("remove_button")),
                       width = "100%")
      ),
      radioButtons("schleife_stufe", "Stufe:",
                   choices = c("Alle" = "alle", "3-stufig" = "BAE_3ST",
                               "4-stufig" = "BAE_4ST", "5-stufig" = "BAE_5ST"),
                   selected = "alle", inline = TRUE),
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("run_schleife", "Schleife durchführen",
                     icon = icon("play"), class = "btn-primary")
      )
    ))
  })

  observeEvent(input$run_schleife, {
    removeModal()
    if (is.null(input$szenario) || is.null(input$modell) ||
        is.null(input$zeitraum) || is.null(input$tv_sel)) {
      showNotification("Bitte zuerst Klimalauf und TV wählen.", type = "error")
      return(invisible())
    }
    is_nr  <- isTRUE(input$datenquelle == "NR")
    ba_set <- if (isTRUE(input$schleife_ba_modus == "auswahl"))
                input$schleife_ba else baumart_choices
    if (length(ba_set) == 0) {
      showNotification("Keine Baumart gewählt.", type = "error")
      return(invisible())
    }

    tryCatch({
      ## ---- Daten laden (einmalig, alle gewaehlten Baumarten) ----
      ## Parallel zur NR-/BWI-Ladelogik aus filtered_raw(), aber unabhaengig von
      ## input$baumart_sel, damit "Alle Baumarten" auch ohne Sidebar-Auswahl geht.
      if (is_nr) {
        if (is.null(BAE_WM_DIR) || !dir.exists(BAE_WM_DIR))
          stop("NR-Pfad nicht konfiguriert (BAE_WM_DIR).")
        if (is.null(NR_GEO_ALL)) stop("NR-Geodaten nicht gefunden.")
        if (is.null(input$nr_sel)) stop("Keine Nachbarschaftsregion gewählt.")
        tv_pad  <- sprintf("%02d", as.integer(input$tv_sel))
        tv_dirs <- list.dirs(BAE_WM_DIR, recursive = FALSE, full.names = TRUE)
        tv_dirs <- tv_dirs[grepl(paste0("^BAE_", tv_pad, "_"),
                                 basename(tv_dirs), ignore.case = TRUE)]
        if (length(tv_dirs) == 0) stop("Kein BAE-Ordner für TV", input$tv_sel, ".")
        nr_dirs <- unlist(lapply(tv_dirs, function(d) {
          sub <- list.dirs(d, recursive = FALSE, full.names = TRUE)
          sub[grepl(paste0("^", input$nr_sel, "$"), basename(sub), ignore.case = TRUE)]
        }))
        if (length(nr_dirs) == 0)
          stop("Kein Ordner ", input$nr_sel, " unter den TV", input$tv_sel, "-Ordnern.")
        leaf_dirs <- file.path(nr_dirs, input$szenario, input$modell, input$zeitraum)
        leaf_dirs <- leaf_dirs[dir.exists(leaf_dirs)]
        alle_csv  <- if (length(leaf_dirs) > 0)
          list.files(leaf_dirs, pattern = "\\.csv$", full.names = TRUE) else character(0)
        if (length(alle_csv) == 0)
          alle_csv <- list.files(nr_dirs, pattern = "\\.csv$",
                                 recursive = TRUE, full.names = TRUE)
        ba_pat <- paste0("_(", paste(ba_set, collapse = "|"), ")\\.csv$")
        bn     <- basename(alle_csv)
        match_files <- unique(alle_csv[
          grepl(paste0("_", input$nr_sel,   "_"), bn, ignore.case = TRUE) &
            grepl(paste0("_", input$szenario, "_"), bn) &
            grepl(paste0("_", input$modell,   "_"), bn) &
            grepl(paste0("_", input$zeitraum, "_"), bn) &
            grepl(ba_pat, bn)
        ])
        if (length(match_files) == 0) stop("Keine NR-CSV für die Auswahl gefunden.")
        df_raw <- data.table::rbindlist(lapply(match_files, function(f) {
          dt <- data.table::fread(f, fill = TRUE)
          if ("MASTER_ID" %in% names(dt))
            data.table::set(dt, j = "MASTER_ID", value = as.character(dt$MASTER_ID))
          bae_cols <- grep("^BAE_", names(dt), ignore.case = TRUE, value = TRUE)
          for (col in bae_cols)
            data.table::set(dt, j = col, value = as.character(dt[[col]]))
          dt$Baumart <- sub("\\.csv$", "", sub(".*_", "", basename(f)))
          dt
        }), fill = TRUE)
      } else {
        df_raw <- csv_raw()
      }

      df <- df_raw %>% rename_with(toupper)
      if ("BAUMART" %in% names(df) && !is_nr)
        df <- df %>% filter(BAUMART %in% ba_set)
      if ("TV" %in% names(df))
        df <- df %>% filter(TV %in% as.integer(input$tv_sel))
      if ("BAUMART" %in% names(df)) {
        df <- df %>% rename(Baumart = BAUMART)
      } else if (!"Baumart" %in% names(df)) {
        df$Baumart <- NA_character_
      }
      if (nrow(df) == 0) stop("Keine Daten für die gewählte Kombination.")
      df <- df %>% distinct(MASTER_ID, Baumart, .keep_all = TRUE)

      geo    <- geo_daten()
      joined <- geo %>% left_join(df, by = "MASTER_ID") %>% filter(!is.na(Baumart))
      if (nrow(joined) == 0) stop("Join ohne Treffer (MASTER_ID prüfen).")

      ## ---- Stufen / Baumarten / Zielordner bestimmen ----
      verfuegbar <- intersect(c("BAE_3ST", "BAE_4ST", "BAE_5ST", "BAE_7ST"),
                              toupper(names(joined)))
      stufen <- if (isTRUE(input$schleife_stufe == "alle")) verfuegbar
                else intersect(input$schleife_stufe, verfuegbar)
      if (length(stufen) == 0) stop("Gewählte Stufe in den Daten nicht vorhanden.")
      ba_loop <- intersect(ba_set, sort(unique(joined$Baumart)))
      if (length(ba_loop) == 0) stop("Keine der gewählten Baumarten in den Daten.")

      tv_label  <- names(tv_bezeichnung)[tv_bezeichnung == input$tv_sel]
      if (length(tv_label) == 0) tv_label <- paste0("TV", input$tv_sel)
      tv_folder <- gsub("/", "-", gsub("[: ]+", "_", tv_label))   # "TV2: LFOA-MV" -> "TV2_LFOA-MV"
      out_dir   <- file.path(result_dir, "BAE_Auswertung", "maps", tv_folder)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      region_tag   <- if (is_nr) input$nr_sel else "BWI"
      stufe_labels <- c(BAE_3ST = "3-stufig", BAE_4ST = "4-stufig",
                        BAE_5ST = "5-stufig", BAE_7ST = "7-stufig")

      ## ---- Render-Schleife: je Baumart x Stufe eine Karte ----
      kombis  <- expand.grid(ba = ba_loop, stufe = stufen, stringsAsFactors = FALSE)
      n_total <- nrow(kombis); n_ok <- 0; erzeugt <- character(0)

      withProgress(message = "Schleife: Karten rendern...", value = 0, {
        for (i in seq_len(n_total)) {
          ba    <- kombis$ba[i]
          stufe <- kombis$stufe[i]
          incProgress(1 / n_total,
                      detail = paste0(ba, " / ", stufe_labels[stufe],
                                      "  (", i, "/", n_total, ")"))

          df_ba <- joined %>% filter(Baumart == ba)
          if (nrow(df_ba) == 0) next
          df_ba <- df_ba %>%
            mutate(Kat   = map_stufe(.data[[stufe]], stufe),
                   Farbe = dplyr::coalesce(unname(kat_palette[Kat]), "#B0B0B0"))

          kat_order   <- names(kat_palette)
          farben      <- kat_palette[kat_order[kat_order %in% unique(df_ba$Kat)]]

          if (is_nr) {
            df_plot <- sf::st_drop_geometry(df_ba)
            bb   <- sf::st_bbox(sf::st_transform(df_ba, 4326))
            padx <- max(as.numeric(bb["xmax"] - bb["xmin"]) * 0.1, 0.05)
            pady <- max(as.numeric(bb["ymax"] - bb["ymin"]) * 0.1, 0.05)
            karte_xlim <- as.numeric(c(bb["xmin"] - padx, bb["xmax"] + padx))
            karte_ylim <- as.numeric(c(bb["ymin"] - pady, bb["ymax"] + pady))
          } else {
            coords  <- sf::st_coordinates(df_ba)
            df_plot <- sf::st_drop_geometry(df_ba) %>%
              mutate(lon = coords[, 1], lat = coords[, 2])
            karte_xlim <- c(5.7, 15.2); karte_ylim <- c(47.1, 55.2)
          }

          subtitle_txt <- paste0(input$szenario, "  |  ", input$modell, "  |  ",
                                 input$zeitraum, "  |  ", stufe_labels[stufe],
                                 "  –  Teilvorhaben: ", tv_label,
                                 if (is_nr) paste0("  |  ", input$nr_sel) else "")
          caption_txt  <- paste0("Baumart: ", ba, "   •   N = ",
                                 fmt_n(nrow(df_plot)), " Punkte   •  ",
                                 if (is_nr) input$nr_sel else "BWI-BZE")

          # EIN durchgehender ggplot-Aufruf (parallel zu output$save_png)
          p <- ggplot() +
            geom_sf(data = DE_GRENZE, fill = "#f4f4f2",
                    color = "#aaaaaa", linewidth = 0.35) +
            {
              if (is_nr)
                geom_sf(data = df_ba, aes(fill = Kat),
                        color = "#555555", linewidth = 0.3, alpha = 0.80)
              else
                geom_point(data = df_plot, aes(x = lon, y = lat, color = Kat),
                           size = (input$punktgroesse %||% 4) * 0.55,
                           alpha = 0.80, shape = 16)
            } +
            {
              if (is_nr)
                scale_fill_manual(name = "Empfehlung", values = farben,
                                  breaks = names(farben),
                                  guide = guide_legend(override.aes = list(alpha = 1), ncol = 1))
              else
                scale_color_manual(name = "Empfehlung", values = farben,
                                   breaks = names(farben),
                                   guide = guide_legend(override.aes = list(size = 3.5, alpha = 1), ncol = 1))
            } +
            coord_sf(xlim = karte_xlim, ylim = karte_ylim, expand = FALSE) +
            labs(title = "Baumartenempfehlung MRS", subtitle = subtitle_txt,
                 caption = caption_txt, x = NULL, y = NULL) +
            theme_minimal(base_size = 11) +
            theme(
              plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 3)),
              plot.subtitle    = element_text(size = 9.5, color = "#444444", margin = margin(b = 8)),
              plot.caption     = element_text(size = 8, color = "#888888", hjust = 0, margin = margin(t = 6)),
              legend.position  = "right",
              legend.title     = element_text(face = "bold", size = 9),
              legend.text      = element_text(size = 8.5),
              legend.key.size  = unit(0.45, "cm"),
              legend.background = element_rect(fill = "white", color = "#dddddd", linewidth = 0.3),
              legend.margin    = margin(6, 8, 6, 8),
              panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.2),
              panel.grid.minor = element_blank(),
              axis.text        = element_text(size = 7, color = "#aaaaaa"),
              plot.background  = element_rect(fill = "white", color = NA),
              plot.margin      = margin(10, 10, 8, 10)
            )

          fname <- paste0("BAE_", ba, "_", input$szenario, "_", input$modell, "_",
                          input$zeitraum, "_", tolower(sub("^BAE_", "", stufe)),
                          "_", region_tag, ".png")
          ggplot2::ggsave(filename = file.path(out_dir, fname), plot = p,
                          width = 28, height = 24, units = "cm", dpi = 300)
          n_ok    <- n_ok + 1
          erzeugt <- c(erzeugt, fname)
        }
      })

      message("Schleifen-Export: ", n_ok, "/", n_total, " Karten -> ", out_dir)
      showModal(modalDialog(
        title = "Schleife abgeschlossen", easyClose = TRUE,
        tags$p(tags$b(n_ok), " von ", n_total, " Karten gespeichert unter:"),
        tags$pre(style = "white-space:pre-wrap;", out_dir),
        if (length(erzeugt) > 0)
          tags$details(tags$summary("Dateien anzeigen"),
                       tags$pre(style = "max-height:220px; overflow:auto;",
                                paste(erzeugt, collapse = "\n"))),
        footer = modalButton("Schließen")
      ))
    }, error = function(e) {
      showNotification(paste0("Schleife fehlgeschlagen: ", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  })

  ## ---- 2.17 Analyse-Tab (Phase 2) ----
  
  # Gemeinsame Hilfsfunktion: Kreuztabelle aus gefilterten Daten
  analyse_data <- reactive({
    df <- tryCatch(filtered(), error = function(e) NULL)
    req(!is.null(df) && nrow(df) > 0)
    
    kat_order <- names(kat_palette)
    
    df_plain <- sf::st_drop_geometry(df) %>%
      mutate(Kat = factor(Kat, levels = kat_order))
    
    # Kreuztabelle N
    kreuztab_n <- df_plain %>%
      count(Baumart, Kat, .drop = FALSE) %>%
      tidyr::pivot_wider(names_from = Kat, values_from = n,
                         values_fill = 0)
    
    # Kreuztabelle % (zeilenweise, je Baumart)
    kreuztab_pct <- df_plain %>%
      count(Baumart, Kat, .drop = FALSE) %>%
      group_by(Baumart) %>%
      mutate(Pct = round(n / sum(n) * 100, 1)) %>%
      ungroup() %>%
      dplyr::select(-n) %>%
      tidyr::pivot_wider(names_from = Kat, values_from = Pct,
                         values_fill = 0)
    
    # Balken-Daten (long, mit %)
    balken_df <- df_plain %>%
      count(Baumart, Kat, .drop = FALSE) %>%
      group_by(Baumart) %>%
      mutate(Pct = round(n / sum(n) * 100, 1)) %>%
      ungroup() %>%
      filter(n > 0)
    
    # Deskriptive Statistik: BAE-Wert numerisch je Baumart/TV
    # Fallback: wenn gewählte Stufe nicht in df_plain, höchste verfügbare nehmen
    bae_col <- if (!is.null(input$stufe) && input$stufe %in% names(df_plain)) {
      input$stufe
    } else {
      found <- intersect(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST"), names(df_plain))
      if (length(found) > 0) found[length(found)] else NULL
    }
    stats_df <- if (!is.null(bae_col)) {
      tryCatch(
        df_plain %>%
          mutate(BAE_num = suppressWarnings(as.integer(.data[[bae_col]]))) %>%
          filter(!is.na(BAE_num)) %>%
          group_by(Baumart, TV) %>%
          summarise(
            N      = n(),
            Min    = min(BAE_num),
            Median = median(BAE_num),
            Mean   = round(mean(BAE_num), 2),
            Max    = max(BAE_num),
            SD     = round(sd(BAE_num), 2),
            .groups = "drop"
          ),
        error = function(e) data.frame(Info = conditionMessage(e))
      )
    } else {
      data.frame(Info = "Keine BAE-Spalte verf\u00fcgbar")
    }
    
    list(kreuztab_n   = kreuztab_n,
         kreuztab_pct = kreuztab_pct,
         balken_df    = balken_df,
         stats_df     = stats_df,
         n_total      = nrow(df_plain))
  })
  
  ### ---- 2.17.1 Analyse-Header ----
  output$analyse_header <- renderUI({
    df <- tryCatch(filtered(), error = function(e) NULL)
    if (is.null(df)) return(tags$p(style = "color:#999;",
                                   "Bitte Filter w\u00e4hlen und Karte laden."))
    
    tv_str <- paste(names(tv_bezeichnung)[tv_bezeichnung %in% input$tv_sel],
                    collapse = ", ")
    tags$div(style = "margin-top:10px;",
             tags$span(style = "font-size:15px; font-weight:bold;",
                       "Analyse: Baumartenempfehlungen MRS"),
             tags$br(),
             tags$span(style = "font-size:12px; color:#555;",
                       paste0(input$szenario, "  |  ", input$modell, "  |  ",
                              input$zeitraum, "  |  ", input$stufe,
                              "  \u2013  ", tv_str,
                              "  \u2022  N = ",
                              fmt_n(nrow(df)), " Punkte"))
    )
  })
  
  ### ---- 2.17.2 Kennzahlen-Boxen ----
  output$kennzahlen_boxes <- renderUI({
    ad <- tryCatch(analyse_data(), error = function(e) NULL)
    req(ad)
    
    df <- tryCatch(sf::st_drop_geometry(filtered()), error = function(e) NULL)
    req(df)
    
    kat_order <- names(kat_palette)[1:2]   # sehr + empfohlen
    
    n_positiv <- df %>%
      filter(Kat %in% c("sehr empfohlen", "empfohlen")) %>% nrow()
    pct_pos   <- round(n_positiv / ad$n_total * 100, 1)
    
    n_neg     <- df %>% filter(Kat == "nicht empfohlen") %>% nrow()
    pct_neg   <- round(n_neg / ad$n_total * 100, 1)
    
    kbox <- function(label, val, col) {
      tags$div(style = paste0(
        "background:", col, "22; border-left:3px solid ", col, ";",
        "border-radius:4px; padding:6px 10px; margin-bottom:6px;",
        "font-size:12px;"),
        tags$div(style = "font-size:18px; font-weight:bold; color:",
                 col, ";", val),
        tags$div(style = "color:#555;", label)
      )
    }
    
    tagList(
      kbox(paste0("empfohlen / sehr empfohlen  (", pct_pos, "%)"),
           fmt_n(n_positiv), "#1A9850"),
      kbox(paste0("nicht empfohlen  (", pct_neg, "%)"),
           fmt_n(n_neg), "#A50026"),
      kbox("Punkte gesamt",
           fmt_n(ad$n_total), "#2E7D32")
    )
  })
  
  ### ---- 2.17.3 Balkendiagramm ----
  output$analyse_balken <- renderPlot({
    ad <- tryCatch(analyse_data(), error = function(e) NULL)
    req(ad)
    
    farben_plot <- kat_palette[names(kat_palette) %in% ad$balken_df$Kat]
    
    ggplot(ad$balken_df,
           aes(x = Baumart, y = Pct,
               fill = factor(Kat, levels = names(kat_palette)))) +
      geom_col(position = "stack", width = 0.7) +
      geom_text(aes(label = ifelse(Pct >= 4,
                                   paste0(Pct, "%"), "")),
                position = position_stack(vjust = 0.5),
                size = 3, color = "white", fontface = "bold") +
      scale_fill_manual(values = farben_plot,
                        name   = "Kategorie",
                        drop   = TRUE) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         limits = c(0, 102), expand = c(0, 0)) +
      labs(x = NULL, y = "Anteil (%)",
           title = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position   = "bottom",
        legend.text       = element_text(size = 8),
        legend.key.size   = unit(0.35, "cm"),
        legend.title      = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(face = "bold", size = 10),
        plot.background   = element_rect(fill = "white", color = NA)
      ) +
      guides(fill = guide_legend(nrow = 2))
  }, bg = "white")
  
  ### ---- 2.17.4 Kreuztabelle ----
  output$analyse_kreuztab <- DT::renderDataTable({
    ad <- tryCatch(analyse_data(), error = function(e) NULL)
    req(ad)
    
    n_df   <- ad$kreuztab_n
    pct_df <- ad$kreuztab_pct
    kat_cols <- setdiff(names(n_df), "Baumart")
    
    combined <- n_df
    for (k in kat_cols) {
      if (k %in% names(pct_df))
        combined[[k]] <- paste0(n_df[[k]], " (", pct_df[[k]], "%)")
    }
    combined[combined == "0 (0%)"] <- "\u2013"
    
    # Spaltennamen explizit in UTF-8 konvertieren
    names(combined) <- enc2utf8(names(combined))
    
    DT::datatable(
      combined,
      rownames = FALSE,
      options  = list(pageLength = 15, dom = "t",
                      columnDefs = list(list(className = "dt-center",
                                             targets = seq_len(ncol(combined)) - 1))),
      class = "stripe hover compact"
    ) %>%
      DT::formatStyle("Baumart", fontWeight = "bold")
  })
  
  ### ---- 2.17.5 Deskriptive Statistik ----
  output$analyse_stats <- DT::renderDataTable({
    ad <- tryCatch(analyse_data(), error = function(e) NULL)
    req(ad)
    
    DT::datatable(
      ad$stats_df,
      rownames = FALSE,
      options  = list(pageLength = 15, dom = "tip",
                      scrollX = TRUE),
      class    = "stripe hover compact"
    ) %>%
      DT::formatStyle(c("Baumart", "TV"), fontWeight = "bold") %>%
      DT::formatStyle("Mean",
                      background = DT::styleColorBar(
                        range(ad$stats_df$Mean, na.rm = TRUE), "#c8e6c9"))
  })
  
  ### ---- 2.17.6 Downloads ----
  output$download_kreuztab <- downloadHandler(
    filename = function() {
      paste0("Kreuztabelle_", input$szenario, "_", input$modell, "_",
             input$zeitraum, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) data.table::fwrite(analyse_data()$kreuztab_n, 
                                                file, sep = ";", dec = ",", bom = TRUE)
    
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0("Statistik_", input$szenario, "_", input$modell, "_",
             input$zeitraum, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) data.table::fwrite(analyse_data()$stats_df,
                                                file, sep = ";", dec = ",", bom = TRUE)
  )
  
  ## ---- 2.18 Vergleich-Tab / Differenz-Layer (Phase 5) ----
  
  # Kaskadierung für Lauf A
  observeEvent(input$vgl_sz_a, {
    mod <- klima_meta %>% filter(Szenario == input$vgl_sz_a) %>%
      pull(Modell) %>% unique() %>% sort()
    updateSelectInput(session, "vgl_mod_a", choices = mod, selected = mod[1])
  }, ignoreNULL = TRUE)
  
  observeEvent(input$vgl_mod_a, {
    req(input$vgl_sz_a)
    zr <- klima_meta %>%
      filter(Szenario == input$vgl_sz_a, Modell == input$vgl_mod_a) %>%
      pull(Zeitraum) %>% unique() %>% sort()
    updateSelectInput(session, "vgl_zr_a", choices = zr, selected = zr[1])
  }, ignoreNULL = TRUE)
  
  # Kaskadierung für Lauf B
  observeEvent(input$vgl_sz_b, {
    mod <- klima_meta %>% filter(Szenario == input$vgl_sz_b) %>%
      pull(Modell) %>% unique() %>% sort()
    updateSelectInput(session, "vgl_mod_b", choices = mod, selected = mod[1])
  }, ignoreNULL = TRUE)
  
  observeEvent(input$vgl_mod_b, {
    req(input$vgl_sz_b)
    zr <- klima_meta %>%
      filter(Szenario == input$vgl_sz_b, Modell == input$vgl_mod_b) %>%
      pull(Zeitraum) %>% unique() %>% sort()
    updateSelectInput(session, "vgl_zr_b", choices = zr, selected = zr[1])
  }, ignoreNULL = TRUE)
  
  # Differenz-Berechnung (nur bei Button-Klick)
  vgl_result <- eventReactive(input$vgl_run, {
    req(input$vgl_sz_a, input$vgl_mod_a, input$vgl_zr_a,
        input$vgl_sz_b, input$vgl_mod_b, input$vgl_zr_b,
        input$vgl_baumart, input$vgl_tv, input$vgl_stufe)
    
    # CSV A laden
    meta_a <- klima_meta %>%
      filter(Szenario == input$vgl_sz_a, Modell == input$vgl_mod_a,
             Zeitraum == input$vgl_zr_a)
    validate(need(nrow(meta_a) == 1, "Lauf A: keine eindeutige CSV gefunden."))
    
    # CSV B laden
    meta_b <- klima_meta %>%
      filter(Szenario == input$vgl_sz_b, Modell == input$vgl_mod_b,
             Zeitraum == input$vgl_zr_b)
    validate(need(nrow(meta_b) == 1, "Lauf B: keine eindeutige CSV gefunden."))
    
    bae_col <- input$vgl_stufe
    
    lese_csv <- function(meta, baumarten, tvs) {
      df <- data.table::fread(meta$file) %>%
        rename_with(toupper) %>%
        filter(BAUMART %in% baumarten, TV %in% as.integer(tvs)) %>%
        rename(Baumart = BAUMART)
      validate(need(bae_col %in% names(df),
                    paste0("Spalte '", bae_col, "' nicht in CSV.")))
      df %>%
        dplyr::select(MASTER_ID, Baumart, TV,
                      Kat_num = !!dplyr::sym(bae_col)) %>%
        mutate(Kat_num = suppressWarnings(as.integer(Kat_num)))
    }
    
    df_a <- lese_csv(meta_a, input$vgl_baumart, input$vgl_tv)
    df_b <- lese_csv(meta_b, input$vgl_baumart, input$vgl_tv)
    
    # Join und Differenz
    diff_df <- inner_join(
      df_a %>% rename(Kat_A = Kat_num),
      df_b %>% rename(Kat_B = Kat_num),
      by = c("MASTER_ID", "Baumart", "TV")
    ) %>%
      filter(!is.na(Kat_A), !is.na(Kat_B)) %>%
      mutate(
        Delta    = Kat_B - Kat_A,
        Richtung = dplyr::case_when(
          Delta < 0 ~ "verbessert",
          Delta > 0 ~ "verschlechtert",
          TRUE      ~ "unver\u00e4ndert"
        ),
        # Kategorienamen fuer Anzeige
        Kat_A_label = map_stufe(Kat_A, bae_col),
        Kat_B_label = map_stufe(Kat_B, bae_col),
        Farbe = dplyr::case_when(
          Richtung == "verbessert"      ~ "#1A9850",
          Richtung == "verschlechtert"  ~ "#A50026",
          TRUE                           ~ "#BBBBBB"
        )
      )
    
    validate(need(nrow(diff_df) > 0, "Keine gemeinsamen MASTER_IDs gefunden."))
    
    # Geodaten joinen
    geo_joined <- BWI_GEO %>%
      inner_join(diff_df, by = "MASTER_ID") %>%
      { if (isTRUE(input$vgl_nur_aenderung))
        filter(., Richtung != "unver\u00e4ndert") else . }
    
    validate(need(nrow(geo_joined) > 0,
                  "Keine ver\u00e4nderten Punkte im gew\u00e4hlten Filter."))
    
    list(
      geo  = geo_joined,
      df   = sf::st_drop_geometry(geo_joined),
      label_a = paste(input$vgl_sz_a, input$vgl_mod_a, input$vgl_zr_a, sep = " / "),
      label_b = paste(input$vgl_sz_b, input$vgl_mod_b, input$vgl_zr_b, sep = " / ")
    )
  })
  
  # Basiskarte Vergleich
  output$map_vgl <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  # Karte-Reset Vergleich
  observeEvent(input$map_vgl_reset, {
    leafletProxy("map_vgl") %>% setView(lng = 10.5, lat = 51.2, zoom = 6)
  })
  
  # Marker auf Vergleichskarte rendern
  observeEvent(vgl_result(), {
    res    <- vgl_result()
    df     <- res$geo
    coords <- sf::st_coordinates(df)
    
    popups <- paste0(
      "<b>MASTER_ID:</b> ", df$MASTER_ID,   "<br>",
      "<b>Baumart:</b> ",   df$Baumart,     "<br>",
      "<b>TV:</b> ",        df$TV,          "<br>",
      "<b>Kat A (", res$label_a, "):</b> ", df$Kat_A_label, "<br>",
      "<b>Kat B (", res$label_b, "):</b> ", df$Kat_B_label, "<br>",
      "<b>Delta:</b> ",     df$Delta,       "<br>",
      "<b>Richtung:</b> ",  df$Richtung
    )
    
    diff_farben <- c(
      "verbessert"     = "#1A9850",
      "verschlechtert" = "#A50026",
      "unver\u00e4ndert"  = "#BBBBBB"
    )
    
    leafletProxy("map_vgl") %>%
      clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        lng = coords[, 1], lat = coords[, 2],
        color       = df$Farbe,
        fillColor   = df$Farbe,
        fillOpacity = 0.85,
        radius      = 4,
        stroke      = FALSE,
        popup       = popups,
        group       = df$Richtung
      ) %>%
      addLayersControl(
        overlayGroups = names(diff_farben)[names(diff_farben) %in% unique(df$Richtung)],
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = unname(diff_farben),
        labels   = names(diff_farben),
        title    = "Ver\u00e4nderung",
        opacity  = 0.9
      )
  })
  
  # Zusammenfassungs-Box
  output$vgl_summary_box <- renderUI({
    res <- tryCatch(vgl_result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    
    df <- res$df
    n_ges   <- nrow(df)
    n_verb  <- sum(df$Richtung == "verbessert")
    n_versc <- sum(df$Richtung == "verschlechtert")
    n_unv   <- sum(df$Richtung == "unver\u00e4ndert")
    pct_v   <- round(n_verb  / n_ges * 100, 1)
    pct_s   <- round(n_versc / n_ges * 100, 1)
    
    kbox <- function(val, label, col) {
      tags$div(style = paste0(
        "background:", col, "15; border-left:3px solid ", col, ";",
        "border-radius:4px; padding:5px 9px; margin-bottom:5px; font-size:11px;"),
        tags$div(style = paste0("font-size:17px; font-weight:bold; color:", col, ";"), val),
        tags$div(style = "color:#555;", label)
      )
    }
    
    tags$div(
      style = "margin-top:4px;",
      kbox(paste0(fmt_n(n_verb), "  (", pct_v, "%)"),
           "verbessert",      "#1A9850"),
      kbox(paste0(fmt_n(n_versc), "  (", pct_s, "%)"),
           "verschlechtert",  "#A50026"),
      kbox(fmt_n(n_unv),
           "unver\u00e4ndert", "#888888"),
      tags$div(style = "font-size:10px; color:#999; margin-top:3px;",
               paste0("Gesamt: ", fmt_n(n_ges), " Punkte"))
    )
  })
  
  # Differenztabelle
  output$vgl_tabelle <- DT::renderDataTable({
    res <- tryCatch(vgl_result(), error = function(e) NULL)
    req(res)
    
    tab <- res$df %>%
      dplyr::select(MASTER_ID, Baumart, TV, Richtung,
                    Kat_A = Kat_A_label, Kat_B = Kat_B_label, Delta) %>%
      dplyr::arrange(dplyr::desc(abs(Delta)), Richtung)
    
    DT::datatable(
      tab,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 15, scrollX = TRUE,
                      dom = "tip"),
      class    = "stripe hover compact"
    ) %>%
      DT::formatStyle(
        "Richtung",
        backgroundColor = DT::styleEqual(
          c("verbessert", "verschlechtert", "unver\u00e4ndert"),
          c("#d4edda",    "#f8d7da",        "#f5f5f5")
        ),
        fontWeight = "bold"
      ) %>%
      DT::formatStyle("Delta",
                      color = DT::styleInterval(c(-0.1, 0.1),
                                                c("#1A9850", "#888888", "#A50026")),
                      fontWeight = "bold")
  })
  
  # Download Differenztabelle
  output$vgl_download <- downloadHandler(
    filename = function() {
      res <- vgl_result()
      paste0("Differenz_",
             gsub(" / ", "_", res$label_a), "_vs_",
             gsub(" / ", "_", res$label_b), "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) data.table::fwrite(vgl_result()$df,
                                                file, sep = ";", dec = ",", bom = TRUE)
  )
  
  ## ---- 2.19 Standortblatt PDF (Phase 6) ----
  
  output$download_standortblatt <- downloadHandler(
    filename = function() {
      sel <- selected_punkt()
      mid <- if (!is.null(sel)) as.character(sel$punkt$MASTER_ID) else "unbekannt"
      paste0("Standortblatt_", mid, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      sel <- selected_punkt()
      if (is.null(sel)) stop("Bitte zuerst einen Punkt anklicken.")
      
      d     <- sel$punkt
      boden <- sel$boden
      wm_df <- sel$wm
      # Sicherheitsnetz: Fehler in generate_standortblatt sauber abfangen
      # (verhindert dass Shiny stumm eine HTML-Fehlerseite als Datei ausliefert)
      
      withProgress(message = "Erstelle Standortblatt...", value = 0.2, {
        
        # BAE-Tabelle: alle Klimaläufe für diese MASTER_ID und Baumart
        incProgress(0.2, detail = "Lade alle Klimaläufe...")
        alle_laeufe <- tryCatch({
          raw_list <- lapply(seq_len(nrow(klima_meta)), function(i) {
            df_i <- tryCatch(data.table::fread(klima_meta$file[i],
                                               data.table = FALSE),
                             error = function(e) NULL)
            if (is.null(df_i)) return(NULL)
            names(df_i) <- toupper(names(df_i))
            row_i <- df_i[df_i$MASTER_ID == as.character(d$MASTER_ID) &
                            df_i$BAUMART  == as.character(d$Baumart), ,
                          drop = FALSE]
            if (nrow(row_i) == 0) return(NULL)
            bae_cols <- intersect(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST"),
                                  names(row_i))
            row_i %>%
              dplyr::select(dplyr::any_of(c("MASTER_ID","BAUMART","TV")),
                            dplyr::all_of(bae_cols)) %>%
              dplyr::mutate(Szenario = klima_meta$Szenario[i],
                            Modell   = klima_meta$Modell[i],
                            Zeitraum = klima_meta$Zeitraum[i])
          })
          raw_list <- Filter(Negate(is.null), raw_list)
          if (length(raw_list) == 0) return(NULL)
          data.table::rbindlist(raw_list, fill = TRUE, use.names = TRUE) %>% as.data.frame()
        }, error = function(e) NULL)
        
        incProgress(0.4, detail = "Rendere PDF...")
        
        tryCatch(
          generate_standortblatt(
            d              = d,
            boden          = boden,
            wm_df          = wm_df,
            alle_laeufe_df = alle_laeufe,
            de_grenze      = DE_GRENZE,
            out_file       = file
          ),
          error = function(e) {
            message("Standortblatt-Fehler: ", e$message)
            stop(e)   # weiterwerfen damit Shiny den Fehler anzeigt
          }
        )
      })
    }
  )
  
  ## ---- 2.20 Klimakarten-Anzeige ----
  # PNG-Karten aus BZT_Klimastufeneinteilung je Klimalauf darstellen
  if (dir.exists(KS_IMG_DIR))
    addResourcePath("ks_imgs", KS_IMG_DIR)
  
  output$ks_bild_ui <- renderUI({
    req(input$szenario, input$modell, input$zeitraum)
    ks_typ <- input$ks_typ %||% "kfr_exp"
    
    if (!dir.exists(KS_IMG_DIR)) {
      return(tags$div(style = "padding:20px; color:#999;",
                      icon("exclamation-circle"),
                      " Klimakarten-Verzeichnis nicht gefunden:",
                      tags$code(KS_IMG_DIR)))
    }
    
    # Dateiname konstruieren nach Konvention:
    # KFR_OR_{Sz}_{Mo}_{Zr}_rf1b.ex_pred_exp[_new].png
    # KS_GL_{Sz}_{Mo}_{Zr}_rf1b.ex_pred_exp[_new].png
    prefix <- if (ks_typ == "ks_gl") "KS_GL" else "KFR_OR"
    suffix <- if (ks_typ == "kfr_new") "_rf1b.ex_pred_exp_new.png" else "_rf1b.ex_pred_exp.png"
    fname  <- paste0(prefix, "_", input$szenario, "_", input$modell,
                     "_", input$zeitraum, suffix)
    fpath  <- file.path(KS_IMG_DIR, fname)
    
    typ_label <- c(kfr_exp = "Klimastufenmodell",
                   kfr_new = "Klimastufe MV (A1b)",
                   ks_gl   = "KS-Gliederung Schlutow")[ks_typ]
    
    if (!file.exists(fpath)) {
      # Alle vorhandenen Dateien fuer dieses Praefix auflisten als Hilfe
      avail <- list.files(KS_IMG_DIR,
                          pattern = paste0("^", prefix, ".*\\.png$"),
                          ignore.case = FALSE)
      return(tags$div(
        style = "padding:20px;",
        tags$div(style = "color:#c0392b; margin-bottom:10px;",
                 icon("exclamation-triangle"),
                 paste0(" Kein Bild f\u00fcr: ", input$szenario, " / ",
                        input$modell, " / ", input$zeitraum)),
        tags$div(style = "font-size:11px; color:#666;",
                 paste0("Gesucht: ", fname)),
        if (length(avail) > 0)
          tags$details(
            tags$summary(style="font-size:11px; color:#888; cursor:pointer;",
                         paste0(length(avail), " verf\u00fcgbare ", prefix, "-Dateien")),
            tags$ul(style = "font-size:10px; color:#aaa; columns:2;",
                    lapply(avail, tags$li))
          )
      ))
    }
    
    # Bild einbetten: Base64 (kein Webserver noetig) oder addResourcePath-URL
    img_data <- if (requireNamespace("base64enc", quietly = TRUE)) {
      tryCatch(base64enc::dataURI(file = fpath, mime = "image/png"),
               error = function(e) paste0("ks_imgs/", fname))
    } else {
      paste0("ks_imgs/", fname)   # Fallback: addResourcePath-URL
    }
    tags$div(
      tags$div(style = "font-size:11px; color:#666; margin-bottom:8px;",
               icon("image"),
               paste0(" ", typ_label, "  \u2013  ",
                      input$szenario, " / ", input$modell, " / ", input$zeitraum)),
      tags$img(src   = img_data,
               style = paste("max-width:100%; border:1px solid #ddd;",
                             "border-radius:4px; box-shadow:0 1px 4px rgba(0,0,0,.1);"),
               alt   = fname)
    )
  })
  
  
  
  ## ---- 2.21 Standortanalyse (Tab 5) ----
  # Funktionen: lade_standort_alle_laeufe(), heatmap_standort_ggplot(),
  #             auswertung_standort_ggplot(), auswertung_grid_ggplot()
  #             -> alle in R/mod_standort.R
  
  
  # Daten laden – reagiert auf Karte (sa_run) UND manuellen Selector.
  # Traegt neben der MASTER_ID auch Region + NR mit, damit NR-Standorte aus
  # den NR-CSVs (BAE_WM_DIR) statt aus den BWI-CSVs geladen werden.
  sa_active_mid <- reactiveVal(NULL)   # list(mid, region, nr_id)
  
  observeEvent(input$sa_run, {
    sel <- selected_punkt()
    if (!is.null(sel)) {
      # Kartenpunkt aktiv -> bisheriges Verhalten unveraendert
      is_nr <- isTRUE(input$datenquelle == "NR")
      sa_active_mid(list(
        mid    = as.character(sel$punkt$MASTER_ID),
        region = if (is_nr) "NR" else "BWI",
        nr_id  = if (is_nr) input$nr_sel else NULL
      ))
    } else {
      # Kein Kartenpunkt -> manuell gewaehlte MASTER_ID verwenden
      req(input$sa_master_id_sel)
      is_nr <- isTRUE(input$sa_region_filter == "NR")
      sa_active_mid(list(
        mid    = input$sa_master_id_sel,
        region = if (is_nr) "NR" else "BWI",
        nr_id  = if (is_nr) (input$sa_nr_filter %||% "NR01") else NULL
      ))
    }
  })
  
  observeEvent(sa_manual_trigger(), {
    req(sa_manual_trigger())
    trg <- sa_manual_trigger()
    sa_active_mid(list(
      mid    = trg$MASTER_ID,
      region = trg$region %||% "BWI",
      nr_id  = trg$nr_id
    ))
  })
  
  sa_data <- reactive({
    sel <- sa_active_mid()
    req(!is.null(sel), nchar(sel$mid) > 0)
    withProgress(message = paste0("Lade alle Klimalaeufe: ", sel$mid, "..."),
                 value = 0, {
                   incProgress(0.1)
                   df <- lade_standort_alle_laeufe(
                     master_id = sel$mid,
                     baumarten = if (length(input$sa_baumart) > 0) input$sa_baumart else NULL,
                     tvs       = if (length(input$sa_tv)      > 0) input$sa_tv      else NULL,
                     modelle   = if (length(input$sa_modell)  > 0) input$sa_modell  else NULL,
                     szenarien = if (length(input$sa_szenario)> 0) input$sa_szenario else NULL,
                     zeitraeume = if (length(input$sa_zeitraum)> 0) input$sa_zeitraum else NULL,
                     region    = sel$region %||% "BWI",
                     nr_id     = sel$nr_id
                   )
                   incProgress(0.9)

                   # RCP45-Varianten (Basis/v2/v3) explizit ein-/ausblenden.
                   # Nicht-RCP45-Laeufe bleiben unberuehrt. Sind alle Varianten
                   # abgewaehlt, werden alle RCP45-Laeufe ausgeblendet.
                   if (length(rcp45_var_choices) > 1 && nrow(df) > 0) {
                     erlaubt  <- input$sa_rcp45_var %||% character(0)
                     is_rcp45 <- startsWith(as.character(df$Szenario), "RCP45")
                     vtag     <- bae_variante(df$Szenario, df$Modell)
                     df <- df[!is_rcp45 | vtag %in% erlaubt, , drop = FALSE]
                   }
                   df
                 })
  })
  
  ### ---- 2.21.1 MASTER_ID-Selector: Choices aktualisieren ----
  master_id_tabelle <- reactive({
    build_master_id_table()
  })
  
  observe({
    tbl <- master_id_tabelle()
    req(nrow(tbl) > 0)
    
    # Nach Region filtern
    sub <- tbl[tbl$Region == input$sa_region_filter, ]
    
    # NR: nach NR-ID filtern (NR hat keine Bundesland-Zuordnung)
    if (input$sa_region_filter == "NR") {
      nr_sel <- input$sa_nr_filter %||% "NR01"
      if ("NR_ID" %in% names(sub)) {
        sub <- sub[!is.na(sub$NR_ID) & sub$NR_ID == nr_sel, ]
      }
    } else {
      # BWI/BZE: nach Bundesland filtern
      bl <- input$sa_bl_filter %||% ""
      if (nchar(bl) > 0)
        sub <- sub[sub$BL_Code == bl, ]
    }
    
    ids <- sort(unique(sub$MASTER_ID))
    updateSelectizeInput(session, "sa_master_id_sel",
                         choices  = ids,
                         selected = if (length(ids) > 0) ids[1] else NULL,
                         server   = TRUE)
  })
  
  ### ---- 2.21.2 "Diesen Standort laden" Button ----
  # Loest dieselbe Analyse aus wie ein Kartenklick
  sa_manual_trigger <- reactiveVal(NULL)
  
  observeEvent(input$sa_load_from_sel, {
    req(input$sa_master_id_sel)
    mid   <- input$sa_master_id_sel
    is_nr <- isTRUE(input$sa_region_filter == "NR")
    sa_manual_trigger(list(
      MASTER_ID = mid,
      region    = if (is_nr) "NR" else "BWI",
      nr_id     = if (is_nr) (input$sa_nr_filter %||% "NR01") else NULL,
      ts        = Sys.time()   # Timestamp damit gleiche ID erneut triggerbar
    ))

    # Bodendaten + Minimal-Punkt analog zum Kartenklick laden, damit
    # Boden-Kontext und Standortblatt-PDF auch ohne Kartenklick funktionieren.
    # (handle_punkt_click() kann hier nicht genutzt werden, da sie auf
    # filtered() der Karte basiert, die unabhaengig von dieser Auswahl ist.)
    boden_region_val <- if (is_nr) "NR" else "BWI"
    if (is.null(boden_cache$data[[mid]])) {
      withProgress(message = paste0("Lade Bodendaten: ", mid), value = 0.5, {
        boden_cache$data[[mid]] <- tryCatch(
          get_boden(mid, region = boden_region_val),
          error = function(e) { message("Bodendaten Fehler: ", e$message); NULL }
        )
      })
    }
    selected_punkt(list(
      punkt = data.frame(MASTER_ID = mid, Baumart = NA_character_,
                         TV = NA_character_, Kat = NA_character_,
                         Szenario = NA_character_, Modell = NA_character_,
                         Zeitraum = NA_character_, stringsAsFactors = FALSE),
      boden = boden_cache$data[[mid]],
      wm    = NULL,
      bzt   = NULL
    ))
  })
  
  # Aktiver Punkt Info
  output$sa_punkt_info <- renderUI({
    sel <- selected_punkt()
    if (is.null(sel)) {
      return(div(class = "info-block",
                 icon("hand-pointer"),
                 " Zuerst Punkt in der Karte anklicken."))
    }
    d <- sel$punkt
    div(class = "sidebar-section",
        tags$p(class = "section-title", "\u25B6 Aktiver Standort"),
        boden_zeile("MASTER_ID", d$MASTER_ID),
        boden_zeile("Baumart",   d$Baumart),
        boden_zeile("TV",        d$TV),
        boden_zeile("Kategorie", d$Kat)
    )
  })
  
  # Boden-Kontext oben im Plot-Bereich
  output$sa_boden_kontext <- renderUI({
    sel <- selected_punkt()
    req(sel)
    b <- sel$boden
    tags$div(style = "display:flex; gap:20px; align-items:center;
                      font-size:12px; color:#555; padding:4px 0;",
             tags$b(style = "color:#2E7D32; font-size:13px;",
                    as.character(sel$punkt$MASTER_ID)),
             if (!is.null(b) && nrow(b) > 0) tagList(
               tags$span(paste0("\u25AA Bodentyp: ",  b$BODTYP[1])),
               tags$span(paste0("\u25AA Wasser: ",    b$WASSER[1])),
               tags$span(paste0("\u25AA N\u00e4hrkraft: ", b$NAEHR[1])),
               tags$span(paste0("\u25AA Bodenart: ",  b$BODART[1]))
             ) else tags$span(style = "color:#999;", "(Bodendaten nicht geladen)")
    )
  })
  
  # Plot rendern
  output$sa_plot <- renderPlot({
    df <- sa_data()
    req(!is.null(df) && nrow(df) > 0)
    
    switch(input$sa_ansicht,
           heatmap         = heatmap_standort_ggplot(df, input$sa_stufe),
           heatmap_zukunft = heatmap_bae_zukunft_function(
                               df, unique(df$MASTER_ID)[1], input$sa_stufe,
                               out_dir = NULL),
           szenario = auswertung_standort_ggplot(df, input$sa_stufe, "Szenario"),
           zeitraum = auswertung_standort_ggplot(df, input$sa_stufe, "Zeitraum"),
           modell   = auswertung_standort_ggplot(df, input$sa_stufe, "Modell"),
           grid     = auswertung_grid_ggplot(df, input$sa_stufe),
           heatmap_standort_ggplot(df, input$sa_stufe)
    )
  }, bg = "white")
  
  # PNG-Download
  output$sa_download_png <- downloadHandler(
    filename = function() {
      sel <- selected_punkt()
      mid <- if (!is.null(sel)) as.character(sel$punkt$MASTER_ID) else "standort"
      paste0("SA_", mid, "_", input$sa_ansicht, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      df <- sa_data()
      req(!is.null(df) && nrow(df) > 0)
      p <- switch(input$sa_ansicht,
                  heatmap         = heatmap_standort_ggplot(df, input$sa_stufe),
                  heatmap_zukunft = heatmap_bae_zukunft_function(
                                      df, unique(df$MASTER_ID)[1], input$sa_stufe,
                                      out_dir = NULL),
                  szenario = auswertung_standort_ggplot(df, input$sa_stufe, "Szenario"),
                  zeitraum = auswertung_standort_ggplot(df, input$sa_stufe, "Zeitraum"),
                  modell   = auswertung_standort_ggplot(df, input$sa_stufe, "Modell"),
                  grid     = auswertung_grid_ggplot(df, input$sa_stufe),
                  heatmap_standort_ggplot(df, input$sa_stufe)
      )
      ggplot2::ggsave(file, plot = p, width = 5400, height = 3200,
                      units = "px", dpi = 300, device = "png")
      
    }
  )
  
  # CSV-Download
  output$sa_download_csv <- downloadHandler(
    filename = function() {
      sel <- selected_punkt()
      mid <- if (!is.null(sel)) as.character(sel$punkt$MASTER_ID) else "standort"
      paste0("SA_", mid, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- sa_data()
      req(!is.null(df) && nrow(df) > 0)
      data.table::fwrite(df, file, sep = ";", bom = TRUE)
    }
  )
  
} # end server


# ---- 3. App starten ----

shinyApp(ui, server)