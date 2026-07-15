# ----  bae_auswertung_grafiken_standalone.R  ----
#
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App). Verdichtet die
# BAE-Heatmap zu zwei übersichtlicheren Auswertungsgrafiken pro
# Bewertungsstufe und MASTER_ID:
#
#   1) bae_konsens_function()     -> "Konsens der TVs" (Skizze 1, je Stufe 2 PNGs)
#      Ersetzt die alten TV-Spline-Kurven (Spline über die kategoriale Baumart-
#      Achse = irreführend, unlesbar). Pro Szenario×Zeitraum-Panel:
#        1a Konsens-Balken – gestapelter Anteil der TVs je Empfehlungskategorie
#           (einfarbig = TVs einig, gemischt = uneinig).
#        1b Konsens-Kurve  – Median-Stufe (Linie) + Spannband (min..max über die
#           TVs), gerade Segmente statt Spline.
#      Dateien: KonsensBalken_<st>_<MID>_<Modell>.png, KonsensKurve_<st>_<MID>_<Modell>.png
#
#   2) bae_modus_matrix_function() -> "Häufigste Empfehlung" (Skizze 2)
#      DATENGETRIEBEN SORTIERTE Matrix (Zeile TV × Methode) × Baumart. KEIN Score
#      – es wird nur AUSGEZÄHLT: je Zelle werden die Empfehlungskategorien über
#      alle Einträge der Gruppe gezählt. Die Methode (Spalte Hinweis) wird zur
#      eigenen Zeile: TV6 (KHorg) / TV6 (KHff) / TV6 (KM) sind echte, getrennte
#      Rechnungen; AltBA/BAE20/WKE sind baumart-spezifisch und werden in die
#      Standardzeile "TVx" zusammengelegt (steuerbar über hinweis_row).
#      Die Kachel zeigt die HÄUFIGSTE Kategorie (Modus):
#        * Farbe = häufigste Kategorie (custom_palette, diskret; pBv, fehlende
#                  Daten und leere Zellen alle als "keine Einschätzung möglich"
#                  = hellgraue Kacheln)
#        * Zahl  = ABSOLUTE Anzahl dieser häufigsten Kategorie (je Klimalauf
#                  meist 1; > 1 erst, wenn eine Gruppe mehrere Klimaläufe zählt).
#                  Bei nur EINEM Klimalauf in der Gruppe (alles zwangsläufig 1)
#                  wird die Zahl NICHT gedruckt – sie trägt dort keine Info.
#        * Gleichstand: die BESSERE Kategorie wird gezeigt und mit "*" am Wert
#                       sowie einem Rahmen um die Kachel markiert.
#      Standard (trennung = "Klimalauf"): eine Matrix je Klimalauf, unaggregiert
#      wie die Heatmap. Alternativ über Zeit/Szenario zählbar.
#      facet = TRUE legt ALLE Gruppen in EINE facettierte Grafik (facet_wrap,
#      wie die Kurvengrafik) statt einzelner PNGs; die Achsen sind dann gemeinsam
#      und GLOBAL (über alle Gruppen) gewichtet sortiert. facet = FALSE (Default)
#      = wie bisher: je Gruppe eine eigene, eigen sortierte PNG.
#      Sortierung GEWICHTET (dunkelgrün zählt am meisten) über die Summe der
#      Empfehlungsstufe (sehr empfohlen = max … nicht empfohlen = 1; pBv / Keine
#      Datengrundlage = 0):
#        * Zeilen (TV × Methode): höchste Summe -> beste Zeile oben
#        * Spalten (Baumart):     höchste Summe -> beste Baumart rechts
#      Datei: ModusMatrix_<st>_<grp>_<MID>_<Modell>.png
#
#   bae_auswertung_grafiken() ruft beide nacheinander auf.
#   bae_modus_facet_paar() legt zwei facettierte Modus-Matrizen (z. B. binär
#   links, 4-stufig rechts) unter EINER Überschrift nebeneinander; die beiden
#   Einzel-Facets werden dabei auch separat gespeichert.
#   bae_konsens_facet_paar() legt zwei Konsens-Balken (z. B. 4-stufig oben,
#   binär unten) unter EINER Überschrift untereinander; die beiden Einzel-Balken
#   werden dabei auch separat gespeichert.
#
# Datengrundlage/Spalten identisch zu heatmap_bae_zukunft_standalone.R:
#   MASTER_ID, Baumart, TV, Klimalauf, BAE_3ST, BAE_4ST, BAE_5ST
# `Klimalauf` z. B. "OBS_DWD_1991-2020", "RCP45_MPICLM_2071-2100",
#                   "RCP45-v3_MPICLM_2021-2050", "RCP85_MPICLM_2071-2100_v2".
# Stufen-Mappings und Farbpalette sind mit dem Heatmap-Skript konsistent.

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ----  1  GEMEINSAME KONSTANTEN (konsistent mit heatmap_bae_zukunft_standalone.R) ----

# Farbpalette der Kategorien (Kachelfarben der Heatmap)
.bae_palette <- c(
  "sehr empfohlen"        = "#1A9850",
  "empfohlen"             = "#A6D96A",
  "mäßig empfohlen"       = "#FEE08B",
  "wenig empfohlen"       = "#FDAE61",
  "nicht empfohlen"            = "#A50026",
  # Original-Grautöne (grau_zusammen = FALSE) …
  "pBv"                        = "#404040",
  "Keine Datengrundlage"       = "#B0B0B0",
  # … und die zusammengefasste Sammelkategorie (grau_zusammen = TRUE, Default):
  # pBv + Keine Datengrundlage + leere Zellen -> hellgrau.
  "keine Einschätzung möglich" = "#D9D9D9"
)

# Code -> Kategorie je Stufigkeit (Code 1 = beste Bewertung)
# "2st" = binär aus der 3-stufigen Spalte (BAE_3ST – bei 5st fehlen vielerorts
# die Daten): NUR der schlechteste Code 3 = "nicht empfohlen", die übrigen
# gültigen Codes (1-2) = "empfohlen". pBv / leer / NA fallen (wie bei den
# anderen Stufen) auf "keine Einschätzung möglich".
.bae_maps <- list(
  "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
  "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "nicht empfohlen"),
  "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "wenig empfohlen", "5" = "nicht empfohlen"),
  "2st" = c("1" = "empfohlen", "2" = "empfohlen", "3" = "nicht empfohlen")
)
.bae_col <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST",
              "2st" = "BAE_3ST")

# Kategorien je Stufe von "schlecht" (Stufe 1, unten/rot) nach "gut"
# (Stufe n, oben/grün). Index in diesem Vektor = numerische Empfehlungsstufe
# UND Rangwert (Gewicht) für den Score. Damit gilt automatisch:
#   Stufe / Rangwert = (n_Stufen + 1) - Code.
.bae_kat_order <- list(
  "3st" = c("nicht empfohlen", "mäßig empfohlen", "sehr empfohlen"),
  "4st" = c("nicht empfohlen", "mäßig empfohlen", "empfohlen", "sehr empfohlen"),
  "5st" = c("nicht empfohlen", "wenig empfohlen", "mäßig empfohlen",
            "empfohlen", "sehr empfohlen"),
  "2st" = c("nicht empfohlen", "empfohlen")
)

# Farben für die (bis zu 12) TV-Linien in Skizze 1
.bae_tv_colors <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#1F78B4", "#B2182B", "#33A02C", "#6A3D9A"
)

# Code -> Kategorie (Originaldefinition aus den Daten: pBv bzw. Keine
# Datengrundlage getrennt). Das optionale Zusammenfassen zu "keine Einschätzung
# möglich" macht .bae_grau_merge() bei grau_zusammen = TRUE.
.bae_map_val <- function(val, mapping) {
  val <- as.character(val)
  dplyr::case_when(
    val %in% names(mapping) ~ unname(mapping[val]),
    val == "pBv"            ~ "pBv",
    TRUE                    ~ "Keine Datengrundlage"
  )
}

# pBv + Keine Datengrundlage -> "keine Einschätzung möglich" (hellgrau).
# Nur bei grau_zusammen = TRUE angewandt; sonst bleiben beide Kategorien getrennt.
.bae_grau_merge <- function(kat) {
  ifelse(as.character(kat) %in% c("pBv", "Keine Datengrundlage"),
         "keine Einschätzung möglich", as.character(kat))
}

# Hinweis (= Rechenmethode) -> Zeilen-Label in der Matrix (Skizze 2). Jede
# eigenständige Methode wird zu einer eigenen Zeile "TVx (Label)". Ein leeres
# Label ("") legt in die Standardzeile "TVx" zusammen: AltBA/BAE20/WKE sind
# baumart-spezifisch und landen so in "TVx" (füllen dort ihre Baumart-Spalten).
# Ein leerer Hinweis ("") und nicht gelistete Hinweise fallen automatisch auf
# sich selbst zurück (leerer Hinweis -> Standardzeile). Kein ""-Eintrag hier,
# da c("" = ...) in R einen Fehler wirft (leerer Variablenname).
.bae_hinweis_row <- c(
  "KHoriginal"    = "KHorg",
  "KHformfitting" = "KHff",
  "KM"            = "KM",
  "kor"           = "kor",
  "AltBA"         = "",
  "BAE20"         = "",
  "WKE"           = ""
)

# Baumart-Anzeigenamen (Kürzel wie in der Grafik gewünscht) c("<intern>" =
# "<Anzeige>"). Wird in .bae_prep() auf die Baumart angewandt; nicht gelistete
# Baumarten behalten ihren Originalnamen.
.bae_baumart_labels <- c(
  "Fi"  = "GFI",
  "Ta"  = "WTA",
  "Bah" = "BAH",
  "La"  = "ELA",
  "Dgl" = "GDG",
  "Rei" = "REI",
  "Tei" = "TEI",
  "Bu"  = "RBU",
  "Hbu" = "HBU",
  "Ki"  = "GKI",
  "Bi"  = "GBI",
  "Sei" = "SEI"
)

#' Gemeinsame Aufbereitung (intern): auf MASTER_ID filtern, Klimalauf zerlegen,
#' Szenarien- und RCP-Zukunfts-Filter anwenden. Liefert das aufbereitete
#' data.frame `d` (ohne stufenabhängige Kategorie/Stufe – die ergänzt
#' .bae_add_stufe() je Stufe) oder NULL, wenn nichts übrig bleibt.
#'
#' @param data,master_id,rcp_zukunft_ab,obs_alle,szen_rename,szenarien
#'   wie bei den öffentlichen Funktionen.
.bae_prep <- function(data, master_id, rcp_zukunft_ab = 2021,
                      obs_alle = TRUE, szen_rename = character(0),
                      szenarien = c("OBS", "RCP45", "RCP85")) {

  d <- data %>% dplyr::filter(as.character(MASTER_ID) == as.character(master_id))
  if (nrow(d) == 0) {
    message("Keine Daten für MASTER_ID: ", master_id)
    return(NULL)
  }

  # Szenario / Modell / Zeitraum / Variante aus Klimalauf ableiten
  d <- d %>%
    dplyr::mutate(
      Klimalauf = as.character(Klimalauf),
      Zeitraum  = stringr::str_extract(Klimalauf, "\\d{4}-\\d{4}"),
      Variante  = tolower(stringr::str_extract(Klimalauf, "[vV][0-9]+")),
      Szenario  = stringr::str_remove(stringr::str_extract(Klimalauf, "^[^_]+"),
                                      "[-_]?[vV][0-9]+$"),
      Modell    = stringr::str_remove(
        stringr::str_remove(Klimalauf, "^[^_]+_"),
        "_?\\d{4}-\\d{4}.*$"),
      Szen_label = ifelse(is.na(Variante), Szenario,
                          paste0(Szenario, "_", Variante)),
      Startjahr  = suppressWarnings(as.integer(stringr::str_sub(Zeitraum, 1, 4)))
    )

  # Zeilen-Labels optional umbenennen  c("<intern>" = "<Anzeige>")
  if (length(szen_rename) > 0) {
    idx <- match(d$Szen_label, names(szen_rename))
    treffer <- !is.na(idx)
    d$Szen_label[treffer] <- unname(szen_rename[idx[treffer]])
  }

  ohne_zeit <- is.na(d$Zeitraum)
  if (any(ohne_zeit)) {
    message("Hinweis: ", sum(ohne_zeit), " Zeile(n) ohne erkennbaren Zeitraum ",
            "werden ignoriert.")
    d <- d[!ohne_zeit, , drop = FALSE]
  }

  # Szenarien-Auswahl: nur die gewünschten Szenarien behalten (auf der BASIS
  # Szenario, d. h. RCP45 schließt Varianten wie RCP45-v3 mit ein). Default =
  # OBS (der Beobachtungslauf 1991-2020) + RCP45 + RCP85. szenarien = NULL
  # behält alle Szenarien.
  if (!is.null(szenarien)) {
    d <- d[d$Szenario %in% szenarien, , drop = FALSE]
    if (nrow(d) == 0) {
      message("Nach Szenarien-Filter (", paste(szenarien, collapse = ", "),
              ") keine Daten mehr für: ", master_id)
      return(NULL)
    }
  }

  # RCP: nur Zukunft; OBS: optional alles
  is_rcp   <- grepl("^RCP", d$Szenario, ignore.case = TRUE)
  keep_rcp <- !is.na(d$Startjahr) & d$Startjahr >= rcp_zukunft_ab
  d <- d[(!is_rcp) | keep_rcp, , drop = FALSE]
  if (!obs_alle) {
    is_obs <- grepl("^OBS", d$Szenario, ignore.case = TRUE)
    d <- d[!is_obs | (!is.na(d$Startjahr) & d$Startjahr >= rcp_zukunft_ab), ,
           drop = FALSE]
  }
  if (nrow(d) == 0) {
    message("Nach Zukunfts-/Zeitraum-Filter keine Daten mehr für: ", master_id)
    return(NULL)
  }

  # Baumart auf die Anzeige-Kürzel umbenennen (nicht gelistete bleiben unverändert)
  ba  <- as.character(d$Baumart)
  idx <- match(ba, names(.bae_baumart_labels))
  treffer <- !is.na(idx)
  ba[treffer] <- unname(.bae_baumart_labels[idx[treffer]])
  d$Baumart <- ba

  # Faktor-Ordnungen (global)
  d <- d %>%
    dplyr::mutate(
      Szen_label = factor(Szen_label, levels = sort(unique(Szen_label))),
      Zeitraum   = factor(Zeitraum,   levels = sort(unique(Zeitraum))),
      TV         = factor(paste0("TV", TV),
                          levels = sort(unique(paste0("TV", TV)))),  # TV1 … TVn
      Baumart    = factor(Baumart,    levels = sort(unique(as.character(Baumart)))),
      ist_rcp    = grepl("^RCP", Szenario, ignore.case = TRUE)
    )
  d
}

# stufenabhängige Kategorie + numerische Stufe/Rangwert ergänzen
.bae_add_stufe <- function(d, st) {
  kat_col <- .bae_col[[st]]
  if (is.null(kat_col) || !kat_col %in% names(d)) return(NULL)
  ordn    <- .bae_kat_order[[st]]
  lvl_map <- setNames(seq_along(ordn), ordn)   # Kategorie -> Stufe/Rangwert
  d %>%
    dplyr::mutate(
      Kategorie = .bae_map_val(.data[[kat_col]], .bae_maps[[st]]),
      Stufe     = unname(lvl_map[Kategorie]),  # invertiert, hoch = gut (nur für Kurven-y)
      # Wert = die Einteilung DIREKT wie in BAE_xST: 1 = beste Empfehlung …
      #        n = schlechteste. pBv / leer / nicht-numerisch -> NA (zählt nicht).
      Wert      = suppressWarnings(as.integer(as.character(.data[[kat_col]])))
    )
}

.bae_modell_str <- function(d) {
  m <- sort(unique(as.character(d$Modell)))
  m <- m[!is.na(m) & nzchar(m)]
  if (length(m)) paste(m, collapse = "-") else "NA"
}

# Y-Achsen-Labeller der Modus-Matrix: die TV×Methode-Zeilen NUR ANZEIGE-seitig auf
# Buchstaben umstellen (A = oberste Zeile, dann B, C, … nach unten). Ausnahme:
# "TV2" behält seinen Namen und verbraucht KEINEN Buchstaben. Ändert nur die
# Tick-Beschriftung, nicht die Sortierung/Daten. `lv` = Faktor-Level in
# Achsenreihenfolge (aufsteigend -> unterste Zeile zuerst), deshalb von hinten.
.bae_tv_labeller <- function(lv) {
  out <- character(length(lv))
  i   <- 0
  for (k in rev(seq_along(lv))) {            # oben (letztes Level) -> unten
    i <- i + 1                               # TV2 zählt MIT (verbraucht seinen Buchstaben)
    out[k] <- if (identical(as.character(lv[k]), "TV2")) "TV2"
              else if (i <= length(LETTERS)) LETTERS[i]
              else paste0("Z", i)            # -> A, B, C, D, TV2, F, …
  }
  out
}

# Referenz-Sortierung (feste Achsen für Vergleiche): liefert die TV×Methode- und
# Baumart-Reihenfolge EINER Referenz-Stufe (gewichtete Summe der Stufe, schlecht
# -> gut) als Level-Vektoren. So können mehrere Grafiken (z. B. binär + 4-stufig
# nebeneinander) dieselbe Achsenaufteilung nutzen, statt jede für sich zu
# sortieren. Liefert NULL, wenn die Stufe keine Daten hat.
.bae_ref_levels <- function(data, master_id, stufe, hinweis_row = .bae_hinweis_row,
                            rcp_zukunft_ab = 2021, obs_alle = TRUE,
                            szen_rename = character(0),
                            szenarien = c("OBS", "RCP45", "RCP85")) {
  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  if (is.null(d0)) return(NULL)
  if (!"Hinweis" %in% names(d0)) d0$Hinweis <- ""
  d0 <- d0 %>%
    dplyr::mutate(
      Hinweis = dplyr::coalesce(as.character(Hinweis), ""),
      Methode = dplyr::coalesce(unname(hinweis_row[Hinweis]), Hinweis),
      TV_M    = ifelse(Methode == "", as.character(TV),
                       paste0(as.character(TV), " (", Methode, ")")))
  d_st <- .bae_add_stufe(d0, stufe)
  if (is.null(d_st)) return(NULL)
  gew <- d_st %>% dplyr::mutate(w = dplyr::coalesce(as.numeric(Stufe), 0))
  tv <- gew %>% dplyr::group_by(TV_M) %>%
    dplyr::summarise(s = sum(w), .groups = "drop") %>% dplyr::arrange(s, TV_M)
  ba <- gew %>% dplyr::group_by(Baumart) %>%
    dplyr::summarise(s = sum(w), .groups = "drop") %>% dplyr::arrange(s, Baumart)
  list(tv = as.character(tv$TV_M), ba = as.character(ba$Baumart))
}

# ----  2  SKIZZE 1 – Konsens der TVs (Balken + Kurve) ----

#' Konsens der TVs je Baumart (Skizze 1) – ersetzt die alten Spline-Kurven
#'
#' Die früheren TV-Kurven zogen einen glatten Spline über die KATEGORIALE
#' Baumart-Achse und interpolierten damit Werte, die es nicht gibt (Baumarten
#' sind ungeordnet). Bei 2–5 y-Stufen und bis zu 12 TVs war das Ergebnis ein
#' unlesbares Kurven-Wirrwarr. Diese Funktion beantwortet die eigentliche Frage
#' (wie breit wird eine Baumart empfohlen, wie einig sind die TVs?) mit ZWEI
#' Grafiken je Stufe:
#'   1a Konsens-Balken: pro (Panel, Baumart) gestapelter Anteilsbalken der TVs je
#'      Empfehlungskategorie. Einfarbig = TVs einig; gemischt = uneinig.
#'   1b Konsens-Kurve: pro (Panel, Baumart) Median-Stufe (Linie) + Spannband
#'      (min..max über die TVs), gerade Segmente, KEIN Spline. Schmales Band =
#'      einig, breites Band = uneinig; graue Punkte = einzelne TVs.
#'
#' @param data          data.frame mit MASTER_ID, Baumart, TV, Klimalauf sowie
#'                       BAE_3ST / BAE_4ST / BAE_5ST (wie im Heatmap-Skript).
#' @param master_id     ID des Standorts, auf den gefiltert wird.
#' @param stufen        Bewertungsstufen, je Stufe zwei PNGs: "3st","4st","5st","2st".
#' @param trennung      Facetten-Gruppierung: "Klimalauf" (je Klimalauf, Default),
#'                       "Zeit" (Vergangenheit vs. Zukunft), "Szenario", "Keine".
#' @param rcp_zukunft_ab RCP-Läufe erst ab diesem Startjahr behalten (Default 2021).
#' @param obs_alle       TRUE = OBS-Läufe unabhängig vom Zeitraum behalten.
#' @param szen_rename    benannter Vektor c("<intern>" = "<Anzeige>") zum
#'                       Umbenennen der Szenario-Labels (optional).
#' @param szenarien      zu behaltende Basis-Szenarien (Default OBS+RCP45+RCP85);
#'                       NULL = alle Szenarien.
#' @param facet_ncol     Spaltenzahl der Facetten (Default 3). NULL = automatisch
#'                       (ceiling(sqrt(Anzahl Gruppen))).
#' @param order_ref      Steuert, WELCHE Stufe die Baumart-Reihenfolge vorgibt.
#'                       NULL = jede Stufe sortiert selbst; sonst eine Referenz-
#'                       Stufe (z. B. "4st"), deren Gewichtung ALLE Stufen dieses
#'                       Aufrufs übernehmen -> 4st oben und 2st unten je Facette
#'                       gleich sortiert. Die Balken (1a) sortieren IMMER PRO
#'                       FACETTE (je Klimalauf/Zeit/Szenario eine eigene
#'                       Reihenfolge, bestempfohlene rechts); die Kurve (1b) nutzt
#'                       eine globale Reihenfolge über alle Gruppen.
#' @param grau_zusammen  TRUE (Default) = pBv + Keine Datengrundlage zu EINER
#'                       Kategorie "keine Einschätzung möglich" (hellgrau)
#'                       zusammenfassen. FALSE = Originaldefinition aus den Daten
#'                       (pBv schwarz, Keine Datengrundlage grau getrennt).
#' @param out_dir        Ausgabeordner; je MASTER_ID entsteht ein Unterordner.
#' @return unsichtbar eine Liste der ggplot-Objekte (Nebeneffekt: PNGs).
bae_konsens_function <- function(data,
                                 master_id,
                                 stufen         = c("3st", "4st", "5st", "2st"),
                                 trennung       = c("Klimalauf", "Zeit", "Szenario", "Keine"),
                                 rcp_zukunft_ab = 2021,
                                 obs_alle       = TRUE,
                                 szen_rename    = character(0),
                                 szenarien      = c("OBS", "RCP45", "RCP85"),
                                 facet_ncol     = NULL,
                                 order_ref      = NULL,
                                 grau_zusammen  = TRUE,
                                 out_dir        = "04_results/BAE_Auswertung/auswertung") {

  trennung <- match.arg(trennung)
  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  if (is.null(d0)) return(invisible(NULL))

  # Facetten-Gruppe wie die Modus-Matrix: klimalauf (Default) / zeit / szenario / keine
  d0 <- d0 %>%
    dplyr::mutate(Gruppe = switch(trennung,
                                  "Klimalauf" = as.character(Klimalauf),
                                  "Zeit"      = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
                                  "Szenario"  = as.character(Szen_label),
                                  "Keine"     = "alle"))
  gruppen <- sort(unique(d0$Gruppe))

  modelle_str <- .bae_modell_str(d0)
  mid_dir     <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  n_grp    <- length(gruppen)
  fac_ncol <- if (!is.null(facet_ncol)) facet_ncol else ceiling(sqrt(n_grp))
  fac_nrow <- ceiling(n_grp / fac_ncol)
  breite   <- 500 + fac_ncol * 900
  hoehe    <- 400 + fac_nrow * 650

  # Ordnungs-Quelle für die Baumart-Sortierung: mit order_ref die Referenz-Stufe
  # (feste, gemeinsame Reihenfolge über alle Stufen des Aufrufs -> z. B. 4st oben
  # und 2st unten je Facette gleich sortiert), sonst je Stufe die Stufe selbst.
  # ord_tag markiert die PNGs, damit fest sortierte die selbst sortierten nicht
  # überschreiben.
  d_ref <- if (!is.null(order_ref)) .bae_add_stufe(d0, order_ref) else NULL
  if (!is.null(order_ref) && is.null(d_ref))
    message("order_ref '", order_ref, "' ohne Daten – Sortierung fällt je Stufe selbst.")
  ord_tag <- if (!is.null(d_ref)) paste0("_", order_ref, "ord") else ""

  strip_theme <- ggplot2::theme(
    strip.text       = ggplot2::element_text(face = "bold", size = 15),
    strip.background = ggplot2::element_rect(fill = "grey95", color = "grey70", linewidth = 0.6),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background  = ggplot2::element_rect(fill = "white", color = NA))

  plots <- list()
  for (st in stufen) {
    d_st <- .bae_add_stufe(d0, st)
    if (is.null(d_st)) {
      message("Spalte für Stufe '", st, "' nicht vorhanden – übersprungen.")
      next
    }

    # grau_zusammen: pBv + Keine Datengrundlage zu "keine Einschätzung möglich".
    if (grau_zusammen) d_st <- d_st %>% dplyr::mutate(Kategorie = .bae_grau_merge(Kategorie))

    ordn   <- .bae_kat_order[[st]]
    kat_lv <- if (grau_zusammen) c(ordn, "keine Einschätzung möglich")
              else                c(ordn, "pBv", "Keine Datengrundlage")

    # Baumart-Sortierung GEWICHTET (Summe der Stufe = Nennungen × Empfehlungsstufe):
    # schwächster Konsens links, bestempfohlene rechts. Ordnungs-Quelle = Referenz-
    # Stufe (order_ref) oder die aktuelle Stufe selbst.
    ord_dat <- if (!is.null(d_ref)) d_ref else d_st

    # GLOBAL (für die Kurve 1b, EINE Achse über alle Gruppen)
    ba_lv <- ord_dat %>%
      dplyr::group_by(Baumart) %>%
      dplyr::summarise(s = sum(dplyr::coalesce(as.numeric(Stufe), 0)), .groups = "drop") %>%
      dplyr::arrange(s, Baumart) %>%
      dplyr::pull(Baumart) %>% as.character()

    # PER FACETTE (für die Balken 1a): je Gruppe (Klimalauf/Zeit/Szenario) eine
    # EIGENE Reihenfolge -> in JEDEM Panel steht die dort bestempfohlene Baumart
    # rechts. Umsetzung über einen "Baumart___Gruppe"-Schlüssel (reorder_within-
    # Muster) + facet scales = "free_x"; das Achsen-Label blendet den Gruppen-Teil
    # wieder aus. Level-Reihenfolge = je Gruppe aufsteigend gewichtet.
    ba_grp <- ord_dat %>%
      dplyr::group_by(Gruppe, Baumart) %>%
      dplyr::summarise(s = sum(dplyr::coalesce(as.numeric(Stufe), 0)), .groups = "drop") %>%
      dplyr::arrange(Gruppe, s, Baumart) %>%
      dplyr::mutate(x_key = paste(as.character(Baumart), as.character(Gruppe), sep = "___"))
    key_lv <- ba_grp$x_key

    # ---- 1a Konsens-Balken: je (Gruppe, Baumart) Anteil der TVs je Kategorie ----
    # eine Kategorie je (Gruppe, Baumart, TV): Modus über Hinweis-Methoden (und
    # Klimaläufe der Gruppe, falls trennung aggregiert)
    tv_kat <- d_st %>%
      dplyr::count(Gruppe, Baumart, TV, Kategorie, name = "n") %>%
      dplyr::group_by(Gruppe, Baumart, TV) %>%
      dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    konsens <- tv_kat %>%
      dplyr::count(Gruppe, Baumart, Kategorie, name = "n_tv") %>%
      dplyr::mutate(Kategorie = factor(Kategorie, levels = kat_lv),
                    Gruppe    = factor(as.character(Gruppe),  levels = gruppen),
                    x_key     = factor(paste(as.character(Baumart), as.character(Gruppe), sep = "___"),
                                       levels = key_lv))

    p_bal <- ggplot2::ggplot(konsens, ggplot2::aes(x = x_key, y = n_tv, fill = Kategorie)) +
      ggplot2::geom_col(position = "fill", width = 0.9) +
      ggplot2::facet_wrap(~ Gruppe, ncol = fac_ncol, scales = "free_x") +
      ggplot2::scale_fill_manual(values = .bae_palette, limits = kat_lv, drop = FALSE) +
      ggplot2::scale_x_discrete(labels = function(k) sub("___.*$", "", k)) +
      ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
      ggplot2::labs(
        title    = paste0("BAE – Konsens der TVs je Baumart – ", master_id),
        subtitle = paste0(sub("st$", "", st), "-stufig  |  je ", trennung,
                          "  |  Anteil der TVs je Empfehlung  |  empfohlene Baumarten rechts →  |  Modell: ",
                          modelle_str),
        x = "Baumart  (bestempfohlene →)", y = "Anteil der TVs", fill = "Empfehlung") +
      ggplot2::theme_minimal(base_size = 15) + strip_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 15),
                     legend.position = "bottom")

    f_bal <- file.path(mid_dir, paste0("KonsensBalken_", st, "_", master_id, "_",
                                       modelle_str, ord_tag, ".png"))
    ggplot2::ggsave(f_bal, plot = p_bal, device = "png",
                    width = breite, height = hoehe, units = "px", dpi = 150, limitsize = FALSE)
    message("Gespeichert: ", f_bal)
    plots[[paste0(st, "_balken")]] <- p_bal

    # ---- 1b Konsens-Kurve: Median-Stufe + Spannband ("Konfidenzintervall" der TVs) ----
    # ein Stufen-Wert je (Gruppe, TV, Baumart): Mittel über Hinweis-Methoden/Klimaläufe
    tv_val <- d_st %>%
      dplyr::group_by(Gruppe, TV, Baumart) %>%
      dplyr::summarise(y = mean(Stufe, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.nan(y)) %>%
      dplyr::mutate(x      = as.integer(factor(Baumart, levels = ba_lv)),
                    Gruppe = factor(as.character(Gruppe), levels = gruppen))
    konsens_band <- tv_val %>%
      dplyr::group_by(Gruppe, Baumart, x) %>%
      dplyr::summarise(y_med = stats::median(y), y_min = min(y), y_max = max(y),
                       .groups = "drop")

    p_kur <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = konsens_band,
                           ggplot2::aes(x = x, ymin = y_min, ymax = y_max),
                           fill = "grey70", alpha = 0.35) +
      ggplot2::geom_point(data = tv_val, ggplot2::aes(x = x, y = y),
                          color = "grey45", size = 0.8, alpha = 0.5) +
      ggplot2::geom_line(data = konsens_band, ggplot2::aes(x = x, y = y_med),
                         color = "#1A9850", linewidth = 1) +
      ggplot2::geom_point(data = konsens_band, ggplot2::aes(x = x, y = y_med),
                          color = "#1A9850", size = 1.6) +
      ggplot2::facet_wrap(~ Gruppe, ncol = fac_ncol) +
      ggplot2::scale_x_continuous(breaks = seq_along(ba_lv), labels = ba_lv) +
      ggplot2::scale_y_continuous(
        breaks = seq_along(ordn), labels = ordn,
        limits = c(1, length(ordn)), expand = ggplot2::expansion(mult = 0.05)) +
      ggplot2::labs(
        title    = paste0("BAE – Konsens-Kurve der TVs – ", master_id),
        subtitle = paste0(sub("st$", "", st), "-stufig  |  je ", trennung,
                          "  |  Linie = Median, Band = Spannweite der TVs  |  empfohlene Baumarten rechts →  |  Modell: ",
                          modelle_str),
        x = "Baumart  (bestempfohlene →)", y = "Empfehlung") +
      ggplot2::theme_minimal(base_size = 11) + strip_theme +
      ggplot2::theme(
        panel.border       = ggplot2::element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 15),
        panel.grid.major.x = ggplot2::element_line(color = "grey92"))

    f_kur <- file.path(mid_dir, paste0("KonsensKurve_", st, "_", master_id, "_",
                                       modelle_str, ord_tag, ".png"))
    ggplot2::ggsave(f_kur, plot = p_kur, device = "png",
                    width = breite, height = hoehe, units = "px", dpi = 150, limitsize = FALSE)
    message("Gespeichert: ", f_kur)
    plots[[paste0(st, "_kurve")]] <- p_kur
  }
  invisible(plots)
}

# ----  3  SKIZZE 2 – Häufigste Empfehlung (ausgezählte, sortierte Matrix) ----

#' Häufigste Empfehlung als ausgezählte, gewichtet sortierte Matrix (Skizze 2)
#'
#' @param data          data.frame wie bei bae_konsens_function (zusätzlich
#'                       optional Spalte Hinweis = Rechenmethode).
#' @param master_id     ID des Standorts, auf den gefiltert wird.
#' @param stufen        Bewertungsstufen: "3st","4st","5st","2st".
#' @param trennung      Gruppierung der Matrizen: "Klimalauf" (je Klimalauf, Default),
#'                       "Zeit" (Vergangenheit vs. Zukunft), "Szenario", "Keine".
#' @param rcp_zukunft_ab RCP-Läufe erst ab diesem Startjahr behalten (Default 2021).
#' @param obs_alle       TRUE = OBS-Läufe unabhängig vom Zeitraum behalten.
#' @param szen_rename    benannter Vektor zum Umbenennen der Szenario-Labels.
#' @param szenarien      zu behaltende Basis-Szenarien (Default OBS+RCP45+RCP85);
#'                       NULL = alle.
#' @param hinweis_row    Zuordnung Hinweis -> Zeilen-Label (leer = in Standardzeile
#'                       "TVx" zusammenlegen).
#' @param werte_anzeigen TRUE = Anzahl je Kachel beschriften (nur wenn eine Gruppe
#'                       mehrere Klimaläufe zusammenfasst).
#' @param facet          TRUE = alle Gruppen in EINE facettierte Grafik (facet_wrap,
#'                       gemeinsame, global sortierte Achsen); FALSE = je Gruppe eine PNG.
#' @param facet_ncol     Spaltenzahl der Facetten (nur bei facet = TRUE). NULL =
#'                       automatisch (~Wurzel). z. B. 1 = alle Gruppen untereinander.
#' @param legend_pos     Legendenposition ("right", "bottom", "none", …).
#' @param order_ref      NULL = jede Grafik sortiert sich selbst (gewichtet). Sonst
#'                       eine Referenz-Stufe (z. B. "4st"): deren gewichtete TV- und
#'                       Baumart-Reihenfolge wird für ALLE Grafiken dieses Aufrufs
#'                       fest übernommen -> Achsen vergleichbar.
#' @param grau_zusammen  TRUE (Default) = pBv + Keine Datengrundlage + leere Zellen
#'                       zu EINER hellgrauen Kategorie "keine Einschätzung möglich"
#'                       (leere Zellen werden aufgefüllt). FALSE = Originalanzeige
#'                       aus den Daten (pBv schwarz, Keine Datengrundlage grau,
#'                       leere Zellen bleiben weiß).
#' @param tv_letters     TRUE (Default) = y-Achse als A, B, C, … (TV2 zählt mit,
#'                       behält aber den Namen). FALSE = echte TV×Methode-Namen.
#' @param out_dir        Ausgabeordner; je MASTER_ID entsteht ein Unterordner.
#' @return unsichtbar eine Liste der ggplot-Objekte (Nebeneffekt: PNGs).
bae_modus_matrix_function <- function(data,
                                      master_id,
                                      stufen         = c("3st", "4st", "5st", "2st"),
                                      trennung       = c("Klimalauf", "Zeit", "Szenario", "Keine"),
                                      rcp_zukunft_ab = 2021,
                                      obs_alle       = TRUE,
                                      szen_rename    = character(0),
                                      szenarien      = c("OBS", "RCP45", "RCP85"),
                                      hinweis_row    = .bae_hinweis_row,
                                      werte_anzeigen = TRUE,     # Anzahl je Zelle beschriften
                                      facet          = FALSE,    # TRUE: alle Gruppen in EINE facettierte Grafik
                                      facet_ncol     = NULL,     # Spaltenzahl der Facetten (NULL = auto)
                                      legend_pos     = "right",  # Legendenposition
                                      order_ref      = NULL,     # Referenz-Stufe für feste Achsen (z. B. "4st")
                                      grau_zusammen  = TRUE,     # pBv+Keine Datengrundlage+leer -> "keine Einschätzung möglich" (hellgrau)
                                      tv_letters     = TRUE,     # y-Achse als A,B,C,… (außer TV2); FALSE = echte TV-Namen
                                      out_dir        = "04_results/BAE_Auswertung/auswertung") {

  # trennung: getrennte, JEWEILS EIGEN SORTIERTE Matrizen (eine PNG je Gruppe)
  #   "Klimalauf" -> UNAGGREGIERT, je Klimalauf eine Matrix (wie die Heatmap-
  #                  Panels). Pro Zelle 1 Methode -> Zahl ist hier meist 1. [Default]
  #   "Zeit"      -> Vergangenheit (OBS) vs. Zukunft (RCP), über Klimaläufe gezählt
  #   "Szenario"  -> je Szen_label eine Matrix (gezählt über die Zeiträume)
  #   "Keine"     -> eine gemeinsame Matrix über alles
  # Die Zahl je Kachel wird erst > 1, wenn eine Gruppe mehrere Klimaläufe zählt
  # (z. B. trennung = "Zeit"/"Keine"): dann = in wie vielen die Kategorie vorkam.
  trennung <- match.arg(trennung)

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  if (is.null(d0)) return(invisible(NULL))

  # Methode (Spalte Hinweis) -> eigene Zeile "TVx (Label)". AltBA/BAE20/WKE etc.
  # mit leerem Label werden in die Standardzeile "TVx" zusammengelegt.
  if (!"Hinweis" %in% names(d0)) d0$Hinweis <- ""
  d0 <- d0 %>%
    dplyr::mutate(
      Hinweis = dplyr::coalesce(as.character(Hinweis), ""),
      Methode = dplyr::coalesce(unname(hinweis_row[Hinweis]), Hinweis),  # nicht gelistet -> sich selbst
      TV_M    = ifelse(Methode == "", as.character(TV),
                       paste0(as.character(TV), " (", Methode, ")")),
      Gruppe  = switch(trennung,
                       "Klimalauf" = as.character(Klimalauf),
                       "Zeit"      = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
                       "Szenario"  = as.character(Szen_label),
                       "Keine"     = "alle"))

  modelle_str <- .bae_modell_str(d0)
  mid_dir     <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  # Feste Referenz-Sortierung (order_ref) einmal berechnen -> für alle Stufen/Gruppen
  # dieselben Achsen. NULL = jede Grafik sortiert sich selbst (bisheriges Verhalten).
  ref_lv <- if (!is.null(order_ref))
    .bae_ref_levels(data, master_id, order_ref, hinweis_row,
                    rcp_zukunft_ab, obs_alle, szen_rename, szenarien) else NULL
  if (!is.null(order_ref) && is.null(ref_lv))
    message("order_ref '", order_ref, "' ohne Daten – Sortierung fällt je Grafik selbst.")
  # Dateinamen-Kürzel für die Referenz-Sortierung (nur wenn wirklich angewandt)
  ord_tag <- if (!is.null(ref_lv)) paste0("_", order_ref, "ord") else ""

  gruppen <- sort(unique(d0$Gruppe))   # alphabetisch: OBS… vor RCP…, chronologisch

  plots <- list()
  for (st in stufen) {
    d_st <- .bae_add_stufe(d0, st)
    if (is.null(d_st)) {
      message("Spalte für Stufe '", st, "' nicht vorhanden – übersprungen.")
      next
    }

    # grau_zusammen: pBv + Keine Datengrundlage zu "keine Einschätzung möglich".
    if (grau_zusammen) d_st <- d_st %>% dplyr::mutate(Kategorie = .bae_grau_merge(Kategorie))

    ordn     <- .bae_kat_order[[st]]                        # schlecht -> gut
    grau_lv  <- if (grau_zusammen) "keine Einschätzung möglich" else c("pBv", "Keine Datengrundlage")
    kat_lv   <- c(ordn, grau_lv)                            # Legenden-/Fill-Reihenfolge
    kat_pref <- c(rev(ordn), grau_lv)                       # best -> schlecht (Gleichstand: bessere gewinnt)
    dunkel   <- if (grau_zusammen) c("sehr empfohlen", "nicht empfohlen")      # hellgrau -> dunkle Schrift
                else                c("sehr empfohlen", "nicht empfohlen", "pBv")  # pBv schwarz -> weiße Schrift

    # --------------------------------------------------------------------
    #  facet = TRUE: ALLE Gruppen in EINE facettierte Grafik (wie die
    #  Kurvengrafik, nur facet_wrap statt einzelner PNGs). Achsen sind
    #  gemeinsam -> GLOBAL gewichtet sortiert (nicht je Panel eigen). Eine
    #  PNG je Stufe; die per-Gruppe-Schleife unten wird übersprungen.
    # --------------------------------------------------------------------
    if (facet) {
      # Auszählen wie unten, aber Gruppe bleibt erhalten: Modus je (Gruppe, Zelle).
      kachel <- d_st %>%
        dplyr::count(Gruppe, TV_M, Baumart, Kategorie, name = "n") %>%
        dplyr::group_by(Gruppe, TV_M, Baumart) %>%
        dplyr::mutate(tie = sum(n == max(n)) > 1) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice_min(match(Kategorie, kat_pref), n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          Kategorie = factor(Kategorie, levels = kat_lv),
          label     = ifelse(tie, paste0(n, "*"), as.character(n)),
          txt_col   = ifelse(as.character(Kategorie) %in% dunkel, "white", "grey15"))

      if (nrow(kachel) == 0) {
        message("Stufe '", st, "': keine Einträge – übersprungen.")
        next
      }

      # GLOBAL gewichtet sortiert (gemeinsame Achsen über alle Facetten):
      # Summe der Stufe je Zeile (TV×Methode) bzw. Baumart über ALLE Gruppen.
      # Mit order_ref: stattdessen die feste Referenz-Reihenfolge (present-Werte
      # angehängt, damit nichts verloren geht).
      gew <- d_st %>% dplyr::mutate(w = dplyr::coalesce(as.numeric(Stufe), 0))
      tv_ord <- gew %>% dplyr::group_by(TV_M) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, TV_M)
      ba_ord <- gew %>% dplyr::group_by(Baumart) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, Baumart)
      tv_lv <- if (!is.null(ref_lv)) union(ref_lv$tv, as.character(tv_ord$TV_M)) else as.character(tv_ord$TV_M)
      ba_lv <- if (!is.null(ref_lv)) union(ref_lv$ba, as.character(ba_ord$Baumart)) else as.character(ba_ord$Baumart)

      kachel <- kachel %>%
        dplyr::mutate(
          TV_M    = factor(as.character(TV_M),    levels = tv_lv),
          Baumart = factor(as.character(Baumart), levels = ba_lv),
          Gruppe  = factor(as.character(Gruppe),  levels = gruppen))

      # Leere (sonst WEISSE) Zellen als "keine Einschätzung möglich" auffüllen ->
      # lückenlose, hellgraue Matrix statt weißer Löcher. Nur bei grau_zusammen;
      # sonst bleiben leere Zellen weiß (Originalanzeige).
      if (grau_zusammen) kachel <- kachel %>%
        tidyr::complete(Gruppe, TV_M, Baumart) %>%
        dplyr::mutate(
          Kategorie = factor(dplyr::coalesce(as.character(Kategorie), "keine Einschätzung möglich"),
                             levels = kat_lv),
          tie       = dplyr::coalesce(tie, FALSE),
          label     = dplyr::coalesce(label, ""),
          txt_col   = dplyr::coalesce(txt_col, "grey15"))

      # Zahl nur zeigen, wenn mind. eine Gruppe mehrere Klimaläufe zusammenfasst
      # (bei trennung = "Klimalauf" hat jede Gruppe genau 1 Lauf -> alles 1 -> weg).
      laeufe_je_grp <- d_st %>% dplyr::distinct(Gruppe, Klimalauf) %>%
        dplyr::count(Gruppe)
      zahl_zeigen   <- werte_anzeigen && any(laeufe_je_grp$n > 1)

      n_ba     <- length(levels(kachel$Baumart))
      n_tv     <- length(levels(kachel$TV_M))
      n_grp    <- length(gruppen)
      fac_ncol <- if (!is.null(facet_ncol)) facet_ncol else ceiling(sqrt(n_grp))
      fac_nrow <- ceiling(n_grp / fac_ncol)

      p <- ggplot2::ggplot(kachel, ggplot2::aes(x = Baumart, y = TV_M)) +
        ggplot2::geom_tile(ggplot2::aes(fill = Kategorie), color = "white", linewidth = 0.6) +
        ggplot2::geom_tile(data = dplyr::filter(kachel, tie),
                           fill = NA, color = "grey15", linewidth = 1.1) +
        { if (zahl_zeigen)
          ggplot2::geom_text(ggplot2::aes(label = label, colour = txt_col), size = 3) } +
        ggplot2::facet_wrap(~ Gruppe, ncol = fac_ncol) +
        ggplot2::scale_fill_manual(values = .bae_palette, limits = kat_lv, drop = FALSE) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_y_discrete(labels = if (tv_letters) .bae_tv_labeller else ggplot2::waiver()) +
        ggplot2::labs(
          title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
          subtitle = paste0(sub("st$", "", st), "-stufig  |  facettiert je Gruppe (", trennung,
                            ")  |  Modell: ", modelle_str),
          x = "Baumart  (beste Empfehlungen →)",
          y = "TV × Methode  (meiste Empfehlungen oben ↑)",
          fill = "häufigste Kategorie") +
        ggplot2::coord_equal() +
        # Beschriftungs-Größen wie in bae_modus_facet_paar (groesser-Theme): große
        # Streifen/Achsen/Titel/Legende, damit die facettierte Einzel-Matrix genauso
        # gut lesbar ist. Legende in 2 Zeilen, sonst laeuft die 6-teilige 4st-Legende
        # bei der großen Schrift über den Rand.
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(size = 25, face = "bold"),
          plot.subtitle    = ggplot2::element_text(size = 16),
          strip.text       = ggplot2::element_text(face = "bold", size = 25),
          strip.background = ggplot2::element_rect(fill = "grey95", color = "grey70",
                                                   linewidth = 0.6),
          axis.title       = ggplot2::element_text(size = 25),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                   face = "bold", size = 25),
          axis.text.y      = ggplot2::element_text(face = "bold", size = 25),
          panel.grid       = ggplot2::element_blank(),
          legend.position  = legend_pos,
          legend.text      = ggplot2::element_text(size = 20),
          legend.title     = ggplot2::element_text(size = 25),
          plot.background  = ggplot2::element_rect(fill = "white", color = NA))

      f <- file.path(mid_dir, paste0("ModusMatrix_facet_", st, "_",
                                     master_id, "_", modelle_str, ord_tag, ".png"))
      ggplot2::ggsave(f, plot = p, device = "png",
                      width  = (500 + n_ba * 95) * fac_ncol + 200,
                      height = (400 + n_tv * 95) * fac_nrow + 200,
                      units = "px", dpi = 150, limitsize = FALSE)
      message("Gespeichert: ", f)
      plots[[paste0(st, "_facet")]] <- p
      next
    }

    for (grp in gruppen) {
      # Auszählen (KEIN Score): je Zelle (Zeile TV×Methode  ×  Baumart) je
      # Kategorie die Anzahl über alle Einträge (bei mehreren Klimaläufen je Gruppe).
      zaehl <- d_st %>%
        dplyr::filter(Gruppe == grp) %>%
        dplyr::count(TV_M, Baumart, Kategorie, name = "n")

      if (nrow(zaehl) == 0) {
        message("Stufe '", st, "', Gruppe '", grp,
                "': keine Einträge – übersprungen.")
        next
      }

      # Kachel = häufigste Kategorie (Modus). Bei Gleichstand die BESSERE
      # (kleinster kat_pref) + Markierung tie (Sternchen/Rahmen).
      kachel <- zaehl %>%
        dplyr::group_by(TV_M, Baumart) %>%
        dplyr::mutate(tie = sum(n == max(n)) > 1) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice_min(match(Kategorie, kat_pref), n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          Kategorie = factor(Kategorie, levels = kat_lv),
          label     = ifelse(tie, paste0(n, "*"), as.character(n)),
          txt_col   = ifelse(as.character(Kategorie) %in% dunkel, "white", "grey15"))

      # Sortierung GEWICHTET (dunkelgrün zählt am meisten): Summe der Stufe je
      # Zeile (TV×Methode) bzw. Baumart. Stufe = sehr empfohlen (max) … nicht
      # empfohlen (1), grau/pBv (keine Stufe) = 0. Aufsteigend -> beste (höchste
      # Summe) als letzter Faktor-Level -> beste Zeile oben, beste Baumart rechts.
      gew <- d_st %>%
        dplyr::filter(Gruppe == grp) %>%
        dplyr::mutate(w = dplyr::coalesce(as.numeric(Stufe), 0))
      tv_ord <- gew %>% dplyr::group_by(TV_M) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, TV_M)
      ba_ord <- gew %>% dplyr::group_by(Baumart) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, Baumart)
      # Mit order_ref: feste Referenz-Reihenfolge statt der je-Gruppe-Sortierung.
      tv_lv <- if (!is.null(ref_lv)) union(ref_lv$tv, as.character(tv_ord$TV_M)) else as.character(tv_ord$TV_M)
      ba_lv <- if (!is.null(ref_lv)) union(ref_lv$ba, as.character(ba_ord$Baumart)) else as.character(ba_ord$Baumart)

      kachel <- kachel %>%
        dplyr::mutate(
          TV_M    = factor(as.character(TV_M),    levels = tv_lv),
          Baumart = factor(as.character(Baumart), levels = ba_lv))

      # Leere (sonst WEISSE) Zellen als "keine Einschätzung möglich" auffüllen ->
      # lückenlose, hellgraue Matrix statt weißer Löcher. Nur bei grau_zusammen;
      # sonst bleiben leere Zellen weiß (Originalanzeige).
      if (grau_zusammen) kachel <- kachel %>%
        tidyr::complete(TV_M, Baumart) %>%
        dplyr::mutate(
          Kategorie = factor(dplyr::coalesce(as.character(Kategorie), "keine Einschätzung möglich"),
                             levels = kat_lv),
          tie       = dplyr::coalesce(tie, FALSE),
          label     = dplyr::coalesce(label, ""),
          txt_col   = dplyr::coalesce(txt_col, "grey15"))

      # Nur EIN Klimalauf in der Gruppe -> jede Kachel ist zwangsläufig "1"
      # (keine Aggregation, kein Gleichstand) -> Zahl weglassen, sie trägt nichts
      # bei. Ab 2 Klimaläufen ist die Anzahl echte Information und bleibt.
      n_laeufe <- dplyr::n_distinct(gew$Klimalauf)
      zahl_zeigen <- werte_anzeigen && n_laeufe > 1

      # Modell + enthaltene Szenarien/Zeiträume NUR aus dieser Gruppe (nicht global –
      # sonst steht z. B. bei "Zukunft" fälschlich das OBS-Modell DWD mit dabei).
      modelle_grp <- .bae_modell_str(gew)
      # Szenarien der Gruppe: Referenz (OBS) zuerst, dann RCP; OBS als "Referenz".
      sz          <- gew %>% dplyr::distinct(Szen_label, ist_rcp) %>%
        dplyr::arrange(ist_rcp, Szen_label)
      szen_grp    <- paste(sub("^OBS", "Referenz", as.character(sz$Szen_label)), collapse = "/")
      zeit_grp    <- paste(sort(unique(as.character(gew$Zeitraum))),   collapse = ", ")
      grp_info    <- switch(trennung,
                            "Zeit"      = paste0(" (", szen_grp, "; ", zeit_grp, ")"),
                            "Keine"     = paste0(" (", szen_grp, "; ", zeit_grp, ")"),
                            "Szenario"  = paste0(" (", zeit_grp, ")"),
                            "Klimalauf" = "")

      p <- ggplot2::ggplot(kachel, ggplot2::aes(x = Baumart, y = TV_M)) +
        ggplot2::geom_tile(ggplot2::aes(fill = Kategorie), color = "white", linewidth = 0.6) +
        ggplot2::geom_tile(data = dplyr::filter(kachel, tie),
                           fill = NA, color = "grey15", linewidth = 1.1) +
        { if (zahl_zeigen)
          ggplot2::geom_text(ggplot2::aes(label = label, colour = txt_col), size = 3) } +
        ggplot2::scale_fill_manual(values = .bae_palette, limits = kat_lv, drop = FALSE) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_y_discrete(labels = if (tv_letters) .bae_tv_labeller else ggplot2::waiver()) +
        ggplot2::labs(
          title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
          subtitle = paste0(sub("st$", "", st), "-stufig  |  ", grp, grp_info, "  |  Modell: ", modelle_grp),
          x = "Baumart  (beste Empfehlungen →)",
          y = "TV × Methode  (meiste Empfehlungen oben ↑)",
          fill = "häufigste Kategorie") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1,
                                                  face = "bold", size = 9),
          axis.text.y     = ggplot2::element_text(face = "bold", size = 9),
          panel.grid      = ggplot2::element_blank(),
          legend.position = legend_pos,
          plot.background = ggplot2::element_rect(fill = "white", color = NA))

      n_ba <- length(levels(kachel$Baumart))
      n_tv <- length(levels(kachel$TV_M))
      grp_tag <- gsub("[^A-Za-z0-9]+", "-", grp)
      f <- file.path(mid_dir, paste0("ModusMatrix_", st, "_", grp_tag, "_",
                                     master_id, "_", modelle_grp, ord_tag, ".png"))
      ggplot2::ggsave(f, plot = p, device = "png",
                      width  = 700 + n_ba * 95,
                      height = 500 + n_tv * 95,
                      units = "px", dpi = 150, limitsize = FALSE)
      message("Gespeichert: ", f)
      plots[[paste0(st, "_", grp_tag)]] <- p
    }
  }
  invisible(plots)
}

# ----  4  Bequemer Wrapper: beide Grafiken erzeugen ----

#' Beide Auswertungsgrafiken (Konsens + Modus-Matrix) nacheinander erzeugen
#'
#' Ruft bae_konsens_function() und bae_modus_matrix_function() mit denselben
#' Argumenten auf. Parameter siehe dort.
#'
#' @param data,master_id,stufen,rcp_zukunft_ab,obs_alle,szen_rename,szenarien,out_dir
#'   wie bei bae_konsens_function().
#' @param trennung,hinweis_row,facet,facet_ncol,legend_pos,order_ref wie bei
#'   bae_modus_matrix_function().
#' @return unsichtbar list(konsens = ..., matrix = ...) der ggplot-Objekte.
bae_auswertung_grafiken <- function(data, master_id,
                                    stufen         = c("3st", "4st", "5st", "2st"),
                                    trennung       = c("Klimalauf", "Zeit", "Szenario", "Keine"),
                                    rcp_zukunft_ab = 2021,
                                    obs_alle       = TRUE,
                                    szen_rename    = character(0),
                                    szenarien      = c("OBS", "RCP45", "RCP85"),
                                    hinweis_row    = .bae_hinweis_row,
                                    facet          = FALSE,
                                    facet_ncol     = NULL,
                                    legend_pos     = "right",
                                    order_ref      = NULL,
                                    grau_zusammen  = TRUE,
                                    tv_letters     = TRUE,
                                    out_dir        = "04_results/BAE_Auswertung/auswertung") {
  trennung <- match.arg(trennung)
  konsens <- bae_konsens_function(data, master_id, stufen = stufen, trennung = trennung,
                                  rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
                                  szen_rename = szen_rename, szenarien = szenarien,
                                  grau_zusammen = grau_zusammen, out_dir = out_dir)
  matrix <- bae_modus_matrix_function(data, master_id, stufen = stufen, trennung = trennung,
                                      rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
                                      szen_rename = szen_rename, szenarien = szenarien,
                                      hinweis_row = hinweis_row,
                                      facet = facet, facet_ncol = facet_ncol,
                                      legend_pos = legend_pos, order_ref = order_ref,
                                      grau_zusammen = grau_zusammen, tv_letters = tv_letters,
                                      out_dir = out_dir)
  invisible(list(konsens = konsens, matrix = matrix))
}

# ----  5  Facet-Paar: zwei Stufen nebeneinander (z. B. binär + 4-stufig) ----

#' Zwei facettierte Modus-Matrizen nebeneinander in EINE Grafik
#'
#' Erzeugt die facettierte Modus-Matrix für zwei Stufen (links/rechts, Default
#' binär + 4-stufig) und legt sie mit patchwork unter einer GEMEINSAMEN
#' Überschrift zusammen. Die beiden Einzelgrafiken werden dabei wie gewohnt AUCH
#' einzeln als PNG gespeichert (bleiben also erhalten).
#'
#' @param stufen_paar Länge-2-Vektor c(links, rechts); Default c("2st", "4st").
#' @param order_ref   Referenz-Stufe für die GEMEINSAME Achsen-Sortierung beider
#'                    Seiten (Default "2st" – robusteste Abdeckung, da manche
#'                    Standorte 3st/4st gar nicht haben; "4st"/"3st" ebenfalls
#'                    möglich). So liegen dieselbe TV-Zeile / Baumart-Spalte links
#'                    wie rechts an gleicher Stelle.
#' @param facet_ncol,legend_pos,trennung,rcp_zukunft_ab,obs_alle,szen_rename,
#'   szenarien,hinweis_row,werte_anzeigen,out_dir wie bei bae_modus_matrix_function().
#' @return unsichtbar das kombinierte patchwork-Objekt (Nebeneffekt: PNGs).
bae_modus_facet_paar <- function(data, master_id,
                                 stufen_paar    = c("2st", "4st"),
                                 order_ref      = "2st",
                                 trennung       = c("Klimalauf", "Zeit", "Szenario", "Keine"),
                                 rcp_zukunft_ab = 2021,
                                 obs_alle       = TRUE,
                                 szen_rename    = character(0),
                                 szenarien      = c("OBS", "RCP45", "RCP85"),
                                 hinweis_row    = .bae_hinweis_row,
                                 werte_anzeigen = TRUE,
                                 facet_ncol     = 1,
                                 legend_pos     = "bottom",
                                 grau_zusammen  = TRUE,
                                 tv_letters     = TRUE,
                                 out_dir        = "04_results/BAE_Auswertung/auswertung") {
  trennung <- match.arg(trennung)
  stopifnot(length(stufen_paar) == 2)
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    message("Paket 'patchwork' fehlt: install.packages(\"patchwork\").")
    return(invisible(NULL))
  }

  # Beide Seiten als facettierte Einzel-Modus-Matrix erzeugen. order_ref gibt für
  # BEIDE dieselbe Achsen-Sortierung -> direkt vergleichbar. Nebeneffekt: sie
  # werden dabei AUCH einzeln als PNG gespeichert -> Einzelgrafiken bleiben.
  args <- list(data = data, master_id = master_id, trennung = trennung,
               rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
               szen_rename = szen_rename, szenarien = szenarien,
               hinweis_row = hinweis_row, werte_anzeigen = werte_anzeigen,
               facet = TRUE, facet_ncol = facet_ncol, legend_pos = legend_pos,
               order_ref = order_ref, grau_zusammen = grau_zusammen,
               tv_letters = tv_letters, out_dir = out_dir)
  pl <- do.call(bae_modus_matrix_function, c(args, list(stufen = stufen_paar[1])))
  pr <- do.call(bae_modus_matrix_function, c(args, list(stufen = stufen_paar[2])))

  gl <- pl[[paste0(stufen_paar[1], "_facet")]]
  gr <- pr[[paste0(stufen_paar[2], "_facet")]]
  if (is.null(gl) || is.null(gr)) {
    message("Facet-Paar: mindestens eine Seite ohne Daten – übersprungen.")
    return(invisible(NULL))
  }

  # kurze Seiten-Titel statt der langen Einzel-Titel; gemeinsame Überschrift oben
  lab_l <- paste0(sub("st$", "", stufen_paar[1]), "-stufig")
  lab_r <- paste0(sub("st$", "", stufen_paar[2]), "-stufig")

  gl <- gl + ggplot2::labs(title = lab_l, subtitle = NULL)
  gr <- gr + ggplot2::labs(title = lab_r, subtitle = NULL)

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  modelle_str <- if (is.null(d0)) "NA" else .bae_modell_str(d0)

  # Größere Schrift für die große Kombigrafik: Streifen (Gruppen), Seiten-Titel
  # (binär/4-stufig), Achsen und Überschrift – sonst auf Seitengröße zu klein.
  groesser <- ggplot2::theme(
    plot.title   = ggplot2::element_text(size = 25, face = "bold"),
    strip.text   = ggplot2::element_text(size = 25, face = "bold"),
    axis.title   = ggplot2::element_text(size = 25),
    axis.text.y  = ggplot2::element_text(size = 25, face = "bold"),
    axis.text.x  = ggplot2::element_text(size = 25, face = "bold", angle = 45, hjust = 1),
    legend.text  = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 25))

  # Legende in 2 Zeilen umbrechen, sonst laeuft die 5-teilige 4st-Legende
  # (nicht empfohlen / maessig / empfohlen / sehr / keine Einschätzung möglich)
  # bei der grossen Schrift ueber den rechten Rand hinaus.
  comb <- patchwork::wrap_plots(gl, gr, ncol = 2) & groesser &
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
  comb <- comb +
    patchwork::plot_annotation(
      title    = paste0("BAE – häufigste Empfehlung – ", master_id),
      subtitle = paste0("links: ", lab_l, "  |  rechts: ", lab_r,
                        "  |  Modell: ", modelle_str),
      theme = ggplot2::theme(
        plot.title      = ggplot2::element_text(size = 24, face = "bold"),
        plot.subtitle   = ggplot2::element_text(size = 16),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)))

  # Größe aus dem linken Plot ableiten (beide Seiten teilen dieselben Achsen)
  n_ba  <- nlevels(droplevels(gl$data$Baumart))
  n_tv  <- nlevels(droplevels(gl$data$TV_M))
  n_grp <- nlevels(droplevels(gl$data$Gruppe))
  fc    <- if (!is.null(facet_ncol)) facet_ncol else ceiling(sqrt(n_grp))
  fr    <- ceiling(n_grp / fc)
  w1    <- (500 + n_ba * 95) * fc + 200
  h1    <- (400 + n_tv * 95) * fr + 200

  mid_dir <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)
  ord_tag <- if (!is.null(order_ref)) paste0("_", order_ref, "ord") else ""
  f <- file.path(mid_dir, paste0("ModusMatrix_facetpaar_", stufen_paar[1], "-",
                                 stufen_paar[2], "_", master_id, "_", modelle_str,
                                 ord_tag, ".png"))
  ggplot2::ggsave(f, plot = comb, device = "png",
                  width = 2 * w1, height = h1 + 250, units = "px",
                  dpi = 150, limitsize = FALSE)
  message("Gespeichert: ", f)
  invisible(comb)
}

# ----  5b  Facet-Paar Konsens: zwei Stufen übereinander (z. B. 4-stufig + binär) ----

#' Zwei Konsens-Balken-Grafiken (zwei Stufen) untereinander in EINE Grafik
#'
#' Erzeugt die Konsens-Balken für zwei Stufen (oben/unten, Default 4-stufig +
#' binär) und legt sie mit patchwork unter einer GEMEINSAMEN Überschrift
#' zusammen – untereinander, wie im Screenshot (oben 4-stufig, unten 2-stufig).
#' Der beschreibende Untertitel je Stufe ("… -stufig | je Klimalauf | Anteil der
#' TVs …") bleibt erhalten, damit jede Stufe ihre eigene Legende erklärt (4st hat
#' 6 Kategorien, 2st nur 2 – die Legenden lassen sich nicht zusammenlegen). Die
#' beiden Einzelgrafiken werden dabei wie gewohnt AUCH einzeln als PNG
#' gespeichert (bleiben also erhalten).
#'
#' @param stufen_paar Länge-2-Vektor c(oben, unten); Default c("4st", "2st").
#' @param order_ref   Referenz-Stufe für die gemeinsame Baumart-Sortierung beider
#'                    Grafiken (Default = obere Stufe stufen_paar[1]). Sortiert
#'                    wird PRO FACETTE (je Klimalauf/Zeit/Szenario eigene
#'                    Reihenfolge); order_ref sorgt dafür, dass die untere
#'                    (binäre) Grafik je Facette DIESELBE Reihenfolge hat wie die
#'                    obere – nur binär eingefärbt. NULL = jede Stufe sortiert je
#'                    Facette selbst (dann können obere/untere abweichen).
#' @param facet_ncol,trennung,rcp_zukunft_ab,obs_alle,szen_rename,szenarien,out_dir
#'   wie bei bae_konsens_function(). facet_ncol hier Default 3 (drei Panels je
#'   Zeile: OBS / RCP45 / RCP85 wie im Screenshot).
#' @return unsichtbar das kombinierte patchwork-Objekt (Nebeneffekt: PNGs).
bae_konsens_facet_paar <- function(data, master_id,
                                   stufen_paar    = c("4st", "2st"),
                                   order_ref      = stufen_paar[1],
                                   trennung       = c("Klimalauf", "Zeit", "Szenario", "Keine"),
                                   rcp_zukunft_ab = 2021,
                                   obs_alle       = TRUE,
                                   szen_rename    = character(0),
                                   szenarien      = c("OBS", "RCP45", "RCP85"),
                                   facet_ncol     = 3,
                                   grau_zusammen  = TRUE,
                                   out_dir        = "04_results/BAE_Auswertung/auswertung") {
  trennung <- match.arg(trennung)
  stopifnot(length(stufen_paar) == 2)
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    message("Paket 'patchwork' fehlt: install.packages(\"patchwork\").")
    return(invisible(NULL))
  }

  # Beide Stufen als Konsens-Balken erzeugen. order_ref gibt für BEIDE dieselbe
  # Baumart-Reihenfolge (Default = obere Stufe) -> untere x-Achse == obere. Neben-
  # effekt: sie werden dabei AUCH einzeln als PNG gespeichert -> Einzel bleiben.
  args <- list(data = data, master_id = master_id, trennung = trennung,
               rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
               szen_rename = szen_rename, szenarien = szenarien,
               facet_ncol = facet_ncol, order_ref = order_ref,
               grau_zusammen = grau_zusammen, out_dir = out_dir)
  po <- do.call(bae_konsens_function, c(args, list(stufen = stufen_paar[1])))
  pu <- do.call(bae_konsens_function, c(args, list(stufen = stufen_paar[2])))

  go <- po[[paste0(stufen_paar[1], "_balken")]]
  gu <- pu[[paste0(stufen_paar[2], "_balken")]]
  if (is.null(go) || is.null(gu)) {
    message("Facet-Paar Konsens: mindestens eine Stufe ohne Daten – übersprungen.")
    return(invisible(NULL))
  }

  # Der Einzel-Titel ("BAE – Konsens der TVs …") stünde sonst dreimal (je Stufe +
  # gemeinsame Überschrift). Deshalb je Stufe nur den Untertitel behalten und die
  # gemeinsame Überschrift EINMAL oben über plot_annotation setzen.
  go <- go + ggplot2::labs(title = NULL)
  gu <- gu + ggplot2::labs(title = NULL)

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  modelle_str <- if (is.null(d0)) "NA" else .bae_modell_str(d0)

  # zwei Stufen UNTEREINANDER (ncol = 1), gemeinsame Überschrift oben.
  comb <- patchwork::wrap_plots(go, gu, ncol = 1) +
    patchwork::plot_annotation(
      title = paste0("BAE – Konsens der TVs je Baumart – ", master_id,
                     "   (oben: ", sub("st$", "", stufen_paar[1]), "-stufig, unten: ",
                     sub("st$", "", stufen_paar[2]), "-stufig  |  Modell: ", modelle_str, ")"),
      theme = ggplot2::theme(
        plot.title      = ggplot2::element_text(size = 20, face = "bold"),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)))

  # Größe: Breite wie Einzelgrafik, Höhe ~ zwei Einzelgrafiken + Überschrift.
  # Gruppenzahl aus dem oberen Plot (Gruppe ist dort Faktor über alle Gruppen).
  n_grp    <- nlevels(droplevels(go$data$Gruppe))
  fac_ncol <- if (!is.null(facet_ncol)) facet_ncol else ceiling(sqrt(n_grp))
  fac_nrow <- ceiling(n_grp / fac_ncol)
  breite   <- 500 + fac_ncol * 900
  hoehe    <- 400 + fac_nrow * 650

  mid_dir <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)
  ord_tag <- if (!is.null(order_ref)) paste0("_", order_ref, "ord") else ""
  f <- file.path(mid_dir, paste0("KonsensBalken_facetpaar_", stufen_paar[1], "-",
                                 stufen_paar[2], "_", master_id, "_", modelle_str,
                                 ord_tag, ".png"))
  ggplot2::ggsave(f, plot = comb, device = "png",
                  width = breite, height = 2 * hoehe + 250, units = "px",
                  dpi = 150, limitsize = FALSE)
  message("Gespeichert: ", f)
  invisible(comb)
}

# ----  6  BEISPIEL-AUFRUF (auskommentiert) ----
data <- heatmap_data_filter %>% filter(!Klimalauf %in% c("OBS_DWD_1961-1990",
                                                         "RCP45-v3_MPICLM_2021-2050",
                                                         "RCP45_MPICLM_2021-2050",
                                                         "RCP45_MPICLM_2071-2100",
                                                         #"RCP85_MPICLM_2021-2050",
                                                         "RCP45-v3_MPICLM_2071-2100")) #heatmap_data
heatmap_data_filter$Klimalauf %>% unique
data$Klimalauf %>% unique
heatmap_data_filter$MASTER_ID %>% unique

master_id.choose <- "NR_130_08_66519" # "NR_130_08_6189" #"NR_130_08_66519"
#
# Beide Grafiken je Stufe (Modus-Matrix UNAGGREGIERT je Klimalauf = Default).
# Szenarien-Default = OBS (Beobachtung 1991-2020) + RCP45 + RCP85, RCP ab 2021:
bae_auswertung_grafiken(data, master_id = master_id.choose)

# Alle Szenarien behalten (kein Szenarien-Filter):
bae_auswertung_grafiken(data, master_id = master_id.choose, szenarien = NULL)

# Nur die Konsens-Grafiken (Skizze 1: Balken + Kurve), nur 4-stufig:
bae_konsens_function(data, master_id = master_id.choose, stufen = "5st", facet_ncol = 3)
bae_konsens_function(data, master_id = master_id.choose, stufen = "4st", facet_ncol = 3)
bae_konsens_function(data, master_id = master_id.choose, stufen = "3st", facet_ncol = 3)
bae_konsens_function(data, master_id = master_id.choose, stufen = "2st",  facet_ncol = 3)

# Konsens-PAAR: zwei Stufen als EINE Grafik, oben 4-stufig, unten binär (wie der
# Screenshot). Sortiert wird PRO FACETTE (je Klimalauf eigene Reihenfolge,
# bestempfohlene rechts); order_ref = obere Stufe (Default) sorgt dafür, dass die
# untere Grafik je Facette dieselbe Reihenfolge hat wie die obere, nur binär
# eingefärbt. Die beiden Einzel-Balken werden dabei auch separat gespeichert:
bae_konsens_facet_paar(data, master_id = master_id.choose,
                       stufen_paar = c("4st", "2st"),
                       szenarien = c("OBS", "RCP45", "RCP85"),
                       trennung = "Klimalauf", facet_ncol = 3)

# andere Paare/Reihenfolgen möglich, z. B. 3-stufig oben, binär unten:
bae_konsens_facet_paar(data, master_id = master_id.choose,
                       stufen_paar = c("3st", "2st"),
                       szenarien = c("OBS", "RCP45", "RCP85"),
                       trennung = "Klimalauf", facet_ncol = 3)

# Referenz-Sortierung frei wählbar (z. B. beide nach der binären Achse):
bae_konsens_facet_paar(data, master_id = master_id.choose,
                       stufen_paar = c("4st", "2st"), order_ref = "2st",
                       szenarien = c("OBS", "RCP45", "RCP85"),
                       trennung = "Klimalauf", facet_ncol = 3)

# Nur die binäre Stufe (aus 3st: Code 3 = nicht empfohlen, sonst empfohlen):
bae_modus_matrix_function(data, master_id = master_id.choose, stufen = "2st")
bae_modus_matrix_function(data, master_id = master_id.choose, stufen = "3st")
bae_modus_matrix_function(data, master_id = master_id.choose, stufen = "4st")
bae_modus_matrix_function(data, master_id = master_id.choose, stufen = "5st")

# Modus-Matrix (Skizze 2) über Klimaläufe gezählt, Vergangenheit vs. Zukunft:
bae_modus_matrix_function(data, master_id = master_id.choose,
                          trennung = "Zeit")

# Modus-Matrix als EINE facettierte Grafik (alle Gruppen nebeneinander,
# wie die Kurvengrafik) statt einzelner PNGs:
bae_modus_matrix_function(data, master_id = master_id.choose,
                          trennung = "Zeit", facet = TRUE)

# Facet 1-spaltig, 3 Zeilen (Referenz oben, dann RCP85 2021-2050 & 2071-2100),
# Legende unten – nur OBS + RCP85, je Klimalauf ein Panel:

# bae_modus_matrix_function(data, master_id = master_id.choose, stufen = "4st",
#                           szenarien = c("OBS", "RCP85"), trennung = "Klimalauf",
#                           facet = TRUE, facet_ncol = 2, legend_pos = "bottom")

# Facet-PAAR: links binär, rechts 4-stufig unter EINER Überschrift, GEMEINSAME
# Achsen-Sortierung nach binär (order_ref, robusteste Abdeckung) -> beide Seiten
# direkt vergleichbar (die beiden Einzel-Facets werden dabei auch separat gespeichert):
bae_modus_facet_paar(data, master_id = master_id.choose,
                     stufen_paar = c("2st", "4st"), order_ref = "2st",
                     szenarien = c("OBS", "RCP45","RCP85"), #"RCP45",
                     trennung = "Klimalauf", facet_ncol = 1, legend_pos = "bottom")

# Original-Anzeige erzwingen: echte TV-Namen statt A,B,C UND pBv / Keine
# Datengrundlage getrennt (schwarz/grau, leere Zellen bleiben weiß):
bae_modus_facet_paar(data, master_id = master_id.choose,
                     stufen_paar = c("2st", "4st"), order_ref = "2st",
                     szenarien = c("OBS", "RCP45","RCP85"),
                     trennung = "Klimalauf", facet_ncol = 1, legend_pos = "bottom",
                     grau_zusammen = FALSE, tv_letters = FALSE)

bae_modus_facet_paar(data, master_id = master_id.choose,
                     stufen_paar = c("2st", "4st"), order_ref = "3st",
                     szenarien = c("OBS", "RCP45", "RCP85"),
                     trennung = "Klimalauf", facet_ncol = 1, legend_pos = "bottom")


bae_modus_facet_paar(data, master_id = master_id.choose,
                     stufen_paar = c("2st", "4st"), order_ref = "4st",
                     szenarien = c("OBS", "RCP45", "RCP85"),
                     trennung = "Klimalauf", facet_ncol = 1, legend_pos = "bottom")

# Feste Achsen-Sortierung (nach binär) auch für einzelne Matrizen erzwingen:
bae_modus_matrix_function(data, master_id = master_id.choose,
                          stufen = c("2st", "4st"), order_ref = "2st")

bae_modus_matrix_function(data, master_id = master_id.choose,
                          stufen = c("2st", "4st"), order_ref = "3st")

bae_modus_matrix_function(data, master_id = master_id.choose,
                          stufen = c("2st", "4st"), order_ref = "4st")

# Modus-Matrix ungetrennt (alles in einer Matrix), Labels umbenennen:
bae_modus_matrix_function(
  data, master_id = master_id.choose, trennung = "Keine",
  szen_rename = c("OBS" = "Referenz", "RCP45-v3" = "RCP45_real"))
