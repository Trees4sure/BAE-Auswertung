# ----  bae_auswertung_grafiken_standalone.R  ----
#
# Eigenständiges Skript (UNABHÄNGIG von der Shiny-App). Verdichtet die
# BAE-Heatmap zu zwei übersichtlicheren Auswertungsgrafiken pro
# Bewertungsstufe und MASTER_ID:
#
#   1) bae_kurven_function()      -> "Empfehlungs-Kurven" (Skizze 1)
#      Pro Szenario×Zeitraum-Panel (wie die Heatmap) eine Linie je TV über
#      die Baumarten. y-Achse = Empfehlungsstufe (unten "nicht empfohlen",
#      oben "sehr empfohlen"). Wo die TV-Kurven zusammenfallen, sind sich
#      die TVs einig; wo sie auseinanderlaufen, sind sie uneinig.
#      Datei: Kurven_<st>_<MID>_<Modell>.png
#
#   2) bae_modus_matrix_function() -> "Häufigste Empfehlung" (Skizze 2)
#      DATENGETRIEBEN SORTIERTE Matrix (Zeile TV × Methode) × Baumart. KEIN Score
#      – es wird nur AUSGEZÄHLT: je Zelle werden die Empfehlungskategorien über
#      alle Einträge der Gruppe gezählt. Die Methode (Spalte Hinweis) wird zur
#      eigenen Zeile: TV6 (KHorg) / TV6 (KHff) / TV6 (KM) sind echte, getrennte
#      Rechnungen; AltBA/BAE20/WKE sind baumart-spezifisch und werden in die
#      Standardzeile "TVx" zusammengelegt (steuerbar über hinweis_row).
#      Die Kachel zeigt die HÄUFIGSTE Kategorie (Modus):
#        * Farbe = häufigste Kategorie (custom_palette, diskret; pBv / Keine
#                  Datengrundlage erscheinen als graue Kacheln)
#        * Zahl  = ABSOLUTE Anzahl dieser häufigsten Kategorie (je Klimalauf
#                  meist 1; > 1 erst, wenn eine Gruppe mehrere Klimaläufe zählt).
#                  Bei nur EINEM Klimalauf in der Gruppe (alles zwangsläufig 1)
#                  wird die Zahl NICHT gedruckt – sie trägt dort keine Info.
#        * Gleichstand: die BESSERE Kategorie wird gezeigt und mit "*" am Wert
#                       sowie einem Rahmen um die Kachel markiert.
#      Standard (trennung = "klimalauf"): eine Matrix je Klimalauf, unaggregiert
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
  "nicht empfohlen"       = "#A50026",
  "pBv"                   = "#404040",
  "Keine Datengrundlage"  = "#B0B0B0"
)

# Code -> Kategorie je Stufigkeit (Code 1 = beste Bewertung)
# "bin" = binär aus der 3-stufigen Spalte (BAE_3ST – bei 5st fehlen vielerorts
# die Daten): NUR der schlechteste Code 3 = "nicht empfohlen", die übrigen
# gültigen Codes (1-2) = "empfohlen". pBv / leer / NA fallen (wie bei den
# anderen Stufen) auf pBv bzw. Keine Datengrundlage.
.bae_maps <- list(
  "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
  "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "nicht empfohlen"),
  "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "wenig empfohlen", "5" = "nicht empfohlen"),
  "bin" = c("1" = "empfohlen", "2" = "empfohlen", "3" = "nicht empfohlen")
)
.bae_col <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST",
              "bin" = "BAE_3ST")

# Kategorien je Stufe von "schlecht" (Stufe 1, unten/rot) nach "gut"
# (Stufe n, oben/grün). Index in diesem Vektor = numerische Empfehlungsstufe
# UND Rangwert (Gewicht) für den Score. Damit gilt automatisch:
#   Stufe / Rangwert = (n_Stufen + 1) - Code.
.bae_kat_order <- list(
  "3st" = c("nicht empfohlen", "mäßig empfohlen", "sehr empfohlen"),
  "4st" = c("nicht empfohlen", "mäßig empfohlen", "empfohlen", "sehr empfohlen"),
  "5st" = c("nicht empfohlen", "wenig empfohlen", "mäßig empfohlen",
            "empfohlen", "sehr empfohlen"),
  "bin" = c("nicht empfohlen", "empfohlen")
)

# Farben für die (bis zu 12) TV-Linien in Skizze 1
.bae_tv_colors <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#1F78B4", "#B2182B", "#33A02C", "#6A3D9A"
)

# Code -> Kategorie (nicht-numerische Werte -> pBv / Keine Datengrundlage)
.bae_map_val <- function(val, mapping) {
  val <- as.character(val)
  dplyr::case_when(
    val %in% names(mapping) ~ unname(mapping[val]),
    val == "pBv"            ~ "pBv",
    TRUE                    ~ "Keine Datengrundlage"
  )
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

# ----  2  SKIZZE 1 – Empfehlungs-Kurven ----

#' Empfehlungs-Kurven je TV über die Baumarten (Skizze 1)
#'
#' @param data          data.frame mit MASTER_ID, Baumart, TV, Klimalauf sowie
#'                       BAE_3ST / BAE_4ST / BAE_5ST (wie im Heatmap-Skript).
#' @param master_id     ID des Standorts, auf den gefiltert wird.
#' @param stufen        Bewertungsstufen, je eine PNG: "3st","4st","5st","bin".
#' @param rcp_zukunft_ab RCP-Läufe erst ab diesem Startjahr behalten (Default 2021).
#' @param obs_alle       TRUE = OBS-Läufe unabhängig vom Zeitraum behalten.
#' @param szen_rename    benannter Vektor c("<intern>" = "<Anzeige>") zum
#'                       Umbenennen der Szenario-Labels (optional).
#' @param szenarien      zu behaltende Basis-Szenarien (Default OBS+RCP45+RCP85);
#'                       NULL = alle Szenarien.
#' @param out_dir        Ausgabeordner; je MASTER_ID entsteht ein Unterordner.
#' @return unsichtbar eine Liste der ggplot-Objekte je Stufe (Nebeneffekt: PNGs).
bae_kurven_function <- function(data,
                                master_id,
                                stufen         = c("3st", "4st", "5st", "bin"),
                                rcp_zukunft_ab = 2021,
                                obs_alle       = TRUE,
                                szen_rename    = character(0),
                                szenarien      = c("OBS", "RCP45", "RCP85"),
                                out_dir        = "04_results/BAE_Auswertung/auswertung") {

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename, szenarien)
  if (is.null(d0)) return(invisible(NULL))

  modelle_str <- .bae_modell_str(d0)
  mid_dir     <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  n_spalten <- length(levels(droplevels(d0$Zeitraum)))
  n_zeilen  <- length(levels(droplevels(d0$Szen_label)))

  plots <- list()
  for (st in stufen) {
    d_st <- .bae_add_stufe(d0, st)
    if (is.null(d_st)) {
      message("Spalte für Stufe '", st, "' nicht vorhanden – übersprungen.")
      next
    }

    ordn    <- .bae_kat_order[[st]]
    ba_lv   <- levels(droplevels(d_st$Baumart))
    tv_lv   <- levels(droplevels(d_st$TV))
    tv_cols <- setNames(.bae_tv_colors[seq_along(tv_lv)], tv_lv)

    # 1) ein Wert je (Panel, TV, Baumart): Mittel der Stufe über die Hinweis-
    #    Varianten (sonst mehrere y an einer x-Position -> vertikale Zacken).
    kurv <- d_st %>%
      dplyr::group_by(Szen_label, Zeitraum, TV, Baumart) %>%
      dplyr::summarise(y = mean(Stufe, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.nan(y)) %>%
      dplyr::mutate(x = as.integer(factor(Baumart, levels = ba_lv)))

    # 2) glatte Spline-Kurve je (Panel, TV) durch diese Punkte, auf [1,n]
    #    geklammert, damit sie nicht über die Kategorien hinausschwingt.
    kurv_smooth <- kurv %>%
      dplyr::group_by(Szen_label, Zeitraum, TV) %>%
      dplyr::filter(dplyr::n() >= 2) %>%
      dplyr::group_modify(~ {
        s <- stats::spline(.x$x, .x$y, n = 200)
        data.frame(x = s$x, y = pmin(pmax(s$y, 1), length(ordn)))
      }) %>%
      dplyr::ungroup()

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = kurv_smooth,
                         ggplot2::aes(x = x, y = y, colour = TV, group = TV),
                         linewidth = 0.8, alpha = 0.85) +
      ggplot2::geom_point(data = kurv,
                          ggplot2::aes(x = x, y = y, colour = TV),
                          size = 1.4, alpha = 0.9) +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::scale_x_continuous(breaks = seq_along(ba_lv), labels = ba_lv) +
      ggplot2::scale_y_continuous(
        breaks = seq_along(ordn), labels = ordn,
        limits = c(1, length(ordn)), expand = ggplot2::expansion(mult = 0.05)) +
      ggplot2::scale_colour_manual(values = tv_cols, drop = FALSE) +
      ggplot2::labs(
        title    = paste0("BAE-Empfehlungskurven – ", master_id),
        subtitle = paste0(st, "-stufig  |  Modell: ", modelle_str,
                          "  |  Kurven je TV (Mittel über Hinweis-Varianten); Überlappung = Einigkeit"),
        x = NULL, y = "Empfehlung", colour = "TV") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        strip.text      = ggplot2::element_text(face = "bold", size = 9),
        strip.background = ggplot2::element_rect(fill = "grey95", color = "grey70",
                                                 linewidth = 0.6),
        panel.border    = ggplot2::element_rect(color = "grey80", fill = NA,
                                                linewidth = 0.5),
        axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "grey92"),
        legend.position = "bottom",
        plot.background = ggplot2::element_rect(fill = "white", color = NA))

    f <- file.path(mid_dir, paste0("Kurven_", st, "_", master_id, "_",
                                   modelle_str, ".png"))
    ggplot2::ggsave(f, plot = p, device = "png",
                    width  = 1200 + n_spalten * 850,
                    height =  650 + n_zeilen  * 480,
                    units = "px", dpi = 150, limitsize = FALSE)
    message("Gespeichert: ", f)
    plots[[st]] <- p
  }
  invisible(plots)
}

# ----  3  SKIZZE 2 – Häufigste Empfehlung (ausgezählte, sortierte Matrix) ----

#' Häufigste Empfehlung als ausgezählte, gewichtet sortierte Matrix (Skizze 2)
#'
#' @param data          data.frame wie bei bae_kurven_function (zusätzlich
#'                       optional Spalte Hinweis = Rechenmethode).
#' @param master_id     ID des Standorts, auf den gefiltert wird.
#' @param stufen        Bewertungsstufen: "3st","4st","5st","bin".
#' @param trennung      Gruppierung der Matrizen: "klimalauf" (je Klimalauf, Default),
#'                       "zeit" (Vergangenheit vs. Zukunft), "szenario", "keine".
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
#' @param out_dir        Ausgabeordner; je MASTER_ID entsteht ein Unterordner.
#' @return unsichtbar eine Liste der ggplot-Objekte (Nebeneffekt: PNGs).
bae_modus_matrix_function <- function(data,
                                      master_id,
                                      stufen         = c("3st", "4st", "5st", "bin"),
                                      trennung       = c("klimalauf", "zeit", "szenario", "keine"),
                                      rcp_zukunft_ab = 2021,
                                      obs_alle       = TRUE,
                                      szen_rename    = character(0),
                                      szenarien      = c("OBS", "RCP45", "RCP85"),
                                      hinweis_row    = .bae_hinweis_row,
                                      werte_anzeigen = TRUE,     # Anzahl je Zelle beschriften
                                      facet          = FALSE,    # TRUE: alle Gruppen in EINE facettierte Grafik
                                      out_dir        = "04_results/BAE_Auswertung/auswertung") {

  # trennung: getrennte, JEWEILS EIGEN SORTIERTE Matrizen (eine PNG je Gruppe)
  #   "klimalauf" -> UNAGGREGIERT, je Klimalauf eine Matrix (wie die Heatmap-
  #                  Panels). Pro Zelle 1 Methode -> Zahl ist hier meist 1. [Default]
  #   "zeit"      -> Vergangenheit (OBS) vs. Zukunft (RCP), über Klimaläufe gezählt
  #   "szenario"  -> je Szen_label eine Matrix (gezählt über die Zeiträume)
  #   "keine"     -> eine gemeinsame Matrix über alles
  # Die Zahl je Kachel wird erst > 1, wenn eine Gruppe mehrere Klimaläufe zählt
  # (z. B. trennung = "zeit"/"keine"): dann = in wie vielen die Kategorie vorkam.
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
        "klimalauf" = as.character(Klimalauf),
        "zeit"      = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
        "szenario"  = as.character(Szen_label),
        "keine"     = "alle"))

  modelle_str <- .bae_modell_str(d0)
  mid_dir     <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  gruppen <- sort(unique(d0$Gruppe))   # alphabetisch: OBS… vor RCP…, chronologisch

  plots <- list()
  for (st in stufen) {
    d_st <- .bae_add_stufe(d0, st)
    if (is.null(d_st)) {
      message("Spalte für Stufe '", st, "' nicht vorhanden – übersprungen.")
      next
    }

    ordn     <- .bae_kat_order[[st]]                        # schlecht -> gut
    kat_lv   <- c(ordn, "pBv", "Keine Datengrundlage")      # Legenden-/Fill-Reihenfolge
    kat_pref <- c(rev(ordn), "pBv", "Keine Datengrundlage") # best -> schlecht (Gleichstand: bessere gewinnt)
    dunkel   <- c("sehr empfohlen", "nicht empfohlen", "pBv")      # Kacheln mit weißer Schrift

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
      gew <- d_st %>% dplyr::mutate(w = dplyr::coalesce(as.numeric(Stufe), 0))
      tv_ord <- gew %>% dplyr::group_by(TV_M) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, TV_M)
      ba_ord <- gew %>% dplyr::group_by(Baumart) %>%
        dplyr::summarise(s = sum(w), .groups = "drop") %>%
        dplyr::arrange(s, Baumart)

      kachel <- kachel %>%
        dplyr::mutate(
          TV_M    = factor(as.character(TV_M),    levels = as.character(tv_ord$TV_M)),
          Baumart = factor(as.character(Baumart), levels = as.character(ba_ord$Baumart)),
          Gruppe  = factor(as.character(Gruppe),  levels = gruppen))

      # Zahl nur zeigen, wenn mind. eine Gruppe mehrere Klimaläufe zusammenfasst
      # (bei trennung = "klimalauf" hat jede Gruppe genau 1 Lauf -> alles 1 -> weg).
      laeufe_je_grp <- d_st %>% dplyr::distinct(Gruppe, Klimalauf) %>%
        dplyr::count(Gruppe)
      zahl_zeigen   <- werte_anzeigen && any(laeufe_je_grp$n > 1)

      n_ba     <- length(levels(kachel$Baumart))
      n_tv     <- length(levels(kachel$TV_M))
      n_grp    <- length(gruppen)
      fac_ncol <- ceiling(sqrt(n_grp))
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
        ggplot2::labs(
          title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
          subtitle = paste0(st, "-stufig  |  facettiert je Gruppe (", trennung,
                            ")  |  Modell: ", modelle_str,
                            "  |  global gewichtet sortiert: beste Zeile oben, beste Baumart rechts",
                            if (zahl_zeigen)
                              "  |  Zahl = Anzahl; * / Rahmen = Gleichstand (bessere gezeigt)"
                            else ""),
          x = "Baumart  (beste Empfehlungen →)",
          y = "TV × Methode  (beste oben ↑)",
          fill = "häufigste Kategorie") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          strip.text       = ggplot2::element_text(face = "bold", size = 9),
          strip.background = ggplot2::element_rect(fill = "grey95", color = "grey70",
                                                   linewidth = 0.6),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                   face = "bold", size = 9),
          axis.text.y      = ggplot2::element_text(face = "bold", size = 9),
          panel.grid       = ggplot2::element_blank(),
          legend.position  = "right",
          plot.background  = ggplot2::element_rect(fill = "white", color = NA))

      f <- file.path(mid_dir, paste0("ModusMatrix_facet_", st, "_",
                                     master_id, "_", modelle_str, ".png"))
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

      kachel <- kachel %>%
        dplyr::mutate(
          TV_M    = factor(as.character(TV_M),    levels = as.character(tv_ord$TV_M)),
          Baumart = factor(as.character(Baumart), levels = as.character(ba_ord$Baumart)))

      # Nur EIN Klimalauf in der Gruppe -> jede Kachel ist zwangsläufig "1"
      # (keine Aggregation, kein Gleichstand) -> Zahl weglassen, sie trägt nichts
      # bei. Ab 2 Klimaläufen ist die Anzahl echte Information und bleibt.
      n_laeufe <- dplyr::n_distinct(gew$Klimalauf)
      zahl_zeigen <- werte_anzeigen && n_laeufe > 1

      p <- ggplot2::ggplot(kachel, ggplot2::aes(x = Baumart, y = TV_M)) +
        ggplot2::geom_tile(ggplot2::aes(fill = Kategorie), color = "white", linewidth = 0.6) +
        ggplot2::geom_tile(data = dplyr::filter(kachel, tie),
                           fill = NA, color = "grey15", linewidth = 1.1) +
        { if (zahl_zeigen)
            ggplot2::geom_text(ggplot2::aes(label = label, colour = txt_col), size = 3) } +
        ggplot2::scale_fill_manual(values = .bae_palette, limits = kat_lv, drop = FALSE) +
        ggplot2::scale_colour_identity() +
        ggplot2::labs(
          title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
          subtitle = paste0(st, "-stufig  |  ", grp, "  |  Modell: ", modelle_str,
                            "  |  gewichtet sortiert: beste Zeile oben, beste Baumart rechts",
                            if (zahl_zeigen)
                              "  |  Zahl = Anzahl; * / Rahmen = Gleichstand (bessere gezeigt)"
                            else ""),
          x = "Baumart  (beste Empfehlungen →)",
          y = "TV × Methode  (beste oben ↑)",
          fill = "häufigste Kategorie") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1,
                                                  face = "bold", size = 9),
          axis.text.y     = ggplot2::element_text(face = "bold", size = 9),
          panel.grid      = ggplot2::element_blank(),
          legend.position = "right",
          plot.background = ggplot2::element_rect(fill = "white", color = NA))

      n_ba <- length(levels(kachel$Baumart))
      n_tv <- length(levels(kachel$TV_M))
      grp_tag <- gsub("[^A-Za-z0-9]+", "-", grp)
      f <- file.path(mid_dir, paste0("ModusMatrix_", st, "_", grp_tag, "_",
                                     master_id, "_", modelle_str, ".png"))
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

#' Beide Auswertungsgrafiken (Kurven + Modus-Matrix) nacheinander erzeugen
#'
#' Ruft bae_kurven_function() und bae_modus_matrix_function() mit denselben
#' Argumenten auf. Parameter siehe dort.
#'
#' @param data,master_id,stufen,rcp_zukunft_ab,obs_alle,szen_rename,szenarien,out_dir
#'   wie bei bae_kurven_function().
#' @param trennung,hinweis_row,facet wie bei bae_modus_matrix_function().
#' @return unsichtbar list(kurven = ..., matrix = ...) der ggplot-Objekte.
bae_auswertung_grafiken <- function(data, master_id,
                                    stufen         = c("3st", "4st", "5st", "bin"),
                                    trennung       = c("klimalauf", "zeit", "szenario", "keine"),
                                    rcp_zukunft_ab = 2021,
                                    obs_alle       = TRUE,
                                    szen_rename    = character(0),
                                    szenarien      = c("OBS", "RCP45", "RCP85"),
                                    hinweis_row    = .bae_hinweis_row,
                                    facet          = FALSE,
                                    out_dir        = "04_results/BAE_Auswertung/auswertung") {
  trennung <- match.arg(trennung)
  kurven <- bae_kurven_function(data, master_id, stufen = stufen,
                                rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
                                szen_rename = szen_rename, szenarien = szenarien,
                                out_dir = out_dir)
  matrix <- bae_modus_matrix_function(data, master_id, stufen = stufen, trennung = trennung,
                                      rcp_zukunft_ab = rcp_zukunft_ab, obs_alle = obs_alle,
                                      szen_rename = szen_rename, szenarien = szenarien,
                                      hinweis_row = hinweis_row,
                                      facet = facet, out_dir = out_dir)
  invisible(list(kurven = kurven, matrix = matrix))
}

# ----  5  BEISPIEL-AUFRUF (auskommentiert) ----
# data <- data.table::fread("meine_bae_daten.csv")
#
# # Beide Grafiken je Stufe (Modus-Matrix UNAGGREGIERT je Klimalauf = Default).
# # Szenarien-Default = OBS (Beobachtung 1991-2020) + RCP45 + RCP85, RCP ab 2021:
# bae_auswertung_grafiken(data, master_id = "NR_130_08_66519")
#
# # Alle Szenarien behalten (kein Szenarien-Filter):
# bae_auswertung_grafiken(data, master_id = "NR_130_08_66519", szenarien = NULL)
#
# # Nur die Kurven (Skizze 1), nur 4-stufig:
# bae_kurven_function(data, master_id = "NR_130_08_66519", stufen = "4st")
#
# # Nur die binäre Stufe (aus 3st: Code 3 = nicht empfohlen, sonst empfohlen):
# bae_modus_matrix_function(data, master_id = "NR_130_08_66519", stufen = "bin")
#
# # Modus-Matrix (Skizze 2) über Klimaläufe gezählt, Vergangenheit vs. Zukunft:
# bae_modus_matrix_function(data, master_id = "NR_130_08_66519",
#                           trennung = "zeit")
#
# # Modus-Matrix als EINE facettierte Grafik (alle Gruppen nebeneinander,
# # wie die Kurvengrafik) statt einzelner PNGs:
# bae_modus_matrix_function(data, master_id = "NR_130_08_66519",
#                           trennung = "zeit", facet = TRUE)
#
# # Modus-Matrix ungetrennt (alles in einer Matrix), Labels umbenennen:
# bae_modus_matrix_function(
#   data, master_id = "NR_130_08_66519", trennung = "keine",
#   szen_rename = c("OBS" = "Referenz", "RCP45_v3" = "RCP45_real"))
