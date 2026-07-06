# ============================================================================
# bae_auswertung_grafiken_standalone.R
# ----------------------------------------------------------------------------
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
#   2) bae_score_matrix_function() -> "Gezählte Einträge" (Skizze 2)
#      Eine über alle Klimaläufe aggregierte, DATENGETRIEBEN SORTIERTE Matrix
#      TV × Baumart. Je Kategorie ein Rangwert (sehr empfohlen = hoch …
#      nicht empfohlen = niedrig); die Einträge werden gezählt und gewichtet
#      summiert (= "Score"). Bei 3st gelten nur die 3 Kategorien, bei 4st die
#      4, bei 5st die 5 – höhere/nicht bewertete Werte (pBv, Keine
#      Datengrundlage) zählen nicht mit.
#        * Zeilen (TV):     nach Gesamt-Score sortiert -> meiste/beste oben
#        * Spalten (Baumart): nach Gesamt-Score sortiert -> höchste rechts
#        * Farbe:           rot (links/unten) -> orange (Mitte) -> grün
#                           (oben rechts), skaliert nach Score.
#      Datei: ScoreMatrix_<st>_<MID>_<Modell>.png
#
#   bae_auswertung_grafiken() ruft beide nacheinander auf.
#
# Datengrundlage/Spalten identisch zu heatmap_bae_zukunft_standalone.R:
#   MASTER_ID, Baumart, TV, Klimalauf, BAE_3ST, BAE_4ST, BAE_5ST
# `Klimalauf` z. B. "OBS_DWD_1991-2020", "RCP45_MPICLM_2071-2100",
#                   "RCP45-v3_MPICLM_2021-2050", "RCP85_MPICLM_2071-2100_v2".
# Stufen-Mappings und Farbpalette sind mit dem Heatmap-Skript konsistent.
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ============================================================================
#  GEMEINSAME KONSTANTEN (konsistent mit heatmap_bae_zukunft_standalone.R)
# ============================================================================

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
.bae_maps <- list(
  "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
  "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "nicht empfohlen"),
  "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "wenig empfohlen", "5" = "nicht empfohlen")
)
.bae_col <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST")

# Kategorien je Stufe von "schlecht" (Stufe 1, unten/rot) nach "gut"
# (Stufe n, oben/grün). Index in diesem Vektor = numerische Empfehlungsstufe
# UND Rangwert (Gewicht) für den Score. Damit gilt automatisch:
#   Stufe / Rangwert = (n_Stufen + 1) - Code.
.bae_kat_order <- list(
  "3st" = c("nicht empfohlen", "mäßig empfohlen", "sehr empfohlen"),
  "4st" = c("nicht empfohlen", "mäßig empfohlen", "empfohlen", "sehr empfohlen"),
  "5st" = c("nicht empfohlen", "wenig empfohlen", "mäßig empfohlen",
            "empfohlen", "sehr empfohlen")
)

# Farben für die (bis zu 12) TV-Linien in Skizze 1
.bae_tv_colors <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#1F78B4", "#B2182B", "#33A02C", "#6A3D9A"
)

# Farbverlauf Score-Matrix: rot -> orange -> gelb -> hellgrün -> grün
.bae_score_gradient <- c("#A50026", "#FDAE61", "#FEE08B", "#A6D96A", "#1A9850")

# Code -> Kategorie (nicht-numerische Werte -> pBv / Keine Datengrundlage)
.bae_map_val <- function(val, mapping) {
  val <- as.character(val)
  dplyr::case_when(
    val %in% names(mapping) ~ unname(mapping[val]),
    val == "pBv"            ~ "pBv",
    TRUE                    ~ "Keine Datengrundlage"
  )
}

# ----------------------------------------------------------------------------
#  Gemeinsame Aufbereitung: filtern auf MASTER_ID, Klimalauf zerlegen,
#  RCP-Zukunft/OBS filtern. Liefert das aufbereitete data.frame `d`
#  (ohne die stufenabhängige Kategorie/Stufe – die wird pro Stufe ergänzt).
# ----------------------------------------------------------------------------
.bae_prep <- function(data, master_id, rcp_zukunft_ab = 2021,
                      obs_alle = TRUE, szen_rename = character(0)) {

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
      Stufe     = unname(lvl_map[Kategorie])   # NA für pBv / Keine Datengrundlage
    )
}

.bae_modell_str <- function(d) {
  m <- sort(unique(as.character(d$Modell)))
  m <- m[!is.na(m) & nzchar(m)]
  if (length(m)) paste(m, collapse = "-") else "NA"
}

# ============================================================================
#  SKIZZE 1 – Empfehlungs-Kurven
# ============================================================================
bae_kurven_function <- function(data,
                                master_id,
                                stufen         = c("3st", "4st", "5st"),
                                rcp_zukunft_ab = 2021,
                                obs_alle       = TRUE,
                                szen_rename    = character(0),
                                out_dir        = "04_results/BAE_Auswertung/auswertung") {

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename)
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
    tv_lv   <- levels(droplevels(d_st$TV))
    tv_cols <- setNames(.bae_tv_colors[seq_along(tv_lv)], tv_lv)

    p <- ggplot2::ggplot(
        d_st,
        ggplot2::aes(x = Baumart, y = Stufe, group = TV, colour = TV)) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.8, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.6, alpha = 0.9, na.rm = TRUE) +
      ggplot2::facet_grid(Szen_label ~ Zeitraum) +
      ggplot2::scale_y_continuous(
        breaks = seq_along(ordn), labels = ordn,
        limits = c(1, length(ordn)), expand = ggplot2::expansion(mult = 0.05)) +
      ggplot2::scale_colour_manual(values = tv_cols, drop = FALSE) +
      ggplot2::labs(
        title    = paste0("BAE-Empfehlungskurven – ", master_id),
        subtitle = paste0(st, "-stufig  |  Modell: ", modelle_str,
                          "  |  Kurven je TV; Überlappung = Einigkeit der TVs"),
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

# ============================================================================
#  SKIZZE 2 – Gezählte Einträge (sortierte Score-Matrix)
# ============================================================================
bae_score_matrix_function <- function(data,
                                      master_id,
                                      stufen         = c("3st", "4st", "5st"),
                                      trennung       = c("zeit", "szenario", "keine"),
                                      rcp_zukunft_ab = 2021,
                                      obs_alle       = TRUE,
                                      szen_rename    = character(0),
                                      werte_anzeigen = TRUE,     # Score je Zelle beschriften
                                      out_dir        = "04_results/BAE_Auswertung/auswertung") {

  # trennung: getrennte, JEWEILS EIGEN SORTIERTE Matrizen (eine PNG je Gruppe)
  #   "zeit"     -> Vergangenheit (OBS) vs. Zukunft (RCP)   [Default]
  #   "szenario" -> je Szen_label eine Matrix (OBS, RCP45, RCP45_v3, RCP85, …)
  #   "keine"    -> eine gemeinsame Matrix über alles
  trennung <- match.arg(trennung)

  d0 <- .bae_prep(data, master_id, rcp_zukunft_ab, obs_alle, szen_rename)
  if (is.null(d0)) return(invisible(NULL))

  d0 <- d0 %>%
    dplyr::mutate(Gruppe = switch(trennung,
      "zeit"     = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
      "szenario" = as.character(Szen_label),
      "keine"    = "alle"))

  modelle_str <- .bae_modell_str(d0)
  mid_dir     <- file.path(out_dir, as.character(master_id))
  dir.create(mid_dir, showWarnings = FALSE, recursive = TRUE)

  # "Vergangenheit" vor "Zukunft"; sonst alphabetisch
  gruppen <- sort(unique(d0$Gruppe))

  plots <- list()
  for (st in stufen) {
    d_st <- .bae_add_stufe(d0, st)
    if (is.null(d_st)) {
      message("Spalte für Stufe '", st, "' nicht vorhanden – übersprungen.")
      next
    }

    for (grp in gruppen) {
      # Gewichteter Score je TV×Baumart innerhalb der Gruppe: Einträge
      # durchzählen und mit dem Rangwert der Kategorie (Stufe) gewichtet
      # aufsummieren. Nur die für die Stufigkeit gültigen Kategorien zählen
      # (Stufe = NA -> raus).
      agg <- d_st %>%
        dplyr::filter(Gruppe == grp, !is.na(Stufe)) %>%
        dplyr::group_by(TV, Baumart) %>%
        dplyr::summarise(Score = sum(Stufe), N = dplyr::n(), .groups = "drop") %>%
        droplevels()

      if (nrow(agg) == 0) {
        message("Stufe '", st, "', Gruppe '", grp,
                "': keine bewerteten Einträge – übersprungen.")
        next
      }

      # Sortierung: aufsteigend nach Gesamt-Score -> höchste als letzter
      # Faktor-Level. In ggplot heißt das: y oben, x rechts. Jede Gruppe wird
      # eigenständig sortiert.
      tv_ord <- agg %>% dplyr::group_by(TV) %>%
        dplyr::summarise(s = sum(Score), .groups = "drop") %>%
        dplyr::arrange(s, TV)
      ba_ord <- agg %>% dplyr::group_by(Baumart) %>%
        dplyr::summarise(s = sum(Score), .groups = "drop") %>%
        dplyr::arrange(s, Baumart)

      agg <- agg %>%
        dplyr::mutate(
          TV      = factor(as.character(TV),      levels = as.character(tv_ord$TV)),
          Baumart = factor(as.character(Baumart), levels = as.character(ba_ord$Baumart)))

      p <- ggplot2::ggplot(agg, ggplot2::aes(x = Baumart, y = TV, fill = Score)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        { if (werte_anzeigen)
            ggplot2::geom_text(ggplot2::aes(label = Score), size = 3,
                               colour = "grey15") } +
        ggplot2::scale_fill_gradientn(colours = .bae_score_gradient) +
        ggplot2::labs(
          title    = paste0("BAE – gezählte Einträge (Score) – ", master_id),
          subtitle = paste0(st, "-stufig  |  ", grp, "  |  Modell: ", modelle_str,
                            "  |  TV nach Eintr. (oben), Baumart nach Empf. (rechts)"),
          x = "Baumart  (höchste Empfehlungen →)",
          y = "TV  (meiste Einträge ↑)",
          fill = "Score") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1,
                                                  face = "bold", size = 9),
          axis.text.y     = ggplot2::element_text(face = "bold", size = 9),
          panel.grid      = ggplot2::element_blank(),
          legend.position = "right",
          plot.background = ggplot2::element_rect(fill = "white", color = NA))

      n_ba <- length(levels(agg$Baumart))
      n_tv <- length(levels(agg$TV))
      grp_tag <- gsub("[^A-Za-z0-9]+", "-", grp)
      f <- file.path(mid_dir, paste0("ScoreMatrix_", st, "_", grp_tag, "_",
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

# ============================================================================
#  Bequemer Wrapper: beide Grafiken erzeugen
# ============================================================================
bae_auswertung_grafiken <- function(data, master_id,
                                    stufen         = c("3st", "4st", "5st"),
                                    trennung       = c("zeit", "szenario", "keine"),
                                    rcp_zukunft_ab = 2021,
                                    obs_alle       = TRUE,
                                    szen_rename    = character(0),
                                    out_dir        = "04_results/BAE_Auswertung/auswertung") {
  trennung <- match.arg(trennung)
  kurven <- bae_kurven_function(data, master_id, stufen, rcp_zukunft_ab,
                                obs_alle, szen_rename, out_dir)
  matrix <- bae_score_matrix_function(data, master_id, stufen, trennung,
                                      rcp_zukunft_ab, obs_alle, szen_rename,
                                      out_dir = out_dir)
  invisible(list(kurven = kurven, matrix = matrix))
}

# ============================================================================
# BEISPIEL-AUFRUF (auskommentiert)
# ----------------------------------------------------------------------------
# data <- data.table::fread("meine_bae_daten.csv")
#
# # Beide Grafiken je Stufe (Score-Matrix getrennt nach Vergangenheit/Zukunft):
# bae_auswertung_grafiken(data, master_id = "NR_130_08_66519")
#
# # Nur die Kurven (Skizze 1), nur 4-stufig:
# bae_kurven_function(data, master_id = "NR_130_08_66519", stufen = "4st")
#
# # Score-Matrix (Skizze 2) je Szenario statt nur Vergangenheit/Zukunft:
# bae_score_matrix_function(data, master_id = "NR_130_08_66519",
#                           trennung = "szenario")
#
# # Score-Matrix ungetrennt (alles in einer Matrix), Labels umbenennen:
# bae_score_matrix_function(
#   data, master_id = "NR_130_08_66519", trennung = "keine",
#   szen_rename = c("OBS" = "Referenz", "RCP45_v3" = "RCP45_real"))
# ============================================================================
