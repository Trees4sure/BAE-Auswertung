# =============================================================================
# Klima-Wolken-Diagramm (climate space): MAT (x) gegen MAP (y)
#
# Streudiagramm ueber ALLE BWI-BZE-Punkte Deutschlands fuer einen Klimalauf
# (Default OBS_DWD_1991-2020). Drei Ebenen, von hinten nach vorne:
#   1. alle Punkte                       -> dunkelgrau (Hintergrund-"Wolke")
#   2. ein ausgewaehltes Bundesland (BL) -> hellgrau
#   3. ausgewaehlte MASTER_IDs           -> rot (mit Beschriftung)
#
# Bundesland UND MASTER_ID(s) sind frei waehlbar; per Default ist das
# Bundesland "MV" hervorgehoben. Das Diagramm zeigt die Klima-Nische eines
# Standorts (oder einer Auswahl) im Vergleich zur deutschlandweiten Punktwolke.
#
# Erwartetes Eingangs-data.frame: eine Zeile je BWI-BZE-Punkt (je Klimalauf),
# mit den Spalten (Namen anpassbar ueber die *_col-Argumente):
#   MASTER_ID | BL | Zeitlauf | MAT | MAP
#   - MAT = Jahresmitteltemperatur [degC] (mean annual temperature)
#   - MAP = Jahresniederschlagssumme [mm] (mean annual precipitation)
#   - BL  = Bundesland-Kuerzel (z.B. "MV", "BY", ...)
#   - Zeitlauf = Klimalauf-Name (z.B. "OBS_DWD_1991-2020")
# Fehlt eine Zeitlauf-Spalte, wird angenommen, dass df schon den gewuenschten
# Lauf enthaelt (run = NULL setzen, dann wird nicht gefiltert).
#
# Bruecke zum bestehenden WaLi-Trend-Long-Format (id|Zeitlauf|T_year|P_year):
# climate_space_from_wali_trend() mittelt die Jahre je Punkt zu MAT/MAP.
#
# Erwartet geladen: walther_lieth_helpers.R (COL_TEMP fuer das Rot).
# =============================================================================


# ---- Farben der drei Ebenen -------------------------------------------------
COL_CLOUD_ALL <- "grey30"   # dunkelgrau: alle BWI-BZE-Punkte (DE)
COL_CLOUD_BL  <- "grey75"   # hellgrau:   ausgewaehltes Bundesland
# Rot fuer die Auswahl kommt aus walther_lieth_helpers.R (COL_TEMP = "#c0392b");
# Fallback, falls die Helfer nicht geladen sind:
if (!exists("COL_TEMP")) COL_TEMP <- "#c0392b"


# ---- Hauptfunktion ----------------------------------------------------------
#' Klima-Wolken-Diagramm (MAT vs. MAP) mit hervorgehobenem Bundesland + Auswahl.
#'
#' @param df          data.frame mit einer Zeile je Punkt (je Lauf), siehe Kopf.
#' @param master_ids  Vektor der hervorzuhebenden MASTER_IDs (rot). NULL = keine.
#' @param highlight_bl  Bundesland-Kuerzel, das hellgrau hervorgehoben wird
#'                    (Default "MV"). NULL = keine Bundesland-Ebene.
#' @param run         Klimalauf, auf den gefiltert wird (Default
#'                    "OBS_DWD_1991-2020"). NULL = nicht filtern (df gilt as-is).
#' @param label_ids   TRUE: die MASTER_IDs der Auswahl beschriften (ggrepel,
#'                    falls vorhanden, sonst geom_text).
#' @param mat_col,map_col,bl_col,id_col,run_col  Spaltennamen im df.
#' @return ggplot-Objekt (mit print() zeichnen, mit ggplot2::ggsave() speichern).
plot_climate_space <- function(df,
                               master_ids   = NULL,
                               highlight_bl = "MV",
                               run          = "OBS_DWD_1991-2020",
                               label_ids    = TRUE,
                               mat_col = "MAT", map_col = "MAP",
                               bl_col  = "BL",  id_col  = "MASTER_ID",
                               run_col = "Zeitlauf") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Paket 'ggplot2' wird benoetigt.")

  # --- Pflichtspalten pruefen ------------------------------------------------
  need <- c(mat_col, map_col)
  if (!is.null(highlight_bl)) need <- c(need, bl_col)
  if (!is.null(master_ids))   need <- c(need, id_col)
  fehlt <- setdiff(need, names(df))
  if (length(fehlt))
    stop("Spalten fehlen im data.frame: ", paste(fehlt, collapse = ", "),
         ". Spaltennamen ggf. ueber die *_col-Argumente anpassen.")

  # --- auf den gewuenschten Klimalauf filtern --------------------------------
  if (!is.null(run)) {
    if (!run_col %in% names(df)) {
      warning("Spalte '", run_col, "' nicht vorhanden - es wird nicht auf ",
              "run='", run, "' gefiltert (df wird unveraendert verwendet).",
              call. = FALSE)
    } else {
      df <- df[!is.na(df[[run_col]]) & df[[run_col]] == run, , drop = FALSE]
      if (nrow(df) == 0)
        stop("Keine Zeilen fuer run='", run, "' in Spalte '", run_col, "'.")
    }
  }

  # --- gueltige (nicht-NA) Klimawerte --------------------------------------
  df <- df[is.finite(df[[mat_col]]) & is.finite(df[[map_col]]), , drop = FALSE]
  if (nrow(df) == 0) stop("Keine gueltigen MAT/MAP-Werte vorhanden.")

  # --- einheitliche Arbeits-Spalten (x = MAT, y = MAP) -----------------------
  base <- data.frame(MAT = df[[mat_col]], MAP = df[[map_col]])
  if (!is.null(highlight_bl)) base$BL <- as.character(df[[bl_col]])
  if (!is.null(master_ids))   base$MID <- df[[id_col]]

  # Legenden-Labels
  lab_all <- sprintf("alle BWI-BZE-Punkte (DE, n=%d)", nrow(base))

  # --- Ebene 2: ausgewaehltes Bundesland -------------------------------------
  bl_df <- NULL; lab_bl <- NULL
  if (!is.null(highlight_bl)) {
    bl_df <- base[!is.na(base$BL) & base$BL == highlight_bl, , drop = FALSE]
    if (nrow(bl_df) == 0)
      warning("Keine Punkte fuer Bundesland '", highlight_bl, "' gefunden.",
              call. = FALSE)
    lab_bl <- sprintf("Bundesland %s (n=%d)", highlight_bl, nrow(bl_df))
  }

  # --- Ebene 3: ausgewaehlte MASTER_IDs --------------------------------------
  sel_df <- NULL; lab_sel <- NULL
  if (!is.null(master_ids)) {
    sel_df <- base[base$MID %in% master_ids, , drop = FALSE]
    if (nrow(sel_df) == 0)
      warning("Keine der MASTER_IDs (", paste(master_ids, collapse = ", "),
              ") im Lauf gefunden.", call. = FALSE)
    fehlt_id <- setdiff(master_ids, sel_df$MID)
    if (length(fehlt_id))
      warning("MASTER_IDs ohne Treffer: ", paste(fehlt_id, collapse = ", "),
              call. = FALSE)
    lab_sel <- sprintf("Auswahl (MASTER_ID, n=%d)", nrow(sel_df))
  }

  # --- Farb-/Reihenfolge-Skala (Legende) -------------------------------------
  lvls <- c(lab_all, lab_bl, lab_sel)
  vals <- c(COL_CLOUD_ALL, COL_CLOUD_BL, COL_TEMP)
  keep <- !vapply(list(lab_all, lab_bl, lab_sel), is.null, logical(1))
  lvls <- lvls[keep]; vals <- vals[keep]
  names(vals) <- lvls

  # --- Plot von hinten nach vorne aufbauen -----------------------------------
  p <- ggplot2::ggplot()

  # Ebene 1: alle Punkte (dunkelgrau)
  p <- p + ggplot2::geom_point(
    data = base,
    ggplot2::aes(MAT, MAP, colour = lab_all),
    size = 0.6, alpha = 0.35)

  # Ebene 2: Bundesland (hellgrau)
  if (!is.null(bl_df) && nrow(bl_df) > 0)
    p <- p + ggplot2::geom_point(
      data = bl_df,
      ggplot2::aes(MAT, MAP, colour = lab_bl),
      size = 0.9, alpha = 0.8)

  # Ebene 3: Auswahl (rot, gross, mit weisser Kontur)
  if (!is.null(sel_df) && nrow(sel_df) > 0) {
    p <- p + ggplot2::geom_point(
      data = sel_df,
      ggplot2::aes(MAT, MAP, colour = lab_sel),
      size = 3, stroke = 0.8, shape = 21, fill = COL_TEMP, colour = "white")
    # zweiter, unsichtbarer Layer nur fuer die Legende (mappt die Farbe)
    p <- p + ggplot2::geom_point(
      data = sel_df,
      ggplot2::aes(MAT, MAP, colour = lab_sel),
      size = 0, alpha = 0)

    if (isTRUE(label_ids)) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          data = sel_df, ggplot2::aes(MAT, MAP, label = MID),
          colour = COL_TEMP, size = 3, min.segment.length = 0,
          box.padding = 0.5, seed = 1)
      } else {
        p <- p + ggplot2::geom_text(
          data = sel_df, ggplot2::aes(MAT, MAP, label = MID),
          colour = COL_TEMP, size = 3, vjust = -1)
      }
    }
  }

  # --- Achsen, Legende, Stil -------------------------------------------------
  untertitel <- if (!is.null(run)) run else NULL

  p +
    ggplot2::scale_colour_manual(name = NULL, values = vals,
                                 breaks = lvls, limits = lvls) +
    ggplot2::guides(colour = ggplot2::guide_legend(
      override.aes = list(size = c(2, rep(2.5, length(lvls) - 1))[seq_along(lvls)],
                          alpha = 1))) +
    ggplot2::labs(
      title    = "Klima-Wolken-Diagramm \u00b7 MAT vs. MAP",
      subtitle = untertitel,
      x = "Jahresmitteltemperatur MAT [\u00b0C]",
      y = "Jahresniederschlag MAP [mm]") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey25"))
}


# ---- Bruecke: WaLi-Trend-Long-Format -> Punkt-MAT/MAP -----------------------
#' Aus dem WaLi-Trend-Long-Format (id|Zeitlauf|T_year|P_year) je Punkt und Lauf
#' MAT/MAP (Jahresmittel) berechnen, damit plot_climate_space() es nutzen kann.
#'
#' @param ts        data.frame wie aus build_wali_trend_input().
#' @param run       optionaler Lauf-Filter (Default: alle Laeufe behalten).
#' @param id_col    Punkt-Schluessel (Default "id"); wird zu MASTER_ID gemappt,
#'                  damit plot_climate_space() ihn direkt findet.
#' @param bl_col    optional: Spalte mit dem Bundesland im ts (falls vorhanden).
#' @return data.frame: MASTER_ID | Zeitlauf | [BL] | MAT | MAP
climate_space_from_wali_trend <- function(ts, run = NULL, id_col = "id",
                                          bl_col = "BL") {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  if (!is.null(run)) ts <- ts[ts$Zeitlauf %in% run, , drop = FALSE]

  grp <- c(id_col, "Zeitlauf")
  if (bl_col %in% names(ts)) grp <- c(grp, bl_col)

  out <- ts %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::summarise(MAT = mean(.data$T_year, na.rm = TRUE),
                     MAP = mean(.data$P_year, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::rename(MASTER_ID = dplyr::all_of(id_col))
  if (bl_col %in% names(out)) out <- dplyr::rename(out, BL = dplyr::all_of(bl_col))
  as.data.frame(out)
}
