# =============================================================================
# Walther-Lieth-Klimadiagramme: Eingangsdaten aus den Monats-Klimatologien
# bauen (Parameter-IDs 1155 = Monatsmitteltemperatur 30a,
#                      1157 = mittlerer Monatsniederschlag 30a)
#
# Beide Produkte liegen als je 12 Layer (Jan..Dez) pro Klimalauf vor.
# 1155 = ymonmean(monmean(1112))  -> Temperatur: Mittel  (korrekt)
# 1157 = ymonmean(monsum (1114))  -> Niederschlag: Summe-dann-Mittel (korrekt)
#
# Output: Long-Format  id | Zeitlauf | Monat | T_mean | P_sum
# =============================================================================

# ---- Hilfsfunktion: Skalierungs-Plausibilitaet -----------------------------
# Die Roh-Tagesdaten (1112/1114) liegen als Integer x10 vor und werden ueblich
# mit mulc,0.1 in echte Einheiten gebracht. Wurden 1155/1157 daraus korrekt
# abgeleitet, sind die Werte bereits in degC bzw. mm. Diese Funktion prueft das
# per Plausibilitaet und gibt den anzuwendenden Faktor zurueck (1 oder 0.1).
.wl_detect_scale <- function(temp_vals, scale = NULL) {
  if (!is.null(scale)) return(scale)
  m <- stats::median(abs(temp_vals), na.rm = TRUE)
  if (is.finite(m) && m > 60) {
    warning("Temperaturwerte wirken um Faktor 10 zu gross (Median |T| = ",
            round(m, 1), "). Wende Faktor 0.1 an. ",
            "Mit scale = 1 bzw. scale = 0.1 explizit ueberschreiben.",
            call. = FALSE)
    return(0.1)
  }
  1
}

# ---- Extraktion fuer EINEN Lauf --------------------------------------------
#' Monats-Klimatologie eines Laufs in ein breites data.frame ueberfuehren.
#'
#' @param r        SpatRaster mit 24 Layern (12x 1155 Temp + 12x 1157 Nied.)
#'                 ODER eine Liste/Named-list mit zwei SpatRastern (temp, prec).
#' @param geom     optional: data.frame mit Punkt-Metadaten/Geometrie, das
#'                 zeilengleich (cellweise) an die Werte gebunden wird
#'                 (z.B. cbind aus nc.BWI.rw.df etc.). NULL = nur Werte.
#' @param scale    NULL = automatische Plausibilitaetspruefung; sonst 1 oder 0.1.
#' @param temp_idx,prec_idx  Layer-Indizes falls r ein kombinierter 24-Layer-
#'                 Stack ist (Default: 1:12 = Temp, 13:24 = Niederschlag).
#' @return data.frame mit Spalten T_01..T_12, P_01..P_12 (+ geom-Spalten).
wl_extract_run <- function(r, geom = NULL, scale = NULL,
                           temp_idx = 1:12, prec_idx = 13:24) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Paket 'terra' wird benoetigt.")

  # Zwei getrennte Raster (temp, prec) zu einem 24-Layer-Stack zusammenfuehren
  if (is.list(r) && !inherits(r, "SpatRaster")) {
    if (length(r) != 2)
      stop("Liste muss genau zwei SpatRaster enthalten (Temp, Niederschlag).")
    r_t <- r[[1]]; r_p <- r[[2]]
    temp_idx <- 1:12; prec_idx <- 13:24
    r <- c(r_t, r_p)
  }

  if (terra::nlyr(r) < max(c(temp_idx, prec_idx)))
    stop("SpatRaster hat ", terra::nlyr(r),
         " Layer, erwartet werden mindestens 24 (12 Temp + 12 Niederschlag).")

  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  df  <- dplyr::as_tibble(terra::as.data.frame(r, xy = FALSE))
  fac <- .wl_detect_scale(as.matrix(df[, temp_idx]), scale)

  out <- dplyr::bind_cols(
    df[, temp_idx] %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x * fac)) %>%
      stats::setNames(sprintf("T_%02d", 1:12)),
    df[, prec_idx] %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x * fac)) %>%
      stats::setNames(sprintf("P_%02d", 1:12))
  )

  if (!is.null(geom)) {
    if (nrow(geom) != nrow(out))
      stop("geom hat ", nrow(geom), " Zeilen, Werte haben ", nrow(out),
           " - cellweises cbind nicht moeglich.")
    out <- dplyr::bind_cols(dplyr::as_tibble(geom), out)
  }
  out
}

# ---- Aufbau ueber alle Laeufe ----------------------------------------------
#' Walther-Lieth-Eingang fuer mehrere Klimalaeufe im Long-Format bauen.
#'
#' @param rast_list  benannte Liste; Name = Zeitlauf (z.B. "RCP45_MPIWRF_2071-2100"),
#'                   Element = 24-Layer-SpatRaster ODER list(temp, prec).
#' @param geom       optional gemeinsame Geometrie/Metadaten (gilt fuer alle Laeufe).
#'                   Muss Spalte 'id' enthalten, damit Punkte identifizierbar sind.
#' @param scale      NULL (auto) oder 1 / 0.1.
#' @param id_cols    Metadaten-Spalten aus geom, die ins Long-Format mitgenommen
#'                   werden (Default: alle Spalten von geom).
#' @return tibble/data.frame: <id_cols...> | Zeitlauf | Monat | T_mean | P_sum
build_walther_lieth_input <- function(rast_list, geom = NULL, scale = NULL,
                                      id_cols = NULL) {
  if (is.null(names(rast_list)) || any(names(rast_list) == ""))
    stop("rast_list muss benannt sein (Name = Zeitlauf).")
  for (pkg in c("tidyr", "dplyr", "purrr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Paket '", pkg, "' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  if (is.null(id_cols)) id_cols <- if (!is.null(geom)) names(geom) else character(0)

  wide_all <- rast_list %>%
    purrr::imap(function(r, nm) {
      wl_extract_run(r, geom = geom, scale = scale) %>%
        dplyr::mutate(Zeitlauf = nm) %>%
        # Falls keine id vorhanden: Zellindex als Ersatz-id
        { if (!"id" %in% names(.)) dplyr::mutate(., id = dplyr::row_number())
          else . }
    }) %>%
    purrr::list_rbind()

  keep <- unique(c(id_cols, "id", "Zeitlauf"))
  keep <- keep[keep %in% names(wide_all)]

  long <- wide_all %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("^[TP]_[0-9]{2}$"),
      names_to  = c("Groesse", "Monat"),
      names_pattern = "([TP])_([0-9]{2})",
      values_to = "Wert"
    ) %>%
    dplyr::mutate(Monat = as.integer(.data$Monat)) %>%
    tidyr::pivot_wider(names_from = "Groesse", values_from = "Wert") %>%
    dplyr::rename(T_mean = "T", P_sum = "P") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keep)), .data$Monat)

  long
}
