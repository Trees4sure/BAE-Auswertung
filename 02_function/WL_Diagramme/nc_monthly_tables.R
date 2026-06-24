# =============================================================================
# Monats-Klimatologien (1155 Temp / 1157 Niederschlag) aus NetCDF in breite
# Tabellen lesen und von dort in das Walther-Lieth-Long-Format ueberfuehren.
#
#   nc.1155_function()    -> breite Temp-Tabelle (analog nc.1157_function)
#   wl_long_from_tables() -> Bruecke: breite Temp-/Niederschlags-Tabellen
#                            zusammenfuehren -> id | Zeitlauf | Monat | T_mean | P_sum
#
# Das ist der ncdf4-basierte Pfad (ohne terra). Ergebnis von
# wl_long_from_tables() ist direkt fuer plot_walther_lieth_from_long() geeignet.
# =============================================================================


# ---- 1155: Monatsmitteltemperatur (MAT, Varname i.d.R. "tadm") --------------
#' Eine 1155-NetCDF-Datei in eine breite Monatstabelle ueberfuehren.
#'
#' Spiegelt nc.1157_function (Niederschlag, "rrds"), liest aber die Temperatur.
#' Output: cell_id | x | y | <12 Monatsspalten> | name
#'
#' @param nc.file  Pfad zur 1155-NetCDF-Datei.
#' @param varname  Name der Temperatur-Variablen (Default "tadm"). Existiert sie
#'                 nicht, wird automatisch die erste 3-dimensionale Variable
#'                 (x, y, time) verwendet.
#' @param assign_global  TRUE (Default): Ergebnis zusaetzlich als nc.1155_df in
#'                 die globale Umgebung schreiben (wie im bestehenden Workflow).
#' @return data.frame im breiten Format.
nc.1155_function <- function(nc.file = nc.1155_list.files[2],   # MAT
                             varname = "tadm",
                             assign_global = TRUE) {
  library(ncdf4)
  library(dplyr)
  library(tidyr)

  # Datei laden
  nc <- nc_open(nc.file)
  on.exit(nc_close(nc), add = TRUE)   # Datei IMMER schliessen (auch bei Fehler)

  # Koordinaten extrahieren
  easting  <- ncvar_get(nc, "easting")
  northing <- ncvar_get(nc, "northing")

  # Temperatur-Variable robust waehlen: erst "tadm", sonst erste 3D-Variable
  if (!varname %in% names(nc$var)) {
    dims3 <- vapply(nc$var, function(v) v$ndims == 3, logical(1))
    if (!any(dims3))
      stop("Variable '", varname, "' nicht gefunden und keine 3D-Variable da.")
    varname <- names(nc$var)[dims3][1]
    message("Variable '", varname, "' (3D) automatisch gewaehlt.")
  }
  tadm <- ncvar_get(nc, varname)        # [x, y, time]

  # Zeit umwandeln: "days since 1970-01-01"
  zeit_raw   <- ncvar_get(nc, "time")
  zeit_unit  <- nc$dim$time$units
  startdatum <- as.Date(sub("days since ", "", zeit_unit))
  zeit       <- startdatum + zeit_raw

  # Monatsnamen (lokalisiert wie bei nc.1157_function; die Bruecke
  # wl_long_from_tables() keyt spaeter ueber die Spalten-Position, nicht den Namen)
  monat_namen <- format(zeit, "%b")

  # Grid generieren
  grid <- expand.grid(x = easting, y = northing)
  grid$cell_id <- seq_len(nrow(grid))

  # Werte je Monat extrahieren und zuweisen
  for (i in seq_along(monat_namen)) {
    matrix_monat <- tadm[ , , i]
    grid[[monat_namen[i]]] <- as.vector(matrix_monat)
  }

  out <- grid %>%
    relocate(cell_id, x, y) %>%
    arrange(cell_id) %>%
    mutate(name = basename(nc.file))

  if (assign_global) assign("nc.1155_df", out, envir = .GlobalEnv)
  out
}


# ---- Bruecke: breite Tabellen -> WL-Long-Format -----------------------------
#' Breite Temp-/Niederschlags-Monatstabellen ins Walther-Lieth-Long-Format.
#'
#' Erwartet zwei breite data.frames (z.B. aus nc.1155_function / nc.1157_function)
#' mit je 12 Monatsspalten, einer Zell-id-Spalte, optionalen Koordinaten und
#' einer Lauf-Kennung (Dateiname). Die 12 Monatsspalten werden ueber ihre
#' POSITION (1..12) gemappt -> robust gegen lokalisierte Monatsnamen.
#'
#' Der Lauf-Schluessel wird aus dem Dateinamen abgeleitet, indem die Parameter-ID
#' am Anfang (z.B. "1155_"/"1157_") und ".nc" entfernt werden, sodass 1155 und
#' 1157 desselben Laufs denselben Zeitlauf erhalten (Join-Voraussetzung).
#'
#' Skalierung: wie wl_extract_run() wird EIN Faktor (auto aus der Temperatur,
#' Median |T| > 60 -> 0.1) auf Temperatur UND Niederschlag gemeinsam angewandt.
#'
#' @param temp_df,prec_df  breite Tabellen (Temp bzw. Niederschlag).
#' @param id_col      Spalte mit der Zell-id (Default "cell_id"); wird zu 'id'.
#' @param run_col     Spalte mit der Lauf-Kennung/Dateiname (Default "name").
#' @param coord_cols  Koordinatenspalten, die nicht als Monat zaehlen (Default
#'                    c("x","y")); werden, falls vorhanden, durchgereicht.
#' @param month_cols  optional: explizite 12 Monatsspaltennamen (Reihenfolge =
#'                    Jan..Dez). NULL = automatisch alle Spalten ausser
#'                    id/run/coord, in vorhandener Reihenfolge.
#' @param run_from_name  Funktion Dateiname -> Lauf-Schluessel. NULL = Default
#'                    (Parameter-ID-Praefix und ".nc" entfernen).
#' @param scale      NULL = Auto-Plausibilitaet; sonst 1 oder 0.1 explizit.
#' @return tibble: id | Zeitlauf | Monat | T_mean | P_sum (+ Koordinaten).
wl_long_from_tables <- function(temp_df, prec_df,
                                id_col       = "cell_id",
                                run_col      = "name",
                                coord_cols   = c("x", "y"),
                                month_cols   = NULL,
                                run_from_name = NULL,
                                scale        = NULL) {
  for (pkg in c("dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Paket '", pkg, "' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  if (is.null(run_from_name))
    run_from_name <- function(x)
      sub("\\.nc$", "", sub("^[0-9]+_", "", basename(as.character(x))))

  # ein breites df -> langes df (id | Zeitlauf | Monat | <value_name>)
  pivot_one <- function(df, value_name) {
    if (!id_col  %in% names(df)) stop("Spalte '", id_col,  "' fehlt in der Tabelle.")
    if (!run_col %in% names(df)) stop("Spalte '", run_col, "' fehlt in der Tabelle.")
    mcols <- month_cols
    if (is.null(mcols))
      mcols <- setdiff(names(df), c(id_col, run_col, coord_cols))
    if (length(mcols) != 12)
      stop("Erwarte 12 Monatsspalten, gefunden ", length(mcols), " (",
           paste(mcols, collapse = ", "), "). month_cols explizit angeben.")

    n <- nrow(df)
    # unlist(df[mcols]) ist spaltenweise: Monat1 fuer alle Zeilen, dann Monat2 ...
    out <- data.frame(
      id       = rep(df[[id_col]], times = 12),
      Zeitlauf = rep(run_from_name(df[[run_col]]), times = 12),
      Monat    = rep(seq_len(12), each = n),
      stringsAsFactors = FALSE
    )
    out[[value_name]] <- unlist(df[mcols], use.names = FALSE)
    out
  }

  t_long <- pivot_one(temp_df, "T_mean")
  p_long <- pivot_one(prec_df, "P_sum")

  merged <- dplyr::inner_join(t_long, p_long,
                              by = c("id", "Zeitlauf", "Monat"))
  if (nrow(merged) == 0)
    stop("Kein gemeinsamer id/Zeitlauf/Monat zwischen Temp- und ",
         "Niederschlagstabelle. Stimmen id_col und die Lauf-Kennung ueberein?")
  if (nrow(merged) < max(nrow(t_long), nrow(p_long)))
    warning("Temp- und Niederschlagstabelle decken sich nicht vollstaendig - ",
            "es bleiben nur gemeinsame Zellen/Laeufe (inner join).", call. = FALSE)

  # Skalierungsfaktor (auto aus Temperatur), auf T und P gemeinsam anwenden
  fac <- if (!is.null(scale)) {
    scale
  } else if (exists(".wl_detect_scale", mode = "function")) {
    .wl_detect_scale(merged$T_mean, scale)
  } else {
    m <- stats::median(abs(merged$T_mean), na.rm = TRUE)
    if (is.finite(m) && m > 60) {
      warning("Temperaturwerte wirken um Faktor 10 zu gross (Median |T| = ",
              round(m, 1), "). Wende Faktor 0.1 an.", call. = FALSE)
      0.1
    } else 1
  }
  merged$T_mean <- merged$T_mean * fac
  merged$P_sum  <- merged$P_sum  * fac

  # Koordinaten (pro Zelle konstant) anhaengen, falls vorhanden
  cc <- intersect(coord_cols, names(temp_df))
  if (length(cc) > 0) {
    coords <- temp_df[!duplicated(temp_df[[id_col]]), c(id_col, cc), drop = FALSE]
    names(coords)[1] <- "id"
    merged <- dplyr::left_join(merged, coords, by = "id")
  }

  dplyr::as_tibble(merged) %>%
    dplyr::arrange(.data$id, .data$Zeitlauf, .data$Monat)
}
