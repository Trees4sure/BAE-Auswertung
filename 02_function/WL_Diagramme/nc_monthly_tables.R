# =============================================================================
# Klima-Layer-Stacks aus NetCDF in breite Tabellen lesen und ins jeweilige
# Walther-Lieth-Long-Format ueberfuehren. Zwei Produktklassen:
#
#  MONATE (12 Layer Jan..Dez)            JAHRE (~30 Layer, ein Wert je Jahr)
#   nc.1155_function()  Temp (tadm)       nc.1049_function()  Temp (tadm)
#   nc.1157_function()  Nied. (rrds)      nc.1050_function()  Nied. (rrds)
#   wl_long_from_tables()                 wali_trend_from_tables()
#     -> id|Zeitlauf|Monat|T_mean|P_sum     -> id|Zeitlauf|Jahr|Kalenderjahr|
#        (fuer plot_walther_lieth_from_long)    T_year|P_year (fuer
#                                               plot_wali_trend_from_long)
#
# Alle vier Lese-Funktionen teilen sich einen generischen Leser
# (.nc_layer_table); sie unterscheiden sich nur in Variable, Layer-Beschriftung
# (Monat vs. Jahr) und Ziel-Objektname. Das ist der ncdf4-basierte Pfad (ohne
# terra). Die echten Kalenderjahre kommen aus den NC-Zeitstempeln.
# =============================================================================


# ---- Generischer Leser: ein NetCDF-Layer-Stack -> breite Tabelle -----------
#' Eine NetCDF-Datei mit n Zeit-Layern in eine breite Tabelle ueberfuehren.
#'
#' Output: cell_id | x | y | <n Layer-Spalten> | name
#'
#' @param nc.file  Pfad zur NetCDF-Datei.
#' @param varname  Name(n) der Datenvariable - darf mehrere Kandidaten sein,
#'                 z.B. c("rrds","rain") (BWI/NR); der erste vorhandene wird
#'                 genommen. Passt keiner und gibt es genau EINE 3D-Variable
#'                 (x, y, time), wird diese automatisch verwendet.
#' @param label_fun Funktion Datum -> Spaltenlabel je Layer. Default Monatskuerzel
#'                 (\code{format(zeit, "%b")}); fuer Jahresprodukte \code{"%Y"}.
#' @param expected_n  erwartete Layer-Zahl (z.B. 12 fuer Monate); NULL = beliebig.
#' @param auto_var FALSE (Default): fehlt \code{varname}, wird mit Dateiname-Hinweis
#'                 abgebrochen (schuetzt vor stiller Daten-Verwechslung). TRUE:
#'                 ersatzweise die erste 3D-Variable nehmen (mit message()).
#' @param df_name  Name fuer die optionale globale Zuweisung (z.B. "nc.1155_df").
#' @param assign_global  TRUE (Default): Ergebnis zusaetzlich unter df_name in
#'                 die globale Umgebung schreiben (wie im bestehenden Workflow).
#' @return data.frame im breiten Format.
.nc_layer_table <- function(nc.file, varname,
                            label_fun = function(zeit) format(zeit, "%b"),
                            expected_n = NULL,
                            auto_var = FALSE,
                            df_name = NULL, assign_global = TRUE) {
  library(ncdf4)
  library(dplyr)

  datei <- basename(nc.file)   # = spaetere Spalte 'name'; in allen messages voran

  # Datei laden
  nc <- nc_open(nc.file)
  on.exit(nc_close(nc), add = TRUE)   # Datei IMMER schliessen (auch bei Fehler)

  # Koordinaten extrahieren
  easting  <- ncvar_get(nc, "easting")
  northing <- ncvar_get(nc, "northing")

  # Datenvariable bestimmen. 'varname' darf mehrere Kandidaten enthalten
  # (z.B. c("rrds","rain")) - der erste vorhandene wird genommen. Das deckt
  # regionale Benennung ab (BWI: rrds/tadm, NR: rain/temp). Fehlen alle:
  #  - genau EINE 3D-Variable (x,y,time) im File -> die IST die Datenvariable
  #    (nur anders benannt) -> automatisch nehmen.
  #  - mehrere 3D-Variablen -> nur mit auto_var=TRUE die erste, sonst Abbruch
  #    (schuetzt vor stiller Daten-Verwechslung).
  hit <- varname[varname %in% names(nc$var)]
  if (length(hit) >= 1) {
    varname <- hit[1]
  } else {
    vorhanden <- names(nc$var)
    dims3 <- vapply(nc$var, function(v) v$ndims == 3, logical(1))
    if (!any(dims3))
      stop("[", datei, "] keine der Variablen '", paste(varname, collapse = "/"),
           "' und keine 3D-Variable gefunden (vorhanden: ",
           paste(vorhanden, collapse = ", "), ").")
    if (sum(dims3) > 1 && !isTRUE(auto_var))
      stop("[", datei, "] keine der Variablen '", paste(varname, collapse = "/"),
           "' enthalten und mehrere 3D-Variablen vorhanden (",
           paste(names(nc$var)[dims3], collapse = ", "),
           "). Mit varname= oder auto_var=TRUE eindeutig waehlen.")
    alt <- names(nc$var)[dims3][1]
    message("[", datei, "] Variable(n) '", paste(varname, collapse = "/"),
            "' nicht vorhanden - '", alt,
            "' (einzige/erste 3D-Variable) automatisch gewaehlt.")
    varname <- alt
  }
  vals <- ncvar_get(nc, varname)        # [x, y, time]

  if (length(dim(vals)) != 3)
    stop("Variable '", varname, "' ist nicht 3-dimensional [x, y, time].")
  nt <- dim(vals)[3]
  if (!is.null(expected_n) && nt != expected_n)
    stop("[", datei, "] erwarte ", expected_n, " Zeit-Layer, gefunden: ", nt, ".")

  # Zeit umwandeln: "days since 1970-01-01"
  zeit_raw   <- ncvar_get(nc, "time")
  zeit_unit  <- nc$dim$time$units
  startdatum <- as.Date(sub("days since ", "", zeit_unit))
  zeit       <- startdatum + zeit_raw

  # Layer-Labels (Monatskuerzel oder Jahr). Muessen eindeutig sein, sonst
  # Fallback auf den Layer-Index. Die Bruecken keyen ueber Position/Jahr.
  labels <- label_fun(zeit)
  if (length(labels) != nt || anyDuplicated(labels)) {
    warning("[", datei, "] Zeit-Labels nicht eindeutig/passend - nutze ",
            "Layer-Index 1..", nt, ".", call. = FALSE)
    labels <- as.character(seq_len(nt))
  }

  # Grid generieren (x schnellster Index, passend zu as.vector der Matrix)
  grid <- expand.grid(x = easting, y = northing)
  grid$cell_id <- seq_len(nrow(grid))

  # Werte je Layer extrahieren und zuweisen
  for (i in seq_len(nt)) {
    grid[[labels[i]]] <- as.vector(vals[ , , i])
  }

  out <- grid %>%
    relocate(cell_id, x, y) %>%
    arrange(cell_id) %>%
    mutate(name = datei)

  # Fortschrittsmeldung (nuetzlich beim Einlesen in einer Schleife)
  message(sprintf("[%s] gelesen: Variable '%s', %d Layer, %d Zellen.",
                  datei, varname, nt, nrow(out)))

  if (assign_global && !is.null(df_name))
    assign(df_name, out, envir = .GlobalEnv)
  out
}


# ---- MONATE: 1155 Temperatur (tadm) / 1157 Niederschlag (rrds) -------------
#' Eine 1155-NetCDF-Datei in eine breite Monatstabelle ueberfuehren.
#' @inheritParams .nc_layer_table
#' @return data.frame: cell_id | x | y | <12 Monatsspalten> | name
nc.1155_function <- function(nc.file = nc.1155_list.files[2],   # MAT
                             varname = c("tadm", "temp"),       # BWI / NR
                             auto_var = FALSE,
                             assign_global = TRUE) {
  .nc_layer_table(nc.file, varname = varname,
                  label_fun = function(zeit) format(zeit, "%b"), expected_n = 12,
                  auto_var = auto_var,
                  df_name = "nc.1155_df", assign_global = assign_global)
}

#' Eine 1157-NetCDF-Datei in eine breite Monatstabelle ueberfuehren.
#' @inheritParams .nc_layer_table
#' @return data.frame: cell_id | x | y | <12 Monatsspalten> | name
nc.1157_function <- function(nc.file = nc.1157_list.files[2],   # MAP
                             varname = c("rrds", "rain"),       # BWI / NR
                             auto_var = FALSE,
                             assign_global = TRUE) {
  .nc_layer_table(nc.file, varname = varname,
                  label_fun = function(zeit) format(zeit, "%b"), expected_n = 12,
                  auto_var = auto_var,
                  df_name = "nc.1157_df", assign_global = assign_global)
}


# ---- JAHRE: 1049 Temperatur (tadm) / 1050 Niederschlag (rrds) --------------
#' Eine 1049-NetCDF-Datei (Jahresmitteltemperatur) in eine breite Tabelle.
#' Spaltenlabels sind die Kalenderjahre. @inheritParams .nc_layer_table
#' @return data.frame: cell_id | x | y | <n Jahresspalten> | name
nc.1049_function <- function(nc.file = nc.1049_list.files[2],   # MAT/Jahr
                             varname = c("tadm", "temp"),       # BWI / NR
                             auto_var = FALSE,
                             assign_global = TRUE) {
  .nc_layer_table(nc.file, varname = varname,
                  label_fun = function(zeit) format(zeit, "%Y"), expected_n = NULL,
                  auto_var = auto_var,
                  df_name = "nc.1049_df", assign_global = assign_global)
}

#' Eine 1050-NetCDF-Datei (Jahresniederschlagssumme) in eine breite Tabelle.
#' Spaltenlabels sind die Kalenderjahre. @inheritParams .nc_layer_table
#' @return data.frame: cell_id | x | y | <n Jahresspalten> | name
nc.1050_function <- function(nc.file = nc.1050_list.files[2],   # MAP/Jahr
                             varname = c("rrds", "rain"),       # BWI / NR
                             auto_var = FALSE,
                             assign_global = TRUE) {
  .nc_layer_table(nc.file, varname = varname,
                  label_fun = function(zeit) format(zeit, "%Y"), expected_n = NULL,
                  auto_var = auto_var,
                  df_name = "nc.1050_df", assign_global = assign_global)
}


# ---- intern: Lauf-Schluessel in quelle + bereinigten Zeitlauf zerlegen ------
#' "bwi-bze_OBS_DWD_1991-2020" -> list(quelle="BWI", run="OBS_DWD_1991-2020"),
#' "nr-01_OBS_DWD_1961-1990"  -> list(quelle="NR-01", run="OBS_DWD_1961-1990").
#' Ohne Quell-Praefix: quelle = NA, run = unveraendert.
.wl_split_key <- function(keys) {
  keys <- as.character(keys)
  pat  <- "^(bwi[-_]bze|nr-?[0-9]{2})_(.*)$"
  reg  <- sub(pat, "\\1", keys, ignore.case = TRUE)
  run  <- sub(pat, "\\2", keys, ignore.case = TRUE)
  nogo <- reg == keys
  quelle <- ifelse(grepl("^bwi", reg, ignore.case = TRUE), "BWI",
                   toupper(sub("^nr-?", "NR-", reg, ignore.case = TRUE)))
  quelle[nogo] <- NA_character_
  run[nogo]    <- keys[nogo]
  list(quelle = quelle, run = run)
}


# ---- intern: breite Tabelle auf bestimmte Zeitlaeufe vorfiltern -------------
#' Zeilen behalten, deren BEREINIGTER Lauf (ohne Quell-Praefix) einen der 'runs'
#' als Teilstring traegt - so matcht "OBS_DWD_1991-2020" trotz "bwi-bze_"-Praefix
#' im Dateinamen. runs = NULL -> Tabelle unveraendert. Fehler, wenn nichts bleibt.
.wl_filter_runs <- function(df, run_col, run_from_name, runs) {
  if (is.null(runs)) return(df)
  run  <- .wl_split_key(run_from_name(df[[run_col]]))$run   # "OBS_DWD_1991-2020"
  keep <- Reduce(`|`, lapply(runs, function(r) grepl(r, run, fixed = TRUE)))
  if (!any(keep))
    stop("runs-Filter {", paste(runs, collapse = ", "),
         "} passt auf keinen Zeitlauf (vorhanden u.a.: ",
         paste(utils::head(unique(run), 5), collapse = ", "), ").")
  df[keep, , drop = FALSE]
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
#' @param runs       optional: nur diese Zeitlaeufe behalten (vor dem Join
#'                    vorfiltern). Teilstring-Treffer auf den BEREINIGTEN Namen,
#'                    d.h. "OBS_DWD_1991-2020" matcht trotz "bwi-bze_"-Praefix.
#'                    NULL = alle.
#' @param split_quelle TRUE (Default): Ergebnis bekommt Spalte 'quelle'
#'                    ("BWI"/"NR-XX") und einen um das Quell-Praefix BEREINIGTEN
#'                    'Zeitlauf' ("OBS_DWD_1991-2020"). FALSE: Zeitlauf behaelt
#'                    den vollen Schluessel (altes Verhalten).
#' @param scale      NULL = Auto-Plausibilitaet; sonst 1 oder 0.1 explizit.
#' @return tibble: id | [quelle] | Zeitlauf | Monat | T_mean | P_sum (+ Koord.).
wl_long_from_tables <- function(temp_df, prec_df,
                                id_col       = "cell_id",
                                run_col      = "name",
                                coord_cols   = c("x", "y"),
                                month_cols   = NULL,
                                run_from_name = NULL,
                                runs         = NULL,
                                split_quelle = TRUE,
                                scale        = NULL) {
  for (pkg in c("dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Paket '", pkg, "' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  if (is.null(run_from_name))
    run_from_name <- function(x)
      sub("\\.nc$", "", sub("^[0-9]+_", "", basename(as.character(x))))

  # Optional vorfiltern: nur die gewuenschten Zeitlaeufe (Teilstring-Treffer).
  # Spart Speicher/Zeit und vermeidet die inner-join-Warnung bei Teilmengen.
  temp_df <- .wl_filter_runs(temp_df, run_col, run_from_name, runs)
  prec_df <- .wl_filter_runs(prec_df, run_col, run_from_name, runs)

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

  # Zeitlauf in quelle ("BWI"/"NR-XX") + bereinigten Zeitlauf trennen
  if (isTRUE(split_quelle)) {
    sp <- .wl_split_key(merged$Zeitlauf)
    merged$quelle   <- sp$quelle
    merged$Zeitlauf <- sp$run
  }

  dplyr::as_tibble(merged) %>%
    dplyr::arrange(.data$id, .data$Zeitlauf, .data$Monat)
}


# ---- Trend-Bruecke: breite Jahres-Tabellen -> WaLi-Trend-Long-Format --------
#' Breite Jahres-Temp-/Niederschlagstabellen ins WaLi-Trend-Long-Format.
#'
#' Pendant zu wl_long_from_tables() fuer die JAHRES-Produkte (1049/1050). Die
#' Jahresspalten sind die Kalenderjahre (aus den NC-Zeitstempeln); daraus wird
#' direkt 'Kalenderjahr' gebildet und - wenn moeglich - darueber gejoint, sonst
#' ueber die Spalten-Position (Jahr 1..n). Output passt direkt fuer
#' plot_wali_trend_from_long() / compare_wali_trend().
#'
#' @param temp_df,prec_df  breite Jahres-Tabellen (Temp bzw. Niederschlag).
#' @param id_col      Spalte mit der Zell-id (Default "cell_id"); wird zu 'id'.
#' @param run_col     Spalte mit der Lauf-Kennung/Dateiname (Default "name").
#' @param coord_cols  Koordinatenspalten, die nicht als Jahr zaehlen (Default
#'                    c("x","y")); werden, falls vorhanden, durchgereicht.
#' @param year_cols   optional: explizite Jahresspaltennamen. NULL = automatisch
#'                    alle Spalten ausser id/run/coord, in vorhandener Reihenfolge.
#' @param run_from_name  Funktion Dateiname -> Lauf-Schluessel. NULL = Default
#'                    (Parameter-ID-Praefix und ".nc" entfernen).
#' @param runs       optional: nur diese Zeitlaeufe behalten (Teilstring-Treffer);
#'                    NULL = alle.
#' @param split_quelle TRUE (Default): Spalte 'quelle' + bereinigter 'Zeitlauf'
#'                    (wie wl_long_from_tables). FALSE: voller Schluessel.
#' @param scale      NULL = Auto-Plausibilitaet; sonst 1 oder 0.1 explizit.
#' @return tibble: id | [quelle] | Zeitlauf | Jahr | Kalenderjahr | T_year | P_year
wali_trend_from_tables <- function(temp_df, prec_df,
                                   id_col        = "cell_id",
                                   run_col       = "name",
                                   coord_cols    = c("x", "y"),
                                   year_cols     = NULL,
                                   run_from_name = NULL,
                                   runs          = NULL,
                                   split_quelle  = TRUE,
                                   scale         = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Paket 'dplyr' wird benoetigt.")
  `%>%` <- dplyr::`%>%`

  if (is.null(run_from_name))
    run_from_name <- function(x)
      sub("\\.nc$", "", sub("^[0-9]+_", "", basename(as.character(x))))

  temp_df <- .wl_filter_runs(temp_df, run_col, run_from_name, runs)
  prec_df <- .wl_filter_runs(prec_df, run_col, run_from_name, runs)

  # ein breites df -> langes df (id | Zeitlauf | Jahr | Kalenderjahr | <value>)
  pivot_one <- function(df, value_name) {
    if (!id_col  %in% names(df)) stop("Spalte '", id_col,  "' fehlt in der Tabelle.")
    if (!run_col %in% names(df)) stop("Spalte '", run_col, "' fehlt in der Tabelle.")
    ycols <- year_cols
    if (is.null(ycols))
      ycols <- setdiff(names(df), c(id_col, run_col, coord_cols))
    if (length(ycols) < 2)
      stop("Erwarte mehrere Jahresspalten, gefunden ", length(ycols), " (",
           paste(ycols, collapse = ", "), "). year_cols explizit angeben.")

    n  <- nrow(df)
    ny <- length(ycols)
    kj <- suppressWarnings(as.integer(ycols))   # Kalenderjahr aus Spaltenname
    # unlist(df[ycols]) ist spaltenweise: Jahr1 fuer alle Zeilen, dann Jahr2 ...
    out <- data.frame(
      id           = rep(df[[id_col]], times = ny),
      Zeitlauf     = rep(run_from_name(df[[run_col]]), times = ny),
      Jahr         = rep(seq_len(ny), each = n),
      Kalenderjahr = rep(kj, each = n),
      stringsAsFactors = FALSE
    )
    out[[value_name]] <- unlist(df[ycols], use.names = FALSE)
    out
  }

  t_long <- pivot_one(temp_df, "T_year")
  p_long <- pivot_one(prec_df, "P_year")

  # Robust ueber das echte Kalenderjahr joinen, falls fuer beide vorhanden;
  # sonst ueber die Spalten-Position (Jahr). Vermeidet Versatz bei ungleich
  # langen Laeufen (21/29 Jahre, v2/v3-Duplikate).
  if (all(!is.na(t_long$Kalenderjahr)) && all(!is.na(p_long$Kalenderjahr))) {
    p_join <- p_long[, c("id", "Zeitlauf", "Kalenderjahr", "P_year")]
    merged <- dplyr::inner_join(t_long, p_join,
                                by = c("id", "Zeitlauf", "Kalenderjahr"))
  } else {
    p_join <- p_long[, c("id", "Zeitlauf", "Jahr", "P_year")]
    merged <- dplyr::inner_join(t_long, p_join,
                                by = c("id", "Zeitlauf", "Jahr"))
  }
  if (nrow(merged) == 0)
    stop("Kein gemeinsamer id/Zeitlauf/Jahr zwischen Temp- und ",
         "Niederschlagstabelle. Stimmen id_col und die Lauf-Kennung ueberein?")
  if (nrow(merged) < max(nrow(t_long), nrow(p_long)))
    warning("Temp- und Niederschlagstabelle decken sich nicht vollstaendig - ",
            "es bleiben nur gemeinsame Zellen/Jahre/Laeufe (inner join).",
            call. = FALSE)

  # Skalierungsfaktor (auto aus Temperatur), auf T und P gemeinsam anwenden
  fac <- if (!is.null(scale)) {
    scale
  } else if (exists(".wl_detect_scale", mode = "function")) {
    .wl_detect_scale(merged$T_year, scale)
  } else {
    m <- stats::median(abs(merged$T_year), na.rm = TRUE)
    if (is.finite(m) && m > 60) {
      warning("Temperaturwerte wirken um Faktor 10 zu gross (Median |T| = ",
              round(m, 1), "). Wende Faktor 0.1 an.", call. = FALSE)
      0.1
    } else 1
  }
  merged$T_year <- merged$T_year * fac
  merged$P_year <- merged$P_year * fac

  # Koordinaten (pro Zelle konstant) anhaengen, falls vorhanden
  cc <- intersect(coord_cols, names(temp_df))
  if (length(cc) > 0) {
    coords <- temp_df[!duplicated(temp_df[[id_col]]), c(id_col, cc), drop = FALSE]
    names(coords)[1] <- "id"
    merged <- dplyr::left_join(merged, coords, by = "id")
  }

  if (isTRUE(split_quelle)) {
    sp <- .wl_split_key(merged$Zeitlauf)
    merged$quelle   <- sp$quelle
    merged$Zeitlauf <- sp$run
  }

  dplyr::as_tibble(merged) %>%
    dplyr::arrange(.data$id, .data$Zeitlauf, .data$Jahr)
}


# ---- Beispiel (auskommentiert) ---------------------------------------------
# source("02_function/WL_Diagramme/walther_lieth_input.R")  # .wl_detect_scale()
# source("02_function/WL_Diagramme/plot_walther_lieth.R")   # Monats-WL
# source("02_function/WL_Diagramme/wali_trend.R")           # WaLi-Trend
#
# base <- "../../../data/data_raw/extra_downloads"
#
# ## Dateien je Parameter listen - Pattern am Dateinamen-ANFANG verankern,
# ## damit z.B. keine 1155-Datei in der 1157-Liste landet:
# liste <- function(id)
#   list.files(file.path(base, id), pattern = paste0("^", id, "_.*\\.nc$"),
#              full.names = TRUE, recursive = TRUE)
#
# ## --- Alle Dateien eines Parameters in einer Schleife einlesen + stapeln ---
# ## (jede Datei meldet sich per message() mit ihrem name; assign_global = FALSE,
# ##  damit die Schleife nicht staendig nc.1157_df ueberschreibt)
# read_all <- function(files, fun)
#   do.call(rbind, lapply(files, function(f) fun(f, assign_global = FALSE)))
#
# temp_df <- read_all(liste("1155"), nc.1155_function)   # je Datei: "[<name>] gelesen: ..."
# prec_df <- read_all(liste("1157"), nc.1157_function)
# wl <- wl_long_from_tables(temp_df, prec_df)            # joint pro Zeitlauf ueber name
# plot_walther_lieth_from_long(wl, id_val = 1, run = wl$Zeitlauf[1])
#
# ## --- WaLi-Trend (1049/1050) analog ---
# t_year <- read_all(liste("1049"), nc.1049_function)
# p_year <- read_all(liste("1050"), nc.1050_function)
# tr <- wali_trend_from_tables(t_year, p_year)
# plot_wali_trend_from_long(tr, id_val = 1, run = tr$Zeitlauf[1])
