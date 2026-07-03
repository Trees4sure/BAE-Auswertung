# =====================================================================
# 02_horizont_abfolge_plot.R
# ---------------------------------------------------------------------
# Horizontabfolge-Plot einer Feinbodenform (SOEH_KRZ) mit
#   - Standard-Farbwahl  (gemessene Munsell-Farbe, sonst KA5-Fallback)
#   - KA5-Koernungs-Symbolen (Sand=Punkte, Schluff=Striche,
#     Lehm=Mischung, Ton=Linien)
#
# Baut auf aqp (Algorithms for Quantitative Pedology) auf und ersetzt
# die frei gewaehlten hue/value/chroma-Zuweisungen des Ausgangs-Skripts.
# =====================================================================

# Benoetigte Pakete: aqp, dplyr, stringr, tibble
# source("R/00_ka5_referenz.R"); source("R/01_db_zugriff.R")


# ---------------------------------------------------------------------
# Farbe je Horizont bestimmen: gemessene Munsell-Farbe bevorzugen
# ---------------------------------------------------------------------
#' @param horizont  Horizont-Symbole
#' @param munsell   gemessene Munsell-Farbe als String (z.B. "10YR 3/2"),
#'                  darf NA sein
#' @return          Hex-Farbvektor
horizont_farbe <- function(horizont, munsell = NA_character_) {
  n <- length(horizont)
  if (length(munsell) == 1L) munsell <- rep(munsell, n)

  # 1) gemessene Munsell-Farbe normalisieren und parsen
  m <- toupper(trimws(as.character(munsell)))
  m <- gsub(",", ".", m, fixed = TRUE)                 # "3,2" -> "3.2"
  m <- gsub("([A-Z])([0-9])", "\\1 \\2", m)            # "10YR3/2" -> "10YR 3/2"
  hex_measured <- rep(NA_character_, n)
  gut <- !is.na(m) & m != "" & grepl("/", m)
  if (any(gut)) {
    hex_measured[gut] <- tryCatch(
      aqp::parseMunsell(m[gut]),
      error = function(e) rep(NA_character_, sum(gut))
    )
  }

  # 2) Fallback ueber genetisches Horizont-Symbol
  hex_fallback <- horizont_farbe_aus_symbol(horizont)

  ifelse(is.na(hex_measured) | hex_measured %in% c("NA", "#NA"),
         hex_fallback, hex_measured)
}


# ---------------------------------------------------------------------
# Feinbodenform(en) aufloesen (inkl. Kombiform-Fallback)
# ---------------------------------------------------------------------
# Sucht die angeforderten SOEH_KRZ in den vorhandenen Daten. Wird eine
# Kombiform (z.B. "MüS/BiS") nicht direkt gefunden, wird auf die
# Teilformen ausgewichen - je nachdem, welche vorhanden sind (beide,
# nur die eine oder nur die andere).
#' @return Character-Vektor der tatsaechlich vorhandenen SOEH_KRZ
.resolve_soeh_krz <- function(data_input, soeh_krz) {
  vorhanden  <- unique(as.character(data_input$SOEH_KRZ))
  aufgeloest <- character(0)

  for (code in soeh_krz) {
    if (code %in% vorhanden) {
      aufgeloest <- c(aufgeloest, code)
      next
    }
    if (grepl("/", code, fixed = TRUE)) {              # Kombiform testen
      teile    <- trimws(strsplit(code, "/", fixed = TRUE)[[1]])
      gefunden <- teile[teile %in% vorhanden]
      if (length(gefunden)) {
        fehlt <- setdiff(teile, gefunden)
        message("Kombiform '", code, "' nicht direkt vorhanden -> verwende Teilform(en): ",
                paste(gefunden, collapse = ", "),
                if (length(fehlt)) paste0("  (nicht gefunden: ", paste(fehlt, collapse = ", "), ")") else "")
        aufgeloest <- c(aufgeloest, gefunden)
      } else {
        message("Kombiform '", code, "': keine der Teilformen (",
                paste(teile, collapse = ", "), ") gefunden.")
      }
    } else {
      message("Feinbodenform '", code, "' nicht gefunden.")
    }
  }
  unique(aufgeloest)
}


# ---------------------------------------------------------------------
# Datensatz fuer die Darstellung aufbereiten
# ---------------------------------------------------------------------
#' @param data_input  Horizontdaten (z.B. aus lade_leitprofile())
#' @param soeh_krz    Vektor der darzustellenden Feinbodenform-Kuerzel
#' @param region      optionaler Filter auf Bundesland-Spalte BL.
#'                    NULL (Standard) = ueber alle Regionen suchen/plotten.
aufbereiten_profil <- function(data_input, soeh_krz, region = NULL) {

  df <- data_input
  if (!is.null(region) && "BL" %in% names(df)) {
    df <- dplyr::filter(df, BL == region)
  }

  codes <- .resolve_soeh_krz(df, soeh_krz)             # inkl. Kombiform-Fallback
  if (length(codes) == 0) {
    stop("Keine Datensaetze fuer SOEH_KRZ = ",
         paste(soeh_krz, collapse = ", "),
         if (!is.null(region)) paste0(" (Region ", region, ")") else " (alle Regionen)")
  }
  df <- dplyr::filter(df, SOEH_KRZ %in% codes)

  # transparent machen, welche Profile (group_ID) tatsaechlich Horizontdaten
  # haben und geplottet werden. Fehlt eine in den Kartiereinheiten bekannte
  # Region hier, existiert dafuer schlicht kein Leitprofil (vgl. pruefe_leitprofil()).
  if ("group_ID" %in% names(df)) {
    message("Geplottete Profile (group_ID): ",
            paste(sort(unique(df$group_ID)), collapse = ", "))
  }

  # fehlende Untergrenzen (-9999) abfangen: +50 cm auf die Obergrenze
  df <- dplyr::mutate(df,
    TIEFE_UG = dplyr::case_when(TIEFE_UG %in% c(-9999, "-9999") ~ TIEFE_OG + 50,
                                TRUE ~ as.numeric(TIEFE_UG)),
    TIEFE_OG = as.numeric(TIEFE_OG)
  )

  # Standard-Farbe (Munsell gemessen -> sonst KA5-Fallback)
  df$soil_color <- horizont_farbe(df$HORIZONT, df$.munsell)

  # Koernungs-Attribute (Gruppe, Symbol, Schraffur) aus der Bodenart (BODART).
  ka <- koernung_attribute(df$.boart)

  # Fallback: wo keine Zuordnung aus dem Kuerzel gelingt, aus den Kornanteilen
  # SAND/SCHLUFF/TON ableiten (Spalten der Leitprofil-Tabelle).
  if (all(c("SAND", "SCHLUFF", "TON") %in% names(df))) {
    leer <- is.na(ka$gruppe)
    if (any(leer)) {
      grp <- gruppe_aus_anteilen(df$SAND, df$SCHLUFF, df$TON)
      ka2 <- .attribute_aus_gruppe(grp)
      ka[leer, ] <- ka2[leer, ]
    }
  }
  if (all(is.na(ka$gruppe)))
    message("Hinweis: keine Koernung ableitbar (weder BODART noch SAND/SCHLUFF/TON) ",
            "-> keine Koernungs-Symbole.")

  df$koern_gruppe  <- ka$gruppe
  df$koern_name    <- ka$name
  df$koern_symbol  <- ka$symbol
  df$koern_density <- ka$aqp_density
  df$koern_angle   <- ka$aqp_angle

  df$hzname <- df$HORIZONT
  tibble::as_tibble(df)
}


# ---------------------------------------------------------------------
# KA5-Koernungs-Symbole ueber das aqp-Profil zeichnen
# ---------------------------------------------------------------------
# Liest die TATSAECHLICHE Plot-Geometrie von aqp aus (x-Positionen, Breite,
# scaling.factor, y.offset), statt sie zu raten -> Symbole liegen exakt im
# Horizont-Rechteck. Faellt auf die Standardwerte (x = 1..n, width = 0.2,
# sf = 1, y.offset = 0) zurueck, falls die Metadaten nicht verfuegbar sind.

#' aqp-Metadaten des letzten plotSPC()-Aufrufs holen (robust)
.aqp_last_plot <- function() {
  env <- tryCatch(get("aqp.env", envir = asNamespace("aqp")), error = function(e) NULL)
  if (is.null(env)) return(NULL)
  tryCatch(get("last_spc_plot", envir = env), error = function(e) NULL)
}

add_koernung_symbole <- function(spc, df, id_col = "SOEH_KRZ", width = NULL) {

  ids <- aqp::profile_id(spc)         # Profil-Reihenfolge im Plot
  n   <- length(ids)

  lsp <- .aqp_last_plot()
  x0  <- if (!is.null(lsp$x0) && length(lsp$x0) == n) as.numeric(lsp$x0) else seq_len(n)
  w   <- if (!is.null(width)) width else if (!is.null(lsp$width)) as.numeric(lsp$width)[1] else 0.2
  sf  <- rep(if (!is.null(lsp$scaling.factor)) as.numeric(lsp$scaling.factor) else 1, length.out = n)
  yo  <- rep(if (!is.null(lsp$y.offset))       as.numeric(lsp$y.offset)       else 0, length.out = n)

  ytr <- function(d, i) yo[i] + d * sf[i]               # Tiefe -> Plot-y

  for (i in seq_along(ids)) {
    sub <- df[as.character(df[[id_col]]) == ids[i], , drop = FALSE]
    if (nrow(sub) == 0) next
    xl <- x0[i] - w * 0.75                               # etwas eingerueckt
    xr <- x0[i] + w * 0.75

    for (r in seq_len(nrow(sub))) {
      top <- ytr(sub$TIEFE_OG[r], i); bot <- ytr(sub$TIEFE_UG[r], i)
      if (is.na(top) || is.na(bot)) next
      if (bot < top) { tmp <- top; top <- bot; bot <- tmp }
      if (bot - top < 1) next
      inset <- min(2, (bot - top) * 0.15)                # Rand freihalten
      yt <- top + inset; yb <- bot - inset
      if (yb <= yt) { yt <- top; yb <- bot }
      sym <- sub$koern_symbol[r]

      if (sym == "punkte") {                    # Sand
        n_pt <- max(6, round((yb - yt) / 3))
        px <- runif(n_pt, xl, xr)
        py <- runif(n_pt, yt, yb)
        points(px, py, pch = 20, cex = 0.28, col = "#00000088")

      } else if (sym == "striche") {            # Schluff
        ys <- seq(yt, yb, by = 6)
        for (y in ys) {
          xs <- seq(xl, xr, length.out = 4)
          segments(xs, y, pmin(xs + (xr - xl) / 8, xr), y, col = "#00000088", lwd = 0.8)
        }

      } else if (sym == "linien") {             # Ton
        ys <- seq(yt, yb, by = 5)
        segments(xl, ys, xr, ys, col = "#00000099", lwd = 0.9)

      } else if (sym == "misch") {              # Lehm: Punkte + Striche
        ys <- seq(yt, yb, by = 8)
        for (k in seq_along(ys)) {
          y <- ys[k]
          if (k %% 2 == 1) {
            segments(xl, y, xr, y, col = "#00000088", lwd = 0.8)
          } else {
            px <- seq(xl, xr, length.out = 5)
            points(px, rep(y, length(px)), pch = 20, cex = 0.26, col = "#00000088")
          }
        }
      }
    }
  }
  invisible(NULL)
}


# ---------------------------------------------------------------------
# Hauptfunktion: Horizontabfolge plotten
# ---------------------------------------------------------------------
#' @param data_input   Horizontdaten (aus lade_leitprofile())
#' @param soeh_krz     Feinbodenform-Kuerzel (ein oder mehrere). Kombiformen
#'                     mit "/" werden bei Bedarf auf ihre Teilformen aufgeloest.
#' @param region       optionaler BL-Filter (z.B. "MV"). NULL (Standard) =
#'                     ueber alle Regionen suchen und plotten.
#' @param koernung     TRUE = KA5-Koernungs-Symbole einzeichnen
#' @param schraffur    TRUE = zusaetzlich aqp-Schraffur ueber density nutzen
#' @return             (unsichtbar) die aufbereitete SoilProfileCollection
horizont_abfolge_plot <- function(data_input,
                                  soeh_krz,
                                  region     = NULL,
                                  koernung   = TRUE,
                                  schraffur  = FALSE,
                                  untertitel = NULL) {

  df <- aufbereiten_profil(data_input, soeh_krz, region = region)

  # -> SoilProfileCollection.  Als eindeutige Profil-ID die group_ID nutzen,
  # damit dieselbe SOEH_KRZ aus mehreren Regionen (z.B. MV_BiS_1 + ST_BiS_1)
  # als getrennte Profile erscheint. Sonst Rueckfall auf SOEH_KRZ.
  spc_df <- as.data.frame(df)
  id_col <- if ("group_ID" %in% names(spc_df)) "group_ID" else "SOEH_KRZ"
  aqp::depths(spc_df) <- stats::as.formula(paste(id_col, "~ TIEFE_OG + TIEFE_UG"))

  par(mar = c(0, 0, 3, 1))

  args <- list(
    x           = spc_df,
    id.style    = "side",
    name.style  = "center-center",
    cex.names   = 0.6,
    name        = "hzname",
    color       = "soil_color"
  )
  if (schraffur) args$density <- "koern_density"   # aqp-Schraffur je Bodenart

  do.call(aqp::plotSPC, args)
  title(main = paste0("Horizontabfolge - Feinbodenform(en): ",
                      paste(unique(df$SOEH_KRZ), collapse = ", "),
                      if (!is.null(region)) paste0("  (", region, ")") else "  (alle Regionen)"),
        cex.main = 0.9)
  if (!is.null(untertitel))
    mtext(untertitel, side = 1, line = -1, cex = 0.72, col = "grey30")

  # KA5-Koernungs-Symbole ueberlagern
  if (koernung) {
    tryCatch(
      add_koernung_symbole(spc_df, df, id_col = id_col),
      error = function(e) message("Koernungs-Symbole konnten nicht gezeichnet werden: ", conditionMessage(e))
    )
  }

  invisible(spc_df)
}


# ---------------------------------------------------------------------
# Horizontabfolge direkt ueber eine MASTER_ID plotten
# ---------------------------------------------------------------------
#' Schlaegt zur MASTER_ID automatisch die group_ID nach (Quelle NR/BWI/BZE
#' anhand des Praefixes, sonst alle durchsucht) und plottet das Leitprofil.
#'
#' @param master_id  MASTER_ID (z.B. "NR_130_08_66519")
#' @param quelle     optional erzwingen ("NR"/"BWI"/"BZE"/"STOK")
#' @param koernung,schraffur  wie in horizont_abfolge_plot()
#' @return  (unsichtbar) die aufbereitete SoilProfileCollection
horizont_abfolge_plot_master <- function(master_id, quelle = NULL,
                                        koernung = TRUE, schraffur = FALSE) {
  df <- lade_leitprofil_master(master_id, quelle = quelle)

  # Bei Ausweichformen (Kombiform ohne eigenes Leitprofil) vermerken
  untertitel <- if (isTRUE(attr(df, "ausweich")))
    paste0("Ausweichform fuer '", attr(df, "ausweich_von"), "' (", master_id,
           "): kein eigenes Leitprofil") else NULL

  horizont_abfolge_plot(df, soeh_krz = unique(df$SOEH_KRZ), region = NULL,
                        koernung = koernung, schraffur = schraffur,
                        untertitel = untertitel)
}


# ---------------------------------------------------------------------
# Horizontabfolge-Plots je MASTER_ID als PNG speichern
# ---------------------------------------------------------------------
.dateiname_sicher <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

#' Fuer mehrere MASTER_IDs je ein PNG in ein Verzeichnis schreiben
#'
#' @param ids         Vektor von MASTER_IDs
#' @param verzeichnis Zielordner (wird bei Bedarf angelegt)
#' @param quelle      optional Quelle erzwingen ("NR"/"BWI"/"BZE"); sonst
#'                    aus dem MASTER_ID-Praefix erkannt
#' @param breite,hoehe,res  PNG-Groesse (px) und Aufloesung (dpi)
#' @return  (unsichtbar) data.frame mit master_id, datei, status
horizont_plots_speichern <- function(ids,
                                    verzeichnis = "04_results/Horizonte",
                                    quelle = NULL,
                                    breite = 1200, hoehe = 900, res = 150,
                                    koernung = TRUE, schraffur = FALSE) {
  if (!dir.exists(verzeichnis)) dir.create(verzeichnis, recursive = TRUE)

  status <- character(length(ids)); dateien <- character(length(ids))
  for (k in seq_along(ids)) {
    id    <- ids[k]
    datei <- file.path(verzeichnis, paste0("Horizont_", .dateiname_sicher(id), ".png"))
    dateien[k] <- datei
    status[k] <- tryCatch({
      grDevices::png(datei, width = breite, height = hoehe, res = res)
      horizont_abfolge_plot_master(id, quelle = quelle,
                                   koernung = koernung, schraffur = schraffur)
      grDevices::dev.off()
      "ok"
    }, error = function(e) {
      if (length(grDevices::dev.list())) grDevices::dev.off()
      if (file.exists(datei)) unlink(datei)          # unvollstaendige Datei entfernen
      message("FEHLER bei '", id, "': ", conditionMessage(e))
      paste0("Fehler: ", conditionMessage(e))
    })
  }

  ergebnis <- data.frame(master_id = ids, datei = dateien, status = status,
                        stringsAsFactors = FALSE)
  message("Gespeichert nach ", normalizePath(verzeichnis, mustWork = FALSE),
          "  (", sum(status == "ok"), "/", length(ids), " erfolgreich)")
  invisible(ergebnis)
}


# ---------------------------------------------------------------------
# Legende der KA5-Koernungs-Symbole (eigene Grafik)
# ---------------------------------------------------------------------
koernung_legende <- function() {
  ref <- koernung_referenz()
  op <- par(mar = c(1, 1, 3, 1)); on.exit(par(op))
  plot(NA, xlim = c(0, 4), ylim = c(0, nrow(ref)), axes = FALSE,
       xlab = "", ylab = "", main = "KA5-Koernungs-Symbole")

  for (i in seq_len(nrow(ref))) {
    y  <- nrow(ref) - i + 0.5
    xl <- 0.2; xr <- 1.2
    rect(xl, y - 0.35, xr, y + 0.35, col = ref$farbe[i], border = "grey40")
    sym <- ref$symbol[i]

    if (sym == "punkte") {
      points(runif(30, xl, xr), runif(30, y - 0.3, y + 0.3), pch = 20, cex = 0.4)
    } else if (sym == "striche") {
      for (yy in seq(y - 0.25, y + 0.25, by = 0.16))
        segments(seq(xl, xr, length.out = 4), yy,
                 seq(xl, xr, length.out = 4) + 0.08, yy, lwd = 0.9)
    } else if (sym == "linien") {
      for (yy in seq(y - 0.25, y + 0.25, by = 0.12)) segments(xl, yy, xr, yy, lwd = 1)
    } else if (sym == "misch") {
      segments(xl, y + 0.12, xr, y + 0.12, lwd = 1)
      points(seq(xl, xr, length.out = 6), rep(y - 0.12, 6), pch = 20, cex = 0.4)
    }
    text(xr + 0.2, y, labels = paste0(ref$name[i], "  (", ref$erstbuchstabe[i], "...)"),
         adj = 0, cex = 0.9)
  }
  invisible(NULL)
}
