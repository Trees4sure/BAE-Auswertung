# =====================================================================
# 00_ka5_referenz.R
# ---------------------------------------------------------------------
# Standard-Referenzen fuer die Darstellung von Bodenprofilen nach
# der Bodenkundlichen Kartieranleitung (KA5 / KA6, AG Boden 2005 ff.).
#
# Enthaelt zwei Nachschlage-Tabellen und die zugehoerigen Hilfsfunktionen:
#
#   1) horizont_farb_referenz()  ->  Standard-Farben (Munsell) je
#      genetischem Horizont-Symbol. Ersetzt die frei gewaehlten
#      hue/value/chroma-Werte durch eine an KA5 angelehnte, naturnahe
#      Farbwahl. Wird nur als *Fallback* benutzt, wenn im Datensatz
#      keine im Feld gemessene Munsell-Farbe vorliegt.
#
#   2) koernung_referenz()       ->  KA5-Bodenarten-Hauptgruppen mit
#      Standard-Kartenfarbe UND Koernungs-Symbol (Sand = Punkte,
#      Schluff = kurze Striche, Lehm = Mischung, Ton = Linien).
#
# Die exakten Legendenfarben der KA5 sind nicht frei verfuegbar; die
# hier hinterlegten Munsell-/Hex-Werte sind naturnahe Naeherungen, die
# sich an den ueblichen Boden-/Bodenartenlegenden orientieren. Sie sind
# an einer Stelle definiert und koennen zentral angepasst werden.
# =====================================================================


# ---------------------------------------------------------------------
# 1) Genetische Horizonte  ->  Standard-Munsell-Farbe
# ---------------------------------------------------------------------
# Zuordnung ueber das FUEHRENDE Merkmal des Horizont-Symbols. Die
# Reihenfolge in der Tabelle ist zugleich die PRUEF-Reihenfolge:
# spezielle/eindeutige Faelle zuerst, allgemeine zuletzt.
# Die Farbe wird als Munsell-Notation angegeben und spaeter mit
# aqp::parseMunsell() in einen Hex-Wert uebersetzt.
horizont_farb_referenz <- function() {
  tibble::tribble(
    ~schluessel,   ~regex,                 ~munsell,     ~beschreibung,
    "O/H_Auflage", "^[OLH]",               "10YR 2/1",   "Humusauflage / organischer Horizont (Torf, Of, Oh, L)",
    "Ae/E",        "^Ae|^E",               "10YR 7/2",   "Gebleichter Eluvialhorizont (Ae, E) - zuerst pruefen!",
    "Ah",          "^A",                   "10YR 2/2",   "Humoser Oberboden (Ah, Aa, Aeh)",
    "Bt",          "Bt",                   "5YR 4/6",    "Tonanreicherung (Bt) - lebhaft braun/rot",
    "Bs/Bh",       "B[sh]",                "5YR 3/3",    "Sesquioxid-/Humusanreicherung (Bs, Bh) - Podsol",
    "Bv",          "^B",                   "7.5YR 4/4",  "Verwitterungshorizont (Bv) - braun",
    "Sw",          "Sw",                   "2.5Y 6/4",   "Stauwasserleiter, oxidativ (Sw) - fahl, marmoriert",
    "Sd",          "Sd",                   "2.5Y 5/2",   "Stauwassersohle, reduziert (Sd) - grau",
    "S_stau",      "^S|S$",                "2.5Y 6/3",   "Pseudogley/Stauwasser allgemein",
    "Go",          "Go",                   "2.5Y 6/4",   "Grundwasser, oxidativ (Go) - rostfleckig",
    "Gr",          "Gr",                   "5GY 5/1",    "Grundwasser, reduziert (Gr) - blaugrau",
    "G_gley",      "^G|G",                 "5Y 6/2",     "Gley allgemein",
    "C",           "C",                    "2.5Y 6/3",   "Ausgangsgestein (C, Cv, Cn) - hellgrau-braun",
    "Rest",        ".",                    "10YR 5/3",   "Nicht zugeordnet - neutrales Braungrau"
  )
}

#' Standard-Horizontfarbe(n) als Hex bestimmen (Fallback ueber Horizont-Symbol)
#'
#' @param horizont   Character-Vektor der Horizont-Symbole (z.B. "Bv", "Sw-Bv")
#' @return           Character-Vektor mit Hex-Farben
horizont_farbe_aus_symbol <- function(horizont) {
  ref <- horizont_farb_referenz()
  # je Horizont die erste passende Regex-Regel suchen
  idx <- vapply(horizont, function(h) {
    if (is.na(h) || h == "") return(nrow(ref))          # -> "Rest"
    treffer <- which(vapply(ref$regex, function(rx) stringr::str_detect(h, rx), logical(1)))
    if (length(treffer) == 0) nrow(ref) else treffer[1]
  }, integer(1))
  munsell <- ref$munsell[idx]
  aqp::parseMunsell(munsell)   # Munsell -> Hex
}


# ---------------------------------------------------------------------
# 2) KA5-Bodenarten  ->  Hauptgruppe, Kartenfarbe, Koernungs-Symbol
# ---------------------------------------------------------------------
# Die KA5 unterscheidet 31 Bodenarten, die sich zu vier
# Hauptgruppen zusammenfassen lassen. Die Zuordnung erfolgt ueber den
# ERSTEN Buchstaben des Bodenart-Kuerzels (Ss, Sl3, Uu, Us, Ls2, Lt3,
# Tu4, Tt ...):
#   S = Sande, U = Schluffe, L = Lehme, T = Tone.
#
# Koernungs-Symbolik (nach KA5-Konvention, hier als zeichenbare
# Naeherung umgesetzt):
#   Sand    -> Punkte            (grob, offen)
#   Schluff -> kurze Striche     (fein gestrichelt)
#   Lehm    -> Punkte + Striche  (Mischung)
#   Ton     -> durchgehende Linien (dicht, waagerecht)
#
# Fuer die aqp-Schraffur (plotSPC(density=...)) gilt: grob = wenig
# Linien (offen), fein = viele Linien (dicht).
koernung_referenz <- function() {
  tibble::tribble(
    ~gruppe,   ~erstbuchstabe, ~name,      ~farbe,      ~symbol,      ~aqp_density, ~aqp_angle,
    "S",       "S",            "Sand",     "#F2E39D",   "punkte",     0,            0,
    "U",       "U",            "Schluff",  "#D9E6B5",   "striche",    8,            0,
    "L",       "L",            "Lehm",     "#D2A679",   "misch",      15,           45,
    "T",       "T",            "Ton",      "#C1666B",   "linien",     25,           0
  )
}

#' KA5-Bodenart -> Koernungs-Attribute (Gruppe, Farbe, Symbol, Schraffur)
#'
#' @param boart  Character-Vektor der Bodenart-Kuerzel (z.B. "Sl3", "Lt2")
#' @return       data.frame mit Spalten gruppe, name, farbe, symbol,
#'               aqp_density, aqp_angle - Zeile fuer Zeile passend zu `boart`
koernung_attribute <- function(boart) {
  ref <- koernung_referenz()
  erst <- toupper(substr(as.character(boart), 1, 1))
  idx  <- match(erst, ref$erstbuchstabe)
  # nicht zuordenbare / fehlende Bodenarten -> neutrale Zeile
  neutral <- data.frame(gruppe = NA_character_, name = "unbekannt",
                        farbe = "#E0E0E0", symbol = "keine",
                        aqp_density = 0, aqp_angle = 0,
                        stringsAsFactors = FALSE)
  out <- ref[idx, c("gruppe", "name", "farbe", "symbol", "aqp_density", "aqp_angle")]
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  na_zeilen <- is.na(idx)
  if (any(na_zeilen)) out[na_zeilen, ] <- neutral
  rownames(out) <- NULL
  out
}
