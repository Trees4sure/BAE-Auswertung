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

#' Koernungs-Attribute zu einer Hauptgruppe (S/U/L/T)
#'
#' @param gruppe  Character-Vektor mit "S"/"U"/"L"/"T" (sonst NA)
#' @return  data.frame mit gruppe, name, farbe, symbol, aqp_density, aqp_angle
.attribute_aus_gruppe <- function(gruppe) {
  ref <- koernung_referenz()
  idx <- match(gruppe, ref$erstbuchstabe)
  neutral <- data.frame(gruppe = NA_character_, name = "unbekannt",
                        farbe = "#E0E0E0", symbol = "keine",
                        aqp_density = 0, aqp_angle = 0,
                        stringsAsFactors = FALSE)
  out <- as.data.frame(
    ref[idx, c("gruppe", "name", "farbe", "symbol", "aqp_density", "aqp_angle")],
    stringsAsFactors = FALSE)
  na_zeilen <- is.na(idx)
  if (any(na_zeilen)) out[na_zeilen, ] <- neutral
  rownames(out) <- NULL
  out
}

#' KA5-Bodenart -> Koernungs-Attribute (Gruppe, Farbe, Symbol, Schraffur)
#'
#' Zuordnung ueber den ERSTEN GROSSBUCHSTABEN S/U/L/T des Kuerzels
#' (S=Sand, U=Schluff, L=Lehm, T=Ton). Wichtig: die Korngroessen-Praefixe
#' f/m/g (Fein-/Mittel-/Grobsand) sind kleingeschrieben und werden dadurch
#' korrekt uebersprungen, z.B.:
#'   "Sl3" -> S, "Lt2" -> L, "Ut3" -> U, "Tu2" -> T,
#'   "fS"  -> S, "mSfs" -> S, "gSms" -> S.
#' Organische/Sonstige (Hn, Ha, Fhh, V, "NA") ergeben keine Zuordnung.
#'
#' @param boart  Character-Vektor der Bodenart-Kuerzel (z.B. "Sl3", "mSfs")
koernung_attribute <- function(boart) {
  b   <- as.character(boart)
  grp <- rep(NA_character_, length(b))
  m   <- regexpr("[SULT]", b)                 # case-sensitiv: nur Grossbuchstaben
  hit <- !is.na(b) & m > 0
  grp[hit] <- substr(b[hit], m[hit], m[hit])
  .attribute_aus_gruppe(grp)
}

#' Koernungs-Hauptgruppe aus den Kornanteilen Sand/Schluff/Ton ableiten
#'
#' Robuster Fallback, wenn kein (auswertbares) Bodenart-Kuerzel vorliegt.
#' Vereinfachte Zuordnung zu den vier KA5-Hauptgruppen ueber die Anteile
#' (in Masse-%; -9999/negative Werte gelten als fehlend).
#'
#' @return  Character-Vektor "S"/"U"/"L"/"T" (NA, wenn keine Anteile vorhanden)
gruppe_aus_anteilen <- function(sand, schluff, ton) {
  s <- suppressWarnings(as.numeric(sand))
  u <- suppressWarnings(as.numeric(schluff))
  t <- suppressWarnings(as.numeric(ton))
  s[is.na(s) | s < 0] <- NA; u[is.na(u) | u < 0] <- NA; t[is.na(t) | t < 0] <- NA

  s0 <- ifelse(is.na(s), 0, s); u0 <- ifelse(is.na(u), 0, u); t0 <- ifelse(is.na(t), 0, t)
  summe <- s0 + u0 + t0
  ok <- summe > 0
  s0 <- ifelse(ok, s0 / summe * 100, NA)
  u0 <- ifelse(ok, u0 / summe * 100, NA)
  t0 <- ifelse(ok, t0 / summe * 100, NA)

  grp <- ifelse(t0 >= 25, "T",
         ifelse(u0 >= 50, "U",
         ifelse(s0 >= 50 & t0 < 17, "S", "L")))
  grp[!ok] <- NA_character_
  grp
}
