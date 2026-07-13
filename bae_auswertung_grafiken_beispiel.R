# ----  bae_auswertung_grafiken_beispiel.R  ----
#
# FLACHER SCHRITT-FÜR-SCHRITT-DURCHLAUF für EINE Beispiel-Datei / MASTER_ID.
#
# Kein Funktions-Wrapper: jeden Block der Reihe nach markieren und ausführen,
# dann das Zwischenergebnis ansehen (head(d), View(d), table(...), count(...)).
# Dieses Skript zeigt GENAU dasselbe, was die Funktionen in
# bae_auswertung_grafiken_standalone.R intern tun – nur offen hingeschrieben.
# Jede Funktion von dort ist hier ein eigener, benannter Abschnitt:
#
#   .bae_prep()                 -> Abschnitte 4–10  (filtern, zerlegen, filtern, ordnen)
#   .bae_add_stufe()/.map_val() -> Abschnitt 12     (Kategorie + Stufe je Stufigkeit)
#   bae_konsens_function()      -> Abschnitte 13/13b (Skizze 1a Balken / 1b Kurve)
#   bae_modus_matrix_function() -> Abschnitte 14/15 (je Gruppe / facettiert)
#   .bae_ref_levels()           -> Abschnitt 16     (feste Achsen-Sortierung)
#   bae_modus_facet_paar()      -> Abschnitt 17     (zwei Stufen nebeneinander)
#   bae_auswertung_grafiken()   -> einfach 13 + 14 nacheinander
# ----------------------------------------------------------------------------


# ----  1  PAKETE ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
# patchwork erst in Abschnitt 17 (Facet-Paar) nötig.
browser()

# ----  0  PARAMETER  (hier anpassen) ----
data          <- heatmap_data_filter #"meine_bae_daten.csv"   # <- deine CSV-Datei
master_id      <- "NR_130_08_66519"        # <- ein Beispiel-Standort
stufe          <- "4st"                    # "3st" | "4st" | "5st" | "2st"
rcp_zukunft_ab <- 2021                     # RCP: nur Zeiträume ab diesem Jahr
obs_alle       <- TRUE                     # TRUE = OBS-Läufe unabhängig vom Zeitraum behalten
szenarien      <- c("OBS", "RCP45", "RCP85")  # Basis-Szenarien behalten; NULL = alle
szen_rename    <- character(0)             # z. B. c("OBS" = "Referenz", "RCP45_v3" = "RCP45_real")
trennung       <- "klimalauf"              # "klimalauf" | "zeit" | "szenario" | "keine"
werte_anzeigen <- TRUE                     # Anzahl je Kachel beschriften (nur wenn Gruppe > 1 Lauf)
legend_pos     <- "right"                  # Legendenposition ("right","bottom","none",…)

# Hinweis (= Rechenmethode) -> Zeilen-Label in der Matrix. Jede echte Methode
# wird eine eigene Zeile "TVx (Label)" (TV6 hat z. B. KHorg/KHff/KM = 3 Zeilen);
# ein leeres Label ("") legt in die Standardzeile "TVx" zusammen. AltBA/BAE20/WKE
# betreffen nur einzelne Baumarten -> leeres Label -> zusammengelegt. Ein leerer
# Hinweis fällt automatisch auf die Standardzeile zurück; KEIN ""-Eintrag hier,
# da c("" = ...) in R einen Fehler wirft (leerer Variablenname).
hinweis_row <- c(
  "KHoriginal"    = "KHorg",
  "KHformfitting" = "KHff",
  "KM"            = "KM",
  "kor"           = "kor",
  "AltBA"         = "",
  "BAE20"         = "",
  "WKE"           = ""
)


# ----  3  KONSTANTEN: Farben, Stufen-Mappings, TV-Farben, Baumart-Kürzel ----
# Kachelfarben der Empfehlungs-Kategorien (identisch zur Heatmap)
custom_palette <- c(
  "sehr empfohlen"        = "#1A9850",
  "empfohlen"             = "#A6D96A",
  "mäßig empfohlen"       = "#FEE08B",
  "wenig empfohlen"       = "#FDAE61",
  "nicht empfohlen"       = "#A50026",
  "pBv"                   = "#404040",
  "Keine Datengrundlage"  = "#B0B0B0"
)

# Code (in BAE_xST) -> Kategorie. Achtung: Code 1 = BESTE Bewertung.
# "2st" = binär aus BAE_3ST: NUR Code 3 = "nicht empfohlen", 1-2 = "empfohlen".
maps <- list(
  "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
  "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "nicht empfohlen"),
  "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "wenig empfohlen", "5" = "nicht empfohlen"),
  "2st" = c("1" = "empfohlen", "2" = "empfohlen", "3" = "nicht empfohlen")
)

# Kategorien von SCHLECHT (unten/1) nach GUT (oben/n). Position = Zahlenwert
# der Empfehlungsstufe UND Gewicht für die Sortierung.
kat_order <- list(
  "3st" = c("nicht empfohlen", "mäßig empfohlen", "sehr empfohlen"),
  "4st" = c("nicht empfohlen", "mäßig empfohlen", "empfohlen", "sehr empfohlen"),
  "5st" = c("nicht empfohlen", "wenig empfohlen", "mäßig empfohlen",
            "empfohlen", "sehr empfohlen"),
  "2st" = c("nicht empfohlen", "empfohlen")
)

# welche Spalte gehört zur gewählten Stufe? (2st nutzt die 3-stufige Spalte)
bae_col_map <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST",
                 "2st" = "BAE_3ST")

# Farben für die (bis zu 12) TV-Linien in Skizze 1
tv_colors <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#1F78B4", "#B2182B", "#33A02C", "#6A3D9A")

# Baumart-Anzeigenamen (Kürzel wie in der Grafik). Nicht gelistete bleiben unverändert.
baumart_labels <- c(
  "Fi"  = "GFI", "Ta"  = "WTA", "Bah" = "BAH", "La"  = "ELA",
  "Dgl" = "GDG", "Rei" = "REI", "Tei" = "TEI", "Bu"  = "RBU",
  "Hbu" = "HBU", "Ki"  = "GKI", "Bi"  = "GBI", "Sei" = "SEI")


# ----  4  AUF MASTER_ID FILTERN ----
d <- data %>% filter(as.character(MASTER_ID) == as.character(master_id))
stopifnot(nrow(d) > 0)   # sonst: keine Daten für diese MASTER_ID
# ansehen:  nrow(d) ; head(d) ; table(d$Hinweis, d$TV)


# ----  5  KLIMALAUF ZERLEGEN  (Szenario / Modell / Zeitraum / Variante) ----
# Beispiel "RCP45-v3_MPICLM_2021-2050":
#   Zeitraum = "2021-2050", Variante = "v3", Szenario = "RCP45",
#   Modell = "MPICLM", Szen_label = "RCP45_v3"
d <- d %>%
  mutate(
    Klimalauf  = as.character(Klimalauf),
    Zeitraum   = str_extract(Klimalauf, "\\d{4}-\\d{4}"),
    Variante   = tolower(str_extract(Klimalauf, "[vV][0-9]+")),
    Szenario   = str_remove(str_extract(Klimalauf, "^[^_]+"), "[-_]?[vV][0-9]+$"),
    Modell     = str_remove(str_remove(Klimalauf, "^[^_]+_"), "_?\\d{4}-\\d{4}.*$"),
    Szen_label = ifelse(is.na(Variante), Szenario, paste0(Szenario, "_", Variante)),
    Startjahr  = suppressWarnings(as.integer(str_sub(Zeitraum, 1, 4)))
  )

# Zeilen ohne erkennbaren Zeitraum verwerfen (sonst kein Startjahr).
d <- d %>% filter(!is.na(Zeitraum))
# ansehen:
#   d %>% distinct(Klimalauf, Szenario, Modell, Zeitraum, Variante, Szen_label)


# ----  6  SZENARIO-LABELS UMBENENNEN  (optional, szen_rename) ----
# benannter Vektor c("<intern>" = "<Anzeige>"); leer = nichts umbenennen.
if (length(szen_rename) > 0) {
  idx     <- match(d$Szen_label, names(szen_rename))
  treffer <- !is.na(idx)
  d$Szen_label[treffer] <- unname(szen_rename[idx[treffer]])
}
# ansehen:  table(d$Szen_label)


# ----  7  SZENARIEN-FILTER  (Basis-Szenario, OBS/RCP45/RCP85) ----
# Auf der BASIS Szenario filtern -> RCP45 schließt RCP45-v3 mit ein.
# szenarien = NULL behält alle.
if (!is.null(szenarien)) {
  d <- d %>% filter(Szenario %in% szenarien)
  stopifnot(nrow(d) > 0)
}
# ansehen:  table(d$Szenario)


# ----  8  RCP NUR ZUKUNFT  (OBS optional komplett) ----
# RCP: nur Läufe ab rcp_zukunft_ab. OBS: bei obs_alle = TRUE komplett behalten,
# sonst ebenfalls erst ab rcp_zukunft_ab.
d <- d %>%
  filter(!grepl("^RCP", Szenario, ignore.case = TRUE) |
           (!is.na(Startjahr) & Startjahr >= rcp_zukunft_ab))
if (!obs_alle) {
  d <- d %>%
    filter(!grepl("^OBS", Szenario, ignore.case = TRUE) |
             (!is.na(Startjahr) & Startjahr >= rcp_zukunft_ab))
}
stopifnot(nrow(d) > 0)
# ansehen:  table(d$Szen_label, d$Zeitraum)


# ----  9  BAUMART -> ANZEIGE-KÜRZEL ----
# c("<intern>" = "<Anzeige>"); nicht gelistete Baumarten behalten ihren Namen.
d <- d %>%
  mutate(Baumart = coalesce(unname(baumart_labels[as.character(Baumart)]),
                            as.character(Baumart)))
# ansehen:  sort(unique(d$Baumart))


# ----  10  FAKTOREN ORDNEN  (feste Reihenfolge der Achsen) ----
d <- d %>%
  mutate(
    Szen_label = factor(Szen_label, levels = sort(unique(Szen_label))),
    Zeitraum   = factor(Zeitraum,   levels = sort(unique(Zeitraum))),
    TV         = factor(paste0("TV", TV), levels = sort(unique(paste0("TV", TV)))),
    Baumart    = factor(Baumart,    levels = sort(unique(as.character(Baumart)))),
    ist_rcp    = grepl("^RCP", Szenario, ignore.case = TRUE)
  )


# ----  11  METHODE -> EIGENE ZEILE  (Spalte Hinweis -> TV_M) ----
# Jede echte Rechenmethode wird eine eigene Zeile: TV6 (KHorg) / TV6 (KHff) /
# TV6 (KM). AltBA/BAE20/WKE betreffen nur einzelne Baumarten -> leeres Label ->
# in die Standardzeile "TVx" zusammengelegt. Nicht gelistete Hinweise dienen
# sich selbst als Label.
if (!"Hinweis" %in% names(d)) d$Hinweis <- ""
d <- d %>%
  mutate(
    Hinweis = coalesce(as.character(Hinweis), ""),
    Methode = coalesce(unname(hinweis_row[Hinweis]), Hinweis),
    TV_M    = ifelse(Methode == "", as.character(TV),
                     paste0(as.character(TV), " (", Methode, ")")))

# Modell-Kürzel (für Dateinamen/Untertitel): alle Modelle mit "-" verbunden.
modelle_str <- {
  m <- sort(unique(as.character(d$Modell)))
  m <- m[!is.na(m) & nzchar(m)]
  if (length(m)) paste(m, collapse = "-") else "NA"
}
# ansehen:  table(d$TV_M) ; modelle_str


# ----  12  STUFE WÄHLEN: Wert + Kategorie + Stufe ----
# Diesen Block je Stufigkeit erneut laufen lassen (stufe <- "2st"/"3st"/…).
# code       -> Kategorie (Text): sehr empfohlen … pBv / Keine Datengrundlage.
# Kategorie  -> Stufe (Zahl): match() gibt Position in `ordn`, nicht empfohlen = 1
#              … sehr empfohlen = n. Für die Kurven-y-Achse UND die Gewichtung.
# Wert       = Einteilung DIREKT wie in BAE_xST (1 = beste … n = schlechteste),
#              pBv/leer/nicht-numerisch -> NA. Nur zum Ansehen.
bae_col <- bae_col_map[[stufe]]
mapping <- maps[[stufe]]
ordn    <- kat_order[[stufe]]

d <- d %>%
  mutate(
    code      = as.character(.data[[bae_col]]),
    Wert      = suppressWarnings(as.integer(code)),   # 1 = best … n = schlecht
    Kategorie = case_when(
      code %in% names(mapping) ~ unname(mapping[code]),
      code == "pBv"            ~ "pBv",
      TRUE                     ~ "Keine Datengrundlage"),
    Stufe     = match(Kategorie, ordn)                # invertiert: hoch = gut
  )
# ansehen:
#   d %>% count(code, Wert, Kategorie, Stufe)
#   table(d$Kategorie, useNA = "ifany")


# ----  13  SKIZZE 1a – KONSENS-BALKEN je Baumart (Anteil der TVs) ----
# ERSETZT die alten TV-Spline-Kurven: ein Spline über die kategoriale Baumart-
# Achse interpoliert Werte, die es nicht gibt (Baumarten sind ungeordnet, es gibt
# nichts "zwischen" GBI und GDG) -> unlesbares Kurven-Wirrwarr. Stattdessen die
# eigentliche Frage direkt: wie viele TVs empfehlen eine Baumart, und wie einig
# sind sie sich?  Pro (Gruppe, Baumart) auszählen, wie viele TVs in jeder Kategorie
# landen, als GESTAPELTER ANTEILSBALKEN. Großer grüner Block = breit empfohlen;
# gemischte Farben = TVs uneinig.

# GRUPPIERUNG (Facetten) über `trennung` – wie die Modus-Matrix (Abschnitt 0/14):
#   "klimalauf" -> ein Panel je Klimalauf (Default)
#   "zeit"      -> Vergangenheit (OBS) vs. Zukunft (RCP), über Klimaläufe aggregiert
#   "szenario"  -> je Szen_label ein Panel;   "keine" -> ein gemeinsames Panel
d <- d %>%
  mutate(Gruppe = switch(trennung,
    "klimalauf" = as.character(Klimalauf),
    "zeit"      = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
    "szenario"  = as.character(Szen_label),
    "keine"     = "alle"))
gruppen <- sort(unique(d$Gruppe))
# ansehen:  gruppen ; table(d$Gruppe)

# Baumarten GEWICHTET sortieren (wie die Modus-Matrix): Summe der Stufe je Baumart
# = Nennungen × Empfehlungsstufe (sehr empfohlen zählt am meisten; pBv / Keine
# Datengrundlage = 0), GLOBAL über alle Gruppen. Aufsteigend -> schwächster
# Konsens links, bestempfohlene Baumart rechts. Gilt für Balken UND Kurve.
ba_ord <- d %>%
  group_by(Baumart) %>%
  summarise(s = sum(coalesce(as.numeric(Stufe), 0)), .groups = "drop") %>%
  arrange(s, Baumart)
ba_levels <- as.character(ba_ord$Baumart)              # links = schwach … rechts = empfohlen
kat_lv    <- c(ordn, "pBv", "Keine Datengrundlage")    # Fill-/Legenden-Reihenfolge
# ansehen:  ba_ord

# eine Kategorie je (Gruppe, Baumart, TV): häufigste (Modus) über die Hinweis-
# Methoden (und, falls trennung aggregiert, über die Klimaläufe der Gruppe)
tv_kat <- d %>%
  count(Gruppe, Baumart, TV, Kategorie, name = "n") %>%
  group_by(Gruppe, Baumart, TV) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()
# ansehen:  tv_kat

# je (Gruppe, Baumart) die TVs pro Kategorie auszählen
konsens <- tv_kat %>%
  count(Gruppe, Baumart, Kategorie, name = "n_tv") %>%
  mutate(Kategorie = factor(Kategorie, levels = kat_lv),
         Baumart   = factor(as.character(Baumart), levels = ba_levels),   # empfohlene rechts
         Gruppe    = factor(as.character(Gruppe),  levels = gruppen))
# ansehen:  konsens

p_balken <-
  ggplot(konsens, aes(x = Baumart, y = n_tv, fill = Kategorie)) +
  geom_col(position = "fill", width = 0.9) +
  facet_wrap(~ Gruppe) +
  scale_fill_manual(values = custom_palette, limits = kat_lv, drop = FALSE) +
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%")) +
  labs(title    = paste0("BAE – Konsens der TVs je Baumart – ", master_id),
       subtitle = paste0(sub("st$", "", stufe), "-stufig  |  je ", trennung,
                         "  |  Anteil der TVs je Empfehlung  |  empfohlene Baumarten rechts →  |  Modell: ",
                         modelle_str),
       x = "Baumart  (bestempfohlene →)", y = "Anteil der TVs", fill = "Empfehlung") +
  theme_minimal(base_size = 11) +
  theme(strip.text       = element_text(face = "bold", size = 9),
        strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.6),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        plot.background  = element_rect(fill = "white", color = NA))

p_balken                                   # im Plot-Fenster ansehen
# ggsave(paste0("KonsensBalken_", stufe, "_", master_id, "_", modelle_str, ".png"),
#        p_balken, width = 34, height = 22, units = "cm", dpi = 150)


# ----  13b  SKIZZE 1b – KONSENS-KURVE (Median-Stufe + Spannband der TVs) ----
# Ehrliche Variante der alten Kurven: KEIN Spline, gerade Segmente. Statt 12
# verschlungener TV-Linien pro (Gruppe, Baumart) EINE Konsens-Linie (Median der
# TV-Stufen) + graues Band (min..max über die TVs). Schmales Band = TVs einig,
# breites Band = uneinig. Graue Punkte = die einzelnen TV-Werte (Streuung).
# Gleiche Gruppierung (trennung) und gleiche Baumart-Sortierung wie der Balken.

# ein Stufen-Wert je (Gruppe, TV, Baumart): Mittel über Hinweis-Methoden (und
# Klimaläufe der Gruppe)
tv_val <- d %>%
  group_by(Gruppe, TV, Baumart) %>%
  summarise(y = mean(Stufe, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(y)) %>%
  mutate(x      = as.integer(factor(Baumart, levels = ba_levels)),
         Gruppe = factor(as.character(Gruppe), levels = gruppen))
# ansehen:  tv_val

# je (Gruppe, Baumart): Median + Spannweite ("Konfidenzintervall" der TVs)
konsens_band <- tv_val %>%
  group_by(Gruppe, Baumart, x) %>%
  summarise(y_med = median(y), y_min = min(y), y_max = max(y), .groups = "drop")
# ansehen:  konsens_band

p_konsens <-
  ggplot() +
  geom_ribbon(data = konsens_band, aes(x = x, ymin = y_min, ymax = y_max),
              fill = "grey70", alpha = 0.35) +
  geom_point(data = tv_val, aes(x = x, y = y), color = "grey45", size = 0.8, alpha = 0.5) +
  geom_line(data = konsens_band, aes(x = x, y = y_med), color = "#1A9850", linewidth = 1) +
  geom_point(data = konsens_band, aes(x = x, y = y_med), color = "#1A9850", size = 1.6) +
  facet_wrap(~ Gruppe) +
  scale_x_continuous(breaks = seq_along(ba_levels), labels = ba_levels) +
  scale_y_continuous(breaks = seq_along(ordn), labels = ordn,
                     limits = c(1, length(ordn)), expand = expansion(mult = 0.05)) +
  labs(title    = paste0("BAE – Konsens-Kurve der TVs – ", master_id),
       subtitle = paste0(sub("st$", "", stufe), "-stufig  |  je ", trennung,
                         "  |  Linie = Median, Band = Spannweite der TVs  |  empfohlene Baumarten rechts →  |  Modell: ",
                         modelle_str),
       x = "Baumart  (bestempfohlene →)", y = "Empfehlung") +
  theme_minimal(base_size = 11) +
  theme(strip.text        = element_text(face = "bold", size = 9),
        strip.background   = element_rect(fill = "grey95", color = "grey70", linewidth = 0.6),
        panel.border       = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "grey92"),
        plot.background    = element_rect(fill = "white", color = NA))

p_konsens                                  # im Plot-Fenster ansehen
# ggsave(paste0("KonsensKurve_", stufe, "_", master_id, "_", modelle_str, ".png"),
#        p_konsens, width = 34, height = 22, units = "cm", dpi = 150)


# ----  14  SKIZZE 2 – HÄUFIGSTE EMPFEHLUNG je Gruppe (trennung) ----
# Genau wie die Heatmap: NICHT global aggregieren, sondern je GRUPPE eine Matrix.
# KEIN Score – es wird nur AUSGEZÄHLT. Zeilen sind TV × Methode (Abschnitt 11).
# Die Gruppe kommt aus `trennung`:
#   "klimalauf" -> je Klimalauf eine Matrix (unaggregiert wie die Heatmap). [Default]
#   "zeit"      -> Vergangenheit (OBS) vs. Zukunft (RCP), über Klimaläufe gezählt
#   "szenario"  -> je Szen_label eine Matrix (über die Zeiträume gezählt)
#   "keine"     -> eine gemeinsame Matrix über alles
kat_lv   <- c(ordn, "pBv", "Keine Datengrundlage")      # Legenden-/Fill-Reihenfolge
kat_pref <- c(rev(ordn), "pBv", "Keine Datengrundlage") # best -> schlecht (Gleichstand: bessere gewinnt)
dunkel   <- c("sehr empfohlen", "nicht empfohlen", "pBv")      # Kacheln mit weißer Schrift

d <- d %>%
  mutate(Gruppe = switch(trennung,
                         "klimalauf" = as.character(Klimalauf),
                         "zeit"      = ifelse(ist_rcp, "Zukunft", "Vergangenheit"),
                         "szenario"  = as.character(Szen_label),
                         "keine"     = "alle"))

# alphabetisch: OBS… vor RCP…, Zeiträume chronologisch
gruppen <- sort(unique(d$Gruppe))
# ansehen:  gruppen ; table(d$Gruppe)

# Zum Durchklicken EINE Gruppe setzen und den Rumpf einzeln laufen lassen,
# z. B.:  grp <- gruppen[1]
for (grp in gruppen) {
  
  # 14a. nur diese Gruppe; je Zelle (Zeile TV×Methode × Baumart) AUSZÄHLEN
  zaehl <- d %>%
    filter(Gruppe == grp) %>%
    count(TV_M, Baumart, Kategorie, name = "n")
  if (nrow(zaehl) == 0) next
  # ansehen:  zaehl
  
  # 14b. Kachel = häufigste Kategorie (Modus). Gleichstand -> bessere (kleinster
  #      kat_pref) + Markierung tie.
  kachel <- zaehl %>%
    group_by(TV_M, Baumart) %>%
    mutate(tie = sum(n == max(n)) > 1) %>%
    filter(n == max(n)) %>%
    slice_min(match(Kategorie, kat_pref), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Kategorie = factor(Kategorie, levels = kat_lv),
           label     = ifelse(tie, paste0(n, "*"), as.character(n)),
           txt_col   = ifelse(as.character(Kategorie) %in% dunkel, "white", "grey15"))
  # ansehen:  kachel
  
  # 14c. Sortierung GEWICHTET (dunkelgrün zählt am meisten): Summe der Stufe je
  #      Zeile (TV×Methode)/Baumart (sehr empfohlen = max … nicht empfohlen = 1;
  #      pBv/leer = 0). Aufsteigend -> beste (höchste Summe) als letzter Faktor-
  #      Level -> beste Zeile oben (y), beste Baumart rechts (x).
  gew     <- d %>% filter(Gruppe == grp) %>% mutate(w = coalesce(as.numeric(Stufe), 0))
  tv_rang <- gew %>% group_by(TV_M)    %>% summarise(s = sum(w), .groups = "drop") %>% arrange(s, TV_M)
  ba_rang <- gew %>% group_by(Baumart) %>% summarise(s = sum(w), .groups = "drop") %>% arrange(s, Baumart)
  # ansehen:  tv_rang ; ba_rang
  
  kachel <- kachel %>%
    mutate(TV_M    = factor(as.character(TV_M),    levels = as.character(tv_rang$TV_M)),
           Baumart = factor(as.character(Baumart), levels = as.character(ba_rang$Baumart)))
  
  # 14d. Zahl nur zeigen, wenn die Gruppe MEHRERE Klimaläufe zusammenfasst
  #      (bei trennung = "klimalauf" ist alles zwangsläufig 1 -> weglassen).
  n_laeufe    <- n_distinct(gew$Klimalauf)
  zahl_zeigen <- werte_anzeigen && n_laeufe > 1
  
  # Modell + Szenarien/Zeiträume NUR aus dieser Gruppe (nicht global).
  modelle_grp <- {
    mm <- sort(unique(as.character(gew$Modell))); mm <- mm[!is.na(mm) & nzchar(mm)]
    if (length(mm)) paste(mm, collapse = "-") else "NA" }
  sz       <- gew %>% distinct(Szen_label, ist_rcp) %>% arrange(ist_rcp, Szen_label)
  szen_grp <- paste(sub("^OBS", "Referenz", as.character(sz$Szen_label)), collapse = "/")
  zeit_grp <- paste(sort(unique(as.character(gew$Zeitraum))), collapse = ", ")
  grp_info <- switch(trennung,
                     "zeit"      = paste0(" (", szen_grp, "; ", zeit_grp, ")"),
                     "keine"     = paste0(" (", szen_grp, "; ", zeit_grp, ")"),
                     "szenario"  = paste0(" (", zeit_grp, ")"),
                     "klimalauf" = "")
  
  # 14e. EIN durchgehender ggplot-Aufruf
  p_matrix <-
    ggplot(kachel, aes(x = Baumart, y = TV_M)) +
    geom_tile(aes(fill = Kategorie), color = "white", linewidth = 0.6) +
    geom_tile(data = filter(kachel, tie), fill = NA, color = "grey15", linewidth = 1.1) +
    {if (zahl_zeigen) geom_text(aes(label = label, colour = txt_col), size = 3)} +
    scale_fill_manual(values = custom_palette, limits = kat_lv, drop = FALSE) +
    scale_colour_identity() +
    coord_equal() +
    labs(title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
         subtitle = paste0(sub("st$", "", stufe), "-stufig  |  ", grp, grp_info, "  |  Modell: ", modelle_grp),
         x = "Baumart  (beste Empfehlungen →)",
         y = "TV × Methode  (meiste Empfehlungen oben ↑)",
         fill = "häufigste Kategorie") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
          axis.text.y     = element_text(face = "bold", size = 9),
          panel.grid      = element_blank(),
          legend.position = legend_pos,
          plot.background = element_rect(fill = "white", color = NA))
  
  print(p_matrix)                          # im Plot-Fenster ansehen
  # grp_tag <- gsub("[^A-Za-z0-9]+", "-", grp)
  # ggsave(paste0("ModusMatrix_", stufe, "_", grp_tag, "_", master_id, "_", modelle_grp, ".png"),
  #        p_matrix, width = 22, height = 16, units = "cm", dpi = 150)
}


# ----  15  SKIZZE 2 FACETTIERT  (alle Gruppen in EINER Grafik) ----
# Wie die Kurvengrafik: ALLE Gruppen als facet_wrap in EINE PNG statt einzelner
# Dateien. Achsen sind gemeinsam -> GLOBAL (über alle Gruppen) gewichtet sortiert.

# 15a. Auszählen wie oben, aber Gruppe bleibt erhalten -> Modus je (Gruppe, Zelle).
kachel_f <- d %>%
  count(Gruppe, TV_M, Baumart, Kategorie, name = "n") %>%
  group_by(Gruppe, TV_M, Baumart) %>%
  mutate(tie = sum(n == max(n)) > 1) %>%
  filter(n == max(n)) %>%
  slice_min(match(Kategorie, kat_pref), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Kategorie = factor(Kategorie, levels = kat_lv),
         label     = ifelse(tie, paste0(n, "*"), as.character(n)),
         txt_col   = ifelse(as.character(Kategorie) %in% dunkel, "white", "grey15"))

# 15b. GLOBAL gewichtet sortiert (gemeinsame Achsen über alle Facetten).
gew_f  <- d %>% mutate(w = coalesce(as.numeric(Stufe), 0))
tv_ord <- gew_f %>% group_by(TV_M)    %>% summarise(s = sum(w), .groups = "drop") %>% arrange(s, TV_M)
ba_ord <- gew_f %>% group_by(Baumart) %>% summarise(s = sum(w), .groups = "drop") %>% arrange(s, Baumart)

kachel_f <- kachel_f %>%
  mutate(TV_M    = factor(as.character(TV_M),    levels = as.character(tv_ord$TV_M)),
         Baumart = factor(as.character(Baumart), levels = as.character(ba_ord$Baumart)),
         Gruppe  = factor(as.character(Gruppe),  levels = gruppen))

# 15c. Zahl nur zeigen, wenn mind. eine Gruppe mehrere Klimaläufe zusammenfasst.
laeufe_je_grp <- d %>% distinct(Gruppe, Klimalauf) %>% count(Gruppe)
zahl_zeigen_f <- werte_anzeigen && any(laeufe_je_grp$n > 1)
facet_ncol    <- ceiling(sqrt(length(gruppen)))   # z. B. 1 = alle untereinander

# 15d. EIN durchgehender ggplot-Aufruf
p_facet <-
  ggplot(kachel_f, aes(x = Baumart, y = TV_M)) +
  geom_tile(aes(fill = Kategorie), color = "white", linewidth = 0.6) +
  geom_tile(data = filter(kachel_f, tie), fill = NA, color = "grey15", linewidth = 1.1) +
  {if (zahl_zeigen_f) geom_text(aes(label = label, colour = txt_col), size = 3)} +
  facet_wrap(~ Gruppe, ncol = facet_ncol) +
  scale_fill_manual(values = custom_palette, limits = kat_lv, drop = FALSE) +
  scale_colour_identity() +
  coord_equal() +
  labs(title    = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
       subtitle = paste0(sub("st$", "", stufe), "-stufig  |  facettiert je Gruppe (", trennung,
                         ")  |  Modell: ", modelle_str),
       x = "Baumart  (beste Empfehlungen →)",
       y = "TV × Methode  (meiste Empfehlungen oben ↑)",
       fill = "häufigste Kategorie") +
  theme_minimal(base_size = 11) +
  theme(strip.text       = element_text(face = "bold", size = 9),
        strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.6),
        axis.text.x      = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
        axis.text.y      = element_text(face = "bold", size = 9),
        panel.grid       = element_blank(),
        legend.position  = legend_pos,
        plot.background  = element_rect(fill = "white", color = NA))

p_facet
# ggsave(paste0("ModusMatrix_facet_", stufe, "_", master_id, "_", modelle_str, ".png"),
#        p_facet, width = 30, height = 24, units = "cm", dpi = 150)


# ----  16  FESTE ACHSEN-SORTIERUNG  (order_ref) ----
# Sollen mehrere Grafiken DIESELBE Achsenaufteilung nutzen (z. B. binär links,
# 4-stufig rechts an gleicher Stelle), berechnet man die TV×Methode- und
# Baumart-Reihenfolge EINMAL aus einer Referenz-Stufe und übernimmt sie überall.
# (In den Abschnitten 14/15 dann statt der je-Grafik-Sortierung
#  levels = union(ref$tv, <eigene>) bzw. union(ref$ba, <eigene>) setzen.)
order_ref <- "2st"                           # Referenz-Stufe (robusteste Abdeckung)
ref_col   <- bae_col_map[[order_ref]]
ref_ordn  <- kat_order[[order_ref]]

d_ref <- d %>%
  mutate(
    Kategorie_ref = { rc <- as.character(.data[[ref_col]])
    case_when(rc %in% names(maps[[order_ref]]) ~ unname(maps[[order_ref]][rc]),
              rc == "pBv"                      ~ "pBv",
              TRUE                             ~ "Keine Datengrundlage") },
    Stufe_ref = match(Kategorie_ref, ref_ordn),
    w         = coalesce(as.numeric(Stufe_ref), 0))

ref <- list(
  tv = d_ref %>% group_by(TV_M)    %>% summarise(s = sum(w), .groups = "drop") %>%
    arrange(s, TV_M) %>% pull(TV_M) %>% as.character(),
  ba = d_ref %>% group_by(Baumart) %>% summarise(s = sum(w), .groups = "drop") %>%
    arrange(s, Baumart) %>% pull(Baumart) %>% as.character())
# ansehen:  ref$tv ; ref$ba


# ----  17  FACET-PAAR  (binär + 4-stufig nebeneinander, patchwork) ----
# Zwei facettierte Modus-Matrizen unter EINER Überschrift, mit der GEMEINSAMEN
# Referenz-Sortierung aus Abschnitt 16 (order_ref) -> beide Seiten direkt
# vergleichbar. Schleife über die zwei Stufen (jede baut ihre eigene Facet-Grafik).
library(patchwork)
stufen_paar    <- c("2st", "4st")
facet_ncol_paar <- 1                          # Gruppen untereinander

facet_plots <- list()
for (st in stufen_paar) {
  ordn_st <- kat_order[[st]]
  kat_lv_st   <- c(ordn_st, "pBv", "Keine Datengrundlage")
  kat_pref_st <- c(rev(ordn_st), "pBv", "Keine Datengrundlage")
  
  # Kategorie + Stufe für DIESE Stufigkeit (Abschnitt 12 je st)
  d_st <- d %>%
    mutate(
      code_st = as.character(.data[[ bae_col_map[[st]] ]]),
      Kategorie = case_when(
        code_st %in% names(maps[[st]]) ~ unname(maps[[st]][code_st]),
        code_st == "pBv"               ~ "pBv",
        TRUE                           ~ "Keine Datengrundlage"),
      Stufe = match(Kategorie, ordn_st))
  
  # Modus je (Gruppe, Zelle) wie Abschnitt 15a
  kachel_st <- d_st %>%
    count(Gruppe, TV_M, Baumart, Kategorie, name = "n") %>%
    group_by(Gruppe, TV_M, Baumart) %>%
    mutate(tie = sum(n == max(n)) > 1) %>%
    filter(n == max(n)) %>%
    slice_min(match(Kategorie, kat_pref_st), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Kategorie = factor(Kategorie, levels = kat_lv_st),
           # feste Referenz-Achsen (Abschnitt 16); union hängt evtl. Fehlende an
           TV_M    = factor(as.character(TV_M),    levels = union(ref$tv, unique(as.character(TV_M)))),
           Baumart = factor(as.character(Baumart), levels = union(ref$ba, unique(as.character(Baumart)))),
           Gruppe  = factor(as.character(Gruppe),  levels = gruppen))
  
  lab_st <- paste0(sub("st$", "", st), "-stufig")
  facet_plots[[st]] <-
    ggplot(kachel_st, aes(x = Baumart, y = TV_M)) +
    geom_tile(aes(fill = Kategorie), color = "white", linewidth = 0.6) +
    geom_tile(data = filter(kachel_st, tie), fill = NA, color = "grey15", linewidth = 1.1) +
    facet_wrap(~ Gruppe, ncol = facet_ncol_paar) +
    scale_fill_manual(values = custom_palette, limits = kat_lv_st, drop = FALSE) +
    coord_equal() +
    labs(title = lab_st, x = "Baumart  (beste →)", y = "TV × Methode  (beste oben ↑)",
         fill = "häufigste Kategorie") +
    theme_minimal(base_size = 11) +
    theme(strip.text       = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.6),
          axis.text.x      = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
          axis.text.y      = element_text(face = "bold", size = 9),
          panel.grid       = element_blank(),
          plot.background  = element_rect(fill = "white", color = NA))
}

# Größere Schrift für die große Kombigrafik + Legende in 2 Zeilen umbrechen,
# sonst läuft die 6-teilige 4st-Legende über den rechten Rand hinaus.
groesser <- theme(
  plot.title   = element_text(size = 25, face = "bold"),
  strip.text   = element_text(size = 25, face = "bold"),
  axis.title   = element_text(size = 25),
  axis.text.y  = element_text(size = 25, face = "bold"),
  axis.text.x  = element_text(size = 25, face = "bold", angle = 45, hjust = 1),
  legend.text  = element_text(size = 20),
  legend.title = element_text(size = 25))

p_paar <- (facet_plots[[stufen_paar[1]]] + facet_plots[[stufen_paar[2]]]) +
  plot_layout(ncol = 2) & groesser &
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
p_paar <- p_paar +
  plot_annotation(
    title    = paste0("BAE – häufigste Empfehlung – ", master_id),
    subtitle = paste0("links: ", sub("st$", "", stufen_paar[1]), "-stufig  |  rechts: ",
                      sub("st$", "", stufen_paar[2]), "-stufig  |  Modell: ", modelle_str),
    theme = theme(plot.title      = element_text(size = 24, face = "bold"),
                  plot.subtitle   = element_text(size = 16),
                  plot.background = element_rect(fill = "white", color = NA)))

p_paar
# ggsave(paste0("ModusMatrix_facetpaar_2st-4st_", master_id, "_", modelle_str, ".png"),
#        p_paar, width = 60, height = 40, units = "cm", dpi = 150)