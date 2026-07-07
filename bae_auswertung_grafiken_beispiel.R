# ============================================================================
# bae_auswertung_grafiken_beispiel.R
# ----------------------------------------------------------------------------
# FLACHER SCHRITT-FÜR-SCHRITT-DURCHLAUF für EINE Beispiel-Datei / MASTER_ID.
#
# Kein Funktions-Wrapper: jeden Block der Reihe nach markieren und ausführen,
# dann das Zwischenergebnis ansehen (z. B. head(d), View(d), table(...)).
# Dieses Skript zeigt GENAU dasselbe, was die Funktionen in
# bae_auswertung_grafiken_standalone.R intern tun – nur offen hingeschrieben.
#
# Erzeugt:
#   Skizze 1  – Empfehlungs-Kurven je TV (pro Szenario × Zeitraum)
#   Skizze 2  – Modus-Matrix, unaggregiert je Klimalauf (ausgezählt + sortiert)
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


# ============================================================================
# 0.  PARAMETER  – hier anpassen
# ============================================================================
datei          <- "meine_bae_daten.csv"   # <- deine CSV-Datei
master_id      <- "NR_130_08_66519"        # <- ein Beispiel-Standort
stufe          <- "4st"                    # "3st" | "4st" | "5st"
rcp_zukunft_ab <- 2021                     # RCP: nur Zeiträume ab diesem Jahr
# Hinweis (= Rechenmethode) -> Zeilen-Label in der Matrix. Jede echte Methode
# wird eine eigene Zeile "TVx (Label)" (TV6 hat z. B. KHorg/KHff/KM = 3 Zeilen);
# ein leeres Label legt in die Standardzeile "TVx" zusammen. AltBA/BAE20/WKE
# betreffen nur einzelne Baumarten -> leeres Label -> zusammengelegt.
hinweis_row <- c(
  "KHoriginal"    = "KHorg",
  "KHformfitting" = "KHff",
  "KM"            = "KM",
  "kor"           = "kor",
  ""              = "",
  "AltBA"         = "",
  "BAE20"         = "",
  "WKE"           = ""
)


# ============================================================================
# 1.  DATEN LADEN
# ============================================================================
# --- Variante A: echte Datei -----------------------------------------------
# data <- data.table::fread(datei)

# --- Variante B: Demo-Daten zum Ausprobieren (löschen, sobald echte Datei da)
set.seed(1)
data <- expand.grid(
  Baumart   = c("Bah","Bi","Bu","Dgl","Fi","Hbu","Ki","La","Rei","Sei","Ta","Tei"),
  TV        = 1:9,
  # mehrere Einträge je Zelle wie in echt: verschiedene Hinweis-Varianten
  Hinweis   = c("KM","KHoriginal","KHformfitting"),
  Klimalauf = c("OBS_DWD_1961-1990","OBS_DWD_1991-2020",
                "RCP45_MPICLM_2021-2050","RCP45_MPICLM_2071-2100",
                "RCP45-v3_MPICLM_2021-2050","RCP45-v3_MPICLM_2071-2100",
                "RCP85_MPICLM_2021-2050","RCP85_MPICLM_2071-2100"),
  stringsAsFactors = FALSE
)
data$MASTER_ID <- master_id
data$BAE_5ST <- as.character(sample(1:5, nrow(data), replace = TRUE))
data$BAE_5ST[data$TV %in% c(5, 8)] <- "Keine Datengrundlage"   # wie im Bild
data$BAE_5ST[sample(nrow(data), 20)] <- "pBv"
.num <- suppressWarnings(as.integer(data$BAE_5ST))
data$BAE_4ST <- ifelse(is.na(.num), data$BAE_5ST, as.character(pmin(.num, 4)))
data$BAE_3ST <- ifelse(is.na(.num), data$BAE_5ST, as.character(pmin(ceiling(.num/2), 3)))

# ansehen:
#   head(data)
#   str(data)
#   table(data$Klimalauf)
#   table(data$BAE_4ST)
#   # mehrere Zeilen je Zelle (wie in echt durch Hinweis-Varianten):
#   data %>% filter(Baumart=="Bah", TV==6, Klimalauf=="OBS_DWD_1961-1990")


# ============================================================================
# 2.  FARBEN + STUFEN-MAPPINGS  (identisch zur Heatmap)
# ============================================================================
# Kachelfarben der Empfehlungs-Kategorien
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
maps <- list(
  "3st" = c("1" = "sehr empfohlen", "2" = "mäßig empfohlen", "3" = "nicht empfohlen"),
  "4st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "nicht empfohlen"),
  "5st" = c("1" = "sehr empfohlen", "2" = "empfohlen", "3" = "mäßig empfohlen",
            "4" = "wenig empfohlen", "5" = "nicht empfohlen")
)

# Kategorien von SCHLECHT (unten/1) nach GUT (oben/n). Position = Zahlenwert
# der Empfehlungsstufe UND Gewicht für den Score.
kat_order <- list(
  "3st" = c("nicht empfohlen", "mäßig empfohlen", "sehr empfohlen"),
  "4st" = c("nicht empfohlen", "mäßig empfohlen", "empfohlen", "sehr empfohlen"),
  "5st" = c("nicht empfohlen", "wenig empfohlen", "mäßig empfohlen",
            "empfohlen", "sehr empfohlen")
)

# welche Spalte gehört zur gewählten Stufe?
bae_col <- c("3st" = "BAE_3ST", "4st" = "BAE_4ST", "5st" = "BAE_5ST")[[stufe]]

mapping <- maps[[stufe]]        # Code -> Kategorie für die gewählte Stufe
ordn    <- kat_order[[stufe]]   # Kategorie-Reihenfolge für die gewählte Stufe

# ansehen:
#   bae_col ; mapping ; ordn


# ============================================================================
# 3.  AUF MASTER_ID FILTERN
# ============================================================================
d <- data %>% filter(as.character(MASTER_ID) == master_id)

# ansehen:  nrow(d) ; head(d)
#   table(d$Hinweis, d$TV)   # zeigt, welche Methode je TV vorkommt


# ============================================================================
# 3b. METHODE -> EIGENE ZEILE  (Spalte Hinweis)
# ============================================================================
# Jede echte Rechenmethode wird eine eigene Zeile: TV6 (KHorg) / TV6 (KHff) /
# TV6 (KM). AltBA/BAE20/WKE betreffen nur einzelne Baumarten -> leeres Label ->
# in die Standardzeile "TVx" zusammengelegt (füllen dort ihre Baumart-Spalten).
# Nicht gelistete Hinweise dienen sich selbst als Label.
if (!"Hinweis" %in% names(d)) d$Hinweis <- ""
d <- d %>%
  mutate(Hinweis = coalesce(as.character(Hinweis), ""),
         Methode = coalesce(unname(hinweis_row[Hinweis]), Hinweis),
         TV_M    = ifelse(Methode == "", paste0("TV", TV),
                          paste0("TV", TV, " (", Methode, ")")))
# ansehen:  table(d$TV_M)   # welche Zeilen (TV × Methode) entstehen


# ============================================================================
# 4.  KLIMALAUF ZERLEGEN  (Szenario / Modell / Zeitraum / Variante)
# ============================================================================
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

# ansehen:
#   d %>% distinct(Klimalauf, Szenario, Modell, Zeitraum, Variante, Szen_label)


# ============================================================================
# 5.  RCP NUR ZUKUNFT (OBS bleibt komplett)
# ============================================================================
d <- d %>%
  filter(!grepl("^RCP", Szenario, ignore.case = TRUE) | Startjahr >= rcp_zukunft_ab)

# ansehen:  table(d$Szen_label, d$Zeitraum)


# ============================================================================
# 6.  FAKTOREN ORDNEN  (feste Reihenfolge der Achsen)
# ============================================================================
d <- d %>%
  mutate(
    Szen_label = factor(Szen_label, levels = sort(unique(Szen_label))),
    Zeitraum   = factor(Zeitraum,   levels = sort(unique(Zeitraum))),
    TV         = factor(paste0("TV", TV), levels = sort(unique(paste0("TV", TV)))),
    Baumart    = factor(Baumart,    levels = sort(unique(as.character(Baumart)))),
    ist_rcp    = grepl("^RCP", Szenario, ignore.case = TRUE)
  )


# ============================================================================
# 7.  WERT (Einteilung als Skala) + KATEGORIE/STUFE FÜR DIE GEWÄHLTE STUFIGKEIT
# ============================================================================
# code       -> Kategorie (Text): sehr empfohlen … pBv / Keine Datengrundlage.
#              -> für Skizze 2 (Auszählung/Modus) UND die Kurven-Farben.
# Kategorie  -> Stufe (Zahl, INVERTIERT): match() gibt Position in `ordn`,
#              nicht empfohlen = 1 … sehr empfohlen = n. Das ist NUR für die
#              Kurven (Skizze 1), damit "sehr empfohlen" oben liegt.
# Wert       = die Einteilung DIREKT wie in BAE_xST (1 = beste … n = schlechteste)
#              bleibt nur zum Ansehen; Skizze 2 zählt jetzt Kategorien, kein Score.
d <- d %>%
  mutate(
    code      = as.character(.data[[bae_col]]),
    Wert      = suppressWarnings(as.integer(code)),   # 1 = best … n = schlecht
    Kategorie = case_when(
      code %in% names(mapping) ~ unname(mapping[code]),
      code == "pBv"            ~ "pBv",
      TRUE                     ~ "Keine Datengrundlage"),
    Stufe     = match(Kategorie, ordn)
  )

# ansehen:
#   d %>% count(code, Wert, Kategorie, Stufe)   # Code -> Wert/Text/Stufe
#   table(d$Kategorie, useNA = "ifany")


# ============================================================================
# 8.  SKIZZE 1  – EMPFEHLUNGS-KURVEN je TV (glatt)
# ============================================================================
# Idee: pro Panel (Szenario × Zeitraum) für jede TV eine glatte Kurve über die
# Baumarten. y = Stufe (unten "nicht empfohlen", oben "sehr empfohlen").
# Wo sich die Kurven decken, sind sich die TVs einig.
# WICHTIG: Da es je (TV, Baumart) mehrere Hinweis-Zeilen gibt, erst EINEN Wert
# je Zelle bilden (Mittel), sonst zappeln die Linien senkrecht.

# Farben für die TV-Linien
ba_levels <- levels(droplevels(d$Baumart))
tv_levels <- levels(droplevels(d$TV))
tv_farben <- setNames(
  c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02",
    "#A6761D","#666666","#1F78B4","#B2182B","#33A02C","#6A3D9A")[seq_along(tv_levels)],
  tv_levels)

# 8a. ein Wert je (Panel, TV, Baumart): Mittel der Stufe über Hinweis-Varianten
kurv <- d %>%
  group_by(Szen_label, Zeitraum, TV, Baumart) %>%
  summarise(y = mean(Stufe, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(y)) %>%
  mutate(x = as.integer(factor(Baumart, levels = ba_levels)))
# ansehen:  kurv

# 8b. glatte Spline-Kurve je (Panel, TV) durch diese Punkte (auf [1,n] geklammert)
kurv_smooth <- kurv %>%
  group_by(Szen_label, Zeitraum, TV) %>%
  filter(n() >= 2) %>%
  group_modify(~ {
    s <- stats::spline(.x$x, .x$y, n = 200)           # glatte Interpolation
    data.frame(x = s$x, y = pmin(pmax(s$y, 1), length(ordn)))
  }) %>%
  ungroup()
# ansehen:  kurv_smooth

# 8c. EIN durchgehender ggplot-Aufruf – Linie (glatt) + Punkte (echte Werte):
p_kurven <-
  ggplot() +
  geom_line(data = kurv_smooth, aes(x = x, y = y, colour = TV, group = TV),
            linewidth = 0.8, alpha = 0.85) +
  geom_point(data = kurv, aes(x = x, y = y, colour = TV), size = 1.4, alpha = 0.9) +
  facet_grid(Szen_label ~ Zeitraum) +
  scale_x_continuous(breaks = seq_along(ba_levels), labels = ba_levels) +
  scale_y_continuous(breaks = seq_along(ordn), labels = ordn, limits = c(1, length(ordn))) +
  scale_colour_manual(values = tv_farben) +
  labs(title = paste0("BAE-Empfehlungskurven – ", master_id),
       subtitle = paste0(stufe, "-stufig  |  Überlappung der Kurven = Einigkeit der TVs"),
       x = NULL, y = "Empfehlung", colour = "TV") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text  = element_text(face = "bold", size = 9),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA))

p_kurven                                   # im Plot-Fenster ansehen
# ggsave("Kurven_beispiel.png", p_kurven, width = 34, height = 22, units = "cm", dpi = 150)


# ============================================================================
# 9.  SKIZZE 2  – HÄUFIGSTE EMPFEHLUNG (AUSGEZÄHLT), UNAGGREGIERT JE KLIMALAUF
# ============================================================================
# Genau wie die Heatmap: NICHT über Klimaläufe aggregieren, sondern je Klimalauf
# eine eigene Matrix. KEIN Score – es wird nur AUSGEZÄHLT. Zeilen sind TV × Methode
# (aus Abschnitt 3b): TV6 (KHorg)/TV6 (KHff)/TV6 (KM) sind eigene Rechnungen,
# AltBA/BAE20/WKE sind in "TVx" zusammengelegt. Je Zelle werden die Kategorien
# gezählt. Die Kachel zeigt die HÄUFIGSTE Kategorie (Modus):
#   Farbe = häufigste Kategorie (custom_palette; pBv/Keine Datengrundlage = grau)
#   Zahl  = ABSOLUTE Anzahl (je Klimalauf meist 1; > 1 erst über mehrere Klimaläufe)
#   Gleichstand: die BESSERE Kategorie wird gezeigt und mit "*" + Rahmen markiert.
# Sortierung je Klimalauf GEWICHTET (dunkelgrün zählt am meisten, Summe der Stufe):
# beste Zeile oben, beste Baumart rechts.

kat_lv   <- c(ordn, "pBv", "Keine Datengrundlage")      # Legenden-/Fill-Reihenfolge
kat_pref <- c(rev(ordn), "pBv", "Keine Datengrundlage") # best -> schlecht (Gleichstand: bessere gewinnt)
dunkel   <- c("sehr empfohlen", "nicht empfohlen", "pBv")      # Kacheln mit weißer Schrift

# ansehen: welche Klimaläufe gibt es?  ->  sort(unique(as.character(d$Klimalauf)))
# Zum Durchklicken EINEN Klimalauf setzen und die Zeilen im Rumpf einzeln laufen
# lassen, z. B.:  kl <- "RCP85_MPICLM_2071-2100"
for (kl in sort(unique(as.character(d$Klimalauf)))) {

  # 9a. nur dieser Klimalauf; je Zelle (Zeile TV×Methode × Baumart) AUSZÄHLEN
  zaehl <- d %>%
    filter(Klimalauf == kl) %>%
    count(TV_M, Baumart, Kategorie, name = "n")
  # ansehen:  zaehl   (Anzahl je TV_M × Baumart × Kategorie)

  # 9b. Kachel = häufigste Kategorie (Modus). Gleichstand -> bessere (kleinster
  #     kat_pref) + Markierung tie.
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

  # 9c. Sortierung GEWICHTET (dunkelgrün zählt am meisten): Summe der Stufe je
  #     Zeile (TV×Methode)/Baumart (sehr empfohlen = max … nicht empfohlen = 1;
  #     pBv/leer = 0). Aufsteigend -> beste (höchste Summe) als letzter
  #     Faktor-Level, damit bei y = oben und bei x = rechts.
  gew     <- d %>% filter(Klimalauf == kl) %>% mutate(w = coalesce(as.numeric(Stufe), 0))
  tv_rang <- gew %>% group_by(TV_M)    %>% summarise(s = sum(w)) %>% arrange(s)
  ba_rang <- gew %>% group_by(Baumart) %>% summarise(s = sum(w)) %>% arrange(s)
  # ansehen:  tv_rang ; ba_rang    (oben/rechts = höchste Summe = beste)

  kachel <- kachel %>%
    mutate(TV_M    = factor(as.character(TV_M),    levels = as.character(tv_rang$TV_M)),
           Baumart = factor(as.character(Baumart), levels = as.character(ba_rang$Baumart)))

  # 9d. EIN durchgehender ggplot-Aufruf
  p_matrix <-
    ggplot(kachel, aes(x = Baumart, y = TV_M)) +
    geom_tile(aes(fill = Kategorie), color = "white", linewidth = 0.6) +
    geom_tile(data = filter(kachel, tie), fill = NA, color = "grey15", linewidth = 1.1) +
    geom_text(aes(label = label, colour = txt_col), size = 3) +
    scale_fill_manual(values = custom_palette, limits = kat_lv, drop = FALSE) +
    scale_colour_identity() +
    coord_equal() +
    labs(title = paste0("BAE – häufigste Empfehlung (Auszählung) – ", master_id),
         subtitle = paste0(stufe, "-stufig  |  ", kl,
                           "  |  gewichtet sortiert: beste Zeile oben, beste Baumart rechts  |  ",
                           "Zahl = Anzahl; * / Rahmen = Gleichstand"),
         x = "Baumart  (beste Empfehlungen →)",
         y = "TV × Methode  (beste oben ↑)", fill = "häufigste Kategorie") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
          axis.text.y = element_text(face = "bold", size = 9),
          panel.grid  = element_blank(),
          plot.background = element_rect(fill = "white", color = NA))

  print(p_matrix)                          # im Plot-Fenster ansehen
  # ggsave(paste0("ModusMatrix_", gsub("[^A-Za-z0-9]+","-",kl), "_beispiel.png"),
  #        p_matrix, width = 22, height = 16, units = "cm", dpi = 150)
}
