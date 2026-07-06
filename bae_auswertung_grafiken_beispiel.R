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
#   Skizze 2  – Score-Matrix (Vergangenheit / Zukunft getrennt, sortiert)
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
  Klimalauf = c("OBS_DWD_1961-1990","OBS_DWD_1991-2020",
                "RCP45_MPICLM_2021-2050","RCP45_MPICLM_2071-2100",
                "RCP45-v3_MPICLM_2021-2050","RCP45-v3_MPICLM_2071-2100",
                "RCP85_MPICLM_2021-2050","RCP85_MPICLM_2071-2100"),
  stringsAsFactors = FALSE
)
data$MASTER_ID <- master_id
data$BAE_5ST <- as.character(sample(1:5, nrow(data), replace = TRUE))
data$BAE_5ST[data$TV %in% c(5, 8)] <- "Keine Datengrundlage"   # wie im Bild
data$BAE_5ST[sample(nrow(data), 8)] <- "pBv"
.num <- suppressWarnings(as.integer(data$BAE_5ST))
data$BAE_4ST <- ifelse(is.na(.num), data$BAE_5ST, as.character(pmin(.num, 4)))
data$BAE_3ST <- ifelse(is.na(.num), data$BAE_5ST, as.character(pmin(ceiling(.num/2), 3)))

# ansehen:
#   head(data)
#   str(data)
#   table(data$Klimalauf)
#   table(data$BAE_4ST)


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
# 7.  KATEGORIE + ZAHLENWERT (Stufe) FÜR DIE GEWÄHLTE STUFIGKEIT
# ============================================================================
# code       -> Kategorie (Text)
# Kategorie  -> Stufe (Zahl): match() gibt die Position in `ordn`.
#               nicht empfohlen = 1 ... sehr empfohlen = n.
#               pBv / Keine Datengrundlage -> Stufe = NA (zählen NICHT mit).
d <- d %>%
  mutate(
    code      = as.character(.data[[bae_col]]),
    Kategorie = case_when(
      code %in% names(mapping) ~ unname(mapping[code]),
      code == "pBv"            ~ "pBv",
      TRUE                     ~ "Keine Datengrundlage"),
    Stufe     = match(Kategorie, ordn)
  )

# ansehen:
#   d %>% count(code, Kategorie, Stufe)     # zeigt die Zuordnung Code->Text->Zahl
#   table(d$Kategorie, useNA = "ifany")


# ============================================================================
# 8.  SKIZZE 1  – EMPFEHLUNGS-KURVEN je TV
# ============================================================================
# Idee: pro Panel (Szenario × Zeitraum) für jede TV eine Linie über die
# Baumarten. y = Stufe (unten "nicht empfohlen", oben "sehr empfohlen").
# Wo sich die Kurven decken, sind sich die TVs einig.

# Farben für die TV-Linien
tv_levels <- levels(droplevels(d$TV))
tv_farben <- setNames(
  c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02",
    "#A6761D","#666666","#1F78B4","#B2182B","#33A02C","#6A3D9A")[seq_along(tv_levels)],
  tv_levels)

# EIN durchgehender ggplot-Aufruf – jede Ebene eine Zeile:
p_kurven <-
  ggplot(d, aes(x = Baumart, y = Stufe, group = TV, colour = TV)) +
  geom_line(linewidth = 0.8, alpha = 0.8, na.rm = TRUE) +
  geom_point(size = 1.6, alpha = 0.9, na.rm = TRUE) +
  facet_grid(Szen_label ~ Zeitraum) +
  scale_y_continuous(breaks = seq_along(ordn), labels = ordn, limits = c(1, length(ordn))) +
  scale_colour_manual(values = tv_farben) +
  labs(title = paste0("BAE-Empfehlungskurven – ", master_id),
       subtitle = paste0(stufe, "-stufig  |  Überlappung der Linien = Einigkeit der TVs"),
       x = NULL, y = "Empfehlung", colour = "TV") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text  = element_text(face = "bold", size = 9),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA))

p_kurven                                   # im Plot-Fenster ansehen
# ggsave("Kurven_beispiel.png", p_kurven, width = 34, height = 22, units = "cm", dpi = 150)


# ============================================================================
# 9.  SKIZZE 2  – SCORE-MATRIX (Vergangenheit / Zukunft getrennt)
# ============================================================================
# Score = gewichtete Summe: je Zelle (TV × Baumart) die Stufen aller Klimaläufe
# aufsummieren. pBv / Keine Datengrundlage (Stufe = NA) zählen nicht mit.
# Sortierung: TV nach Gesamt-Score (meiste/beste oben), Baumart ebenso (rechts).
# Farbe: rot (niedrig) -> orange -> grün (hoch)  ->  Bestes landet oben rechts.

# Gruppe Vergangenheit (OBS) vs. Zukunft (RCP)
d <- d %>% mutate(Gruppe = ifelse(ist_rcp, "Zukunft", "Vergangenheit"))

score_gradient <- c("#A50026", "#FDAE61", "#FEE08B", "#A6D96A", "#1A9850")

# Für beide Gruppen dasselbe machen. Zum Durchklicken kannst du auch einfach
#   grp <- "Vergangenheit"
# setzen und die Zeilen im Schleifenrumpf einzeln ausführen.
for (grp in c("Vergangenheit", "Zukunft")) {

  # 9a. zählen + gewichtet summieren
  agg <- d %>%
    filter(Gruppe == grp, !is.na(Stufe)) %>%
    group_by(TV, Baumart) %>%
    summarise(Score = sum(Stufe), N = n(), .groups = "drop") %>%
    droplevels()
  # ansehen:  agg   (Score je TV × Baumart)

  # 9b. Sortier-Reihenfolge bestimmen (aufsteigend -> höchster Wert zuletzt,
  #     bei y = oben, bei x = rechts)
  tv_rang <- agg %>% group_by(TV)      %>% summarise(s = sum(Score)) %>% arrange(s)
  ba_rang <- agg %>% group_by(Baumart) %>% summarise(s = sum(Score)) %>% arrange(s)
  # ansehen:  tv_rang ; ba_rang

  # 9c. Faktoren in dieser Reihenfolge setzen
  agg <- agg %>%
    mutate(TV      = factor(as.character(TV),      levels = as.character(tv_rang$TV)),
           Baumart = factor(as.character(Baumart), levels = as.character(ba_rang$Baumart)))

  # 9d. EIN durchgehender ggplot-Aufruf
  p_matrix <-
    ggplot(agg, aes(x = Baumart, y = TV, fill = Score)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = Score), size = 3, colour = "grey15") +
    scale_fill_gradientn(colours = score_gradient) +
    coord_equal() +
    labs(title = paste0("BAE – gezählte Einträge (Score) – ", master_id),
         subtitle = paste0(stufe, "-stufig  |  ", grp,
                           "  |  TV nach Einträgen (oben), Baumart nach Empfehlung (rechts)"),
         x = "Baumart  (höchste Empfehlungen →)",
         y = "TV  (meiste Einträge ↑)", fill = "Score") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
          axis.text.y = element_text(face = "bold", size = 9),
          panel.grid  = element_blank(),
          plot.background = element_rect(fill = "white", color = NA))

  print(p_matrix)                          # im Plot-Fenster ansehen
  # ggsave(paste0("ScoreMatrix_", grp, "_beispiel.png"),
  #        p_matrix, width = 22, height = 16, units = "cm", dpi = 150)
}
