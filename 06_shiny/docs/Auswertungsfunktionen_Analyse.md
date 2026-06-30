# Analyse der Auswertungsfunktionen (BAE-Auswertung)

Stand: 2026-06-22 · Geprüfte Dateien: `R/mod_standort.R`, `R/mod_karte.R`, `app.R` (Analyse-Tab)

Dieses Dokument fasst eine Code-Review der zentralen Auswertungsfunktionen
zusammen: Wo entstehen Zahlen, die zwar berechnet werden, aber fachlich
fragwürdig oder bei Änderungen leise inkonsistent werden können? Jeder Punkt
enthält die betroffene Stelle, ein Code-Beispiel, die Kritik und einen
konkreten Vorschlag.

Kritikalität: 🔴 hoch · 🟠 mittel · 🟡 niedrig

---

## 🔴 1. Die BAE-Stufenzuordnung existiert fünffach

**Betroffene Stellen:**

- `R/mod_karte.R:57` – `map_stufe()` (das eigentlich kanonische Mapping)
- `R/mod_standort.R:240` – `heatmap_standort_ggplot()`, lokale `stufe_maps`
- `R/mod_standort.R:305` – `auswertung_standort_ggplot()`, `stufe_levels`
- `R/mod_standort.R:313` – `auswertung_standort_ggplot()`, lokale `m`
- `R/mod_standort.R:361` – `auswertung_grid_ggplot()`, lokale `m`

**Code-Beispiel (eine von vier Kopien):**

```r
# R/mod_standort.R:240 ff. – heatmap_standort_ggplot()
stufe_maps <- list(
  BAE_3ST = c("1"="sehr empfohlen","2"="mäßig empfohlen","3"="nicht empfohlen"),
  BAE_4ST = c("1"="sehr empfohlen","2"="empfohlen","3"="mäßig empfohlen","4"="nicht empfohlen"),
  BAE_5ST = c("1"="sehr empfohlen","2"="empfohlen","3"="mäßig empfohlen",
              "4"="wenig empfohlen","5"="nicht empfohlen"),
  BAE_7ST = c("1"="sehr empfohlen","2"="sehr empfohlen","3"="empfohlen",
              "4"="mäßig empfohlen","5"="wenig empfohlen",
              "6"="nicht empfohlen","7"="nicht empfohlen")
)
```

```r
# R/mod_karte.R:57 ff. – map_stufe(), nahezu identisch, aber eigenstaendig
map_stufe <- function(val, st) {
  maps <- list(
    "BAE_3ST" = c("1"="sehr empfohlen", "2"="mäßig empfohlen", "3"="nicht empfohlen"),
    "BAE_4ST" = c("1"="sehr empfohlen", "2"="empfohlen", "3"="mäßig empfohlen", "4"="nicht empfohlen"),
    ...
  )
  ...
}
```

**Kritik**

- Fünf Kopien derselben fachlichen Logik. Wird die Skala an einer Stelle
  korrigiert (z. B. weil sich herausstellt, dass `BAE_7ST` Code „2" nicht
  „sehr empfohlen", sondern eine eigene Zwischenstufe sein soll), driften
  Karte, Heatmap, Balkengrafik und Grid-Grafik fachlich auseinander –
  **ohne Fehlermeldung, ohne Testabdeckung, ohne Hinweis im UI.**
- Die gleiche MASTER_ID kann dann je nach Tab eine andere Empfehlung zeigen.
  Das ist für ein Auswertungswerkzeug der größte Vertrauensschaden, den man
  sich vorstellen kann – falsch aussehende Heatmaps fallen sofort auf,
  *unterschiedliche aber jeweils plausible* Werte nicht.
- Die Duplizierung erschwert zusätzlich jede Erweiterung (z. B. eine neue
  Bewertungsstufe `BAE_6ST`): vier Stellen müssen synchron geändert werden.

**Vorschlag**

`map_stufe()` als einzige Quelle behalten, die anderen Funktionen darauf
umstellen:

```r
# R/mod_karte.R – zentrale, einzige Quelle (unverändert lassen)
map_stufe <- function(val, st) { ... }

# R/mod_standort.R – heatmap_standort_ggplot(): lokale stufe_maps entfernen
d$Kat <- map_stufe(d[[kat_col]], kat_col)

# R/mod_standort.R – auswertung_standort_ggplot(): lokales `m` entfernen
d <- df %>%
  dplyr::mutate(
    Kat = map_stufe(.data[[kat_col]], kat_col),
    Kat = factor(Kat, levels = rev(lvls)),
    x   = .data[[x_var]]
  )

# R/mod_standort.R – auswertung_grid_ggplot(): lokales `m` entfernen
df %>%
  dplyr::mutate(Kat = map_stufe(.data[[kat_col]], kat_col),
                Kat = factor(Kat, levels = rev(names(kat_palette))))
```

Die Stufen-Label-Reihenfolgen (`stufe_levels` für die Achsen) können als
eigene, ebenfalls zentrale Konstante neben `kat_palette` definiert werden,
damit auch sie nicht mehrfach kopiert werden.

---

## 🔴 2. Mittelwert/Median/SD über ordinale BAE-Codes

**Betroffene Stelle:** `app.R:1606-1633` – `analyse_data()`, Abschnitt
„Deskriptive Statistik"

**Code-Beispiel**

```r
# app.R:1614-1629
stats_df <- if (!is.null(bae_col)) {
  tryCatch(
    df_plain %>%
      mutate(BAE_num = suppressWarnings(as.integer(.data[[bae_col]]))) %>%
      filter(!is.na(BAE_num)) %>%
      group_by(Baumart, TV) %>%
      summarise(
        N      = n(),
        Min    = min(BAE_num),
        Median = median(BAE_num),
        Mean   = round(mean(BAE_num), 2),
        Max    = max(BAE_num),
        SD     = round(sd(BAE_num), 2),
        .groups = "drop"
      ),
    error = function(e) data.frame(Info = conditionMessage(e))
  )
} else { ... }
```

**Kritik**

- BAE-Codes sind **ordinal**, kein metrisches Intervall: Der „Abstand"
  zwischen Stufe 1 und 2 ist nicht notwendigerweise gleich groß wie zwischen
  2 und 3. Ein arithmetisches Mittel von z. B. 2,4 hat damit keine
  unmittelbare fachliche Bedeutung – es wird im UI aber wie eine belastbare
  Kennzahl präsentiert (inkl. Farbbalken, siehe unten).
- Bei `BAE_7ST` ist die Codierung sogar **nicht-monoton im Sinngehalt**:
  Code 1 *und* 2 bedeuten beide „sehr empfohlen", Code 6 *und* 7 beide
  „nicht empfohlen" (vgl. `map_stufe()`). Ein Mittelwert über die Rohcodes
  gewichtet diese Pseudo-Differenzierung mit, was die Kennzahl zusätzlich
  verzerrt.
- `as.integer("pBv")` ergibt `NA` und wird per `filter(!is.na(BAE_num))`
  **lautlos aus N, Mean, SD entfernt**. „pBv" (potenziell besser vorhandene
  Art?) und „Keine Datengrundlage" verschwinden aus der Statistik, ohne dass
  das im UI sichtbar wird – `N` in der Tabelle ist dann kleiner als die
  Anzahl Standorte in der Kreuztabelle für dieselbe Baumart, und niemand
  sieht warum.
- Der Farbbalken nutzt `range(ad$stats_df$Mean, na.rm = TRUE)` (app.R:1782)
  über *alle* Baumarten – das suggeriert eine Vergleichbarkeit der
  Mittelwerte zwischen Baumarten, die durch die obigen Punkte ohnehin schon
  fraglich ist.

**Vorschlag**

Zwei Optionen, je nach gewünschter fachlicher Tiefe:

a) **Minimal-Fix (sofort umsetzbar):** `N` explizit für die Kreuztabelle und
   für die Statistik getrennt ausweisen, und die ausgeschlossenen Werte
   sichtbar machen:

   ```r
   stats_df <- df_plain %>%
     mutate(BAE_num = suppressWarnings(as.integer(.data[[bae_col]]))) %>%
     group_by(Baumart, TV) %>%
     summarise(
       N_gesamt    = n(),
       N_bewertet  = sum(!is.na(BAE_num)),
       N_pBv       = sum(.data[[bae_col]] == "pBv", na.rm = TRUE),
       Min         = suppressWarnings(min(BAE_num, na.rm = TRUE)),
       Median      = median(BAE_num, na.rm = TRUE),
       Mean        = round(mean(BAE_num, na.rm = TRUE), 2),
       Max         = suppressWarnings(max(BAE_num, na.rm = TRUE)),
       SD          = round(sd(BAE_num, na.rm = TRUE), 2),
       .groups = "drop"
     )
   ```

b) **Fachlich saubererer Fix:** Statt Mittelwert/SD über Rohcodes lieber
   ordinal-gerechte Kennzahlen ausweisen, z. B. **Median + Modus** (robust
   gegen ordinale Skalen) sowie den Anteil „empfohlen oder besser" als
   Prozentwert (analog zur Kreuztabelle). Mittelwert/SD könnten optional
   bleiben, aber mit einem Hinweistext im UI („Hinweis: BAE ist eine
   Ordinalskala, der Mittelwert ist eine Approximation").

Diese Entscheidung sollte fachlich mit dem Team abgestimmt werden – das ist
keine reine Code-Korrektur, sondern betrifft die Interpretation der
Ausgabe.

---

## 🔴 3. Statistik-Tab kann eine andere Stufe zeigen als Kreuztabelle/Heatmap

**Betroffene Stelle:** `app.R:1606-1613`

**Code-Beispiel**

```r
# app.R:1606-1613
# Deskriptive Statistik: BAE-Wert numerisch je Baumart/TV
# Fallback: wenn gewählte Stufe nicht in df_plain, höchste verfügbare nehmen
bae_col <- if (!is.null(input$stufe) && input$stufe %in% names(df_plain)) {
  input$stufe
} else {
  found <- intersect(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST"), names(df_plain))
  if (length(found) > 0) found[length(found)] else NULL
}
```

Demgegenüber verwenden Kreuztabelle und Balkendiagramm direkt `Kat`, das
bereits vorher aus `input$stufe` abgeleitet wurde (`app.R:1577-1580`):

```r
kat_order <- names(kat_palette)
df_plain <- sf::st_drop_geometry(df) %>%
  mutate(Kat = factor(Kat, levels = kat_order))
```

**Kritik**

- Fällt `input$stufe` aus den Spalten von `df_plain` heraus (z. B. weil eine
  CSV-Quelle nur 3-stufige Daten liefert, der Nutzer aber 5-stufig
  ausgewählt hat), springt **nur** die deskriptive Statistik auf
  `found[length(found)]` – die *zuletzt* in der Liste
  `c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST")` gefundene, nicht
  notwendigerweise die "nächstbeste" Stufe.
- Kreuztabelle und Balkendiagramm bleiben bei `input$stufe` (das `Kat`
  vorher schon berechnet hat) – ein Nutzer sieht im selben Bildschirm zwei
  unterschiedliche Stufensysteme, ohne expliziten Hinweis welches wo aktiv
  ist.
- Der Kommentar „höchste verfügbare nehmen" ist zudem missverständlich:
  `found[length(found)]` ist nicht „die höchste verfügbare im Sinne
  feinster Granularität", sondern schlicht der letzte Treffer der
  Such-Reihenfolge im Vektor.

**Vorschlag**

```r
bae_col <- if (!is.null(input$stufe) && input$stufe %in% names(df_plain)) {
  input$stufe
} else {
  NULL  # keine Stufe verfuegbar -> klar kommunizieren statt umschalten
}

stats_df <- if (!is.null(bae_col)) {
  ...
} else {
  data.frame(Info = paste0("Stufe '", input$stufe,
                            "' in den aktuell gefilterten Daten nicht verfügbar."))
}
```

Falls ein automatisches Fallback gewünscht ist, sollte er explizit im UI
angezeigt werden (z. B. als Banner „Statistik zeigt BAE_3ST, da BAE_5ST für
diese Auswahl nicht vorhanden ist"), damit Kreuztabelle und Statistik nicht
unbemerkt divergieren.

---

## 🟠 4. Stilles Verschlucken von Zeilen/Dateien bei der NR-Standortanalyse

**Betroffene Stelle:** `R/mod_standort.R:121-223` – `lade_standort_alle_laeufe()`

**Code-Beispiel**

```r
# R/mod_standort.R:144-164
df <- dplyr::bind_rows(lapply(seq_len(nrow(inv)), function(i) {
  d <- tryCatch(data.table::fread(inv$file[i], data.table = FALSE),
                error = function(e) NULL)
  if (is.null(d)) return(NULL)
  names(d) <- toupper(names(d))
  if (!"MASTER_ID" %in% names(d)) return(NULL)
  row <- d[as.character(d$MASTER_ID) == master_id_chr, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row <- row[1, , drop = FALSE]          # <-- nimmt nur die erste Zeile
  ...
}))
```

```r
# R/mod_standort.R:165-180 – Merge bei mehreren Stufen-Ordnern
df <- df %>%
  dplyr::group_by(MASTER_ID, Baumart, TV, Zeitlauf,
                  Szenario, Modell, Zeitraum) %>%
  dplyr::summarise(
    dplyr::across(dplyr::any_of(c("BAE_3ST","BAE_4ST","BAE_5ST","BAE_7ST")),
                  ~ { v <- .x[!is.na(.x)]; if (length(v)) v[1] else NA_character_ }),
    .groups = "drop") %>%
  as.data.frame()
```

**Kritik**

- `row[1, ]`: Enthält eine CSV mehrere Zeilen für dieselbe `MASTER_ID`
  (z. B. Datenfehler, Duplikate, oder eine andere Granularität als
  angenommen), wird der Rest **ohne Warnung** verworfen.
- Beim Merge über mehrere Stufen-Ordner (3ST/4ST/5ST-Verzeichnisse für
  denselben Klimalauf) wird bei widersprüchlichen Werten einfach der erste
  Nicht-NA-Wert (`v[1]`) genommen. Liefern zwei Quelldateien
  unterschiedliche `BAE_4ST`-Werte für denselben Lauf, **gewinnt die Datei,
  die zuerst in der Dateiliste auftaucht** – ein impliziter, nicht
  dokumentierter Konflikt-Resolver.
- `nr_csv_inventar()` (Z. 81-114) verwirft jede Datei, deren Name nicht ins
  erwartete Positionsschema passt (`return(NULL)` bei `is.na(pos_nr)`,
  fehlendem Zeitraum-Token etc.) – ebenfalls ohne Zähler/Meldung an den
  Nutzer, nur über `message()` in der Server-Konsole sichtbar.

**Vorschlag**

a) Bei mehreren Treffern in derselben CSV eine Warnung loggen und (sofern
   möglich) dem Nutzer sichtbar machen, statt sie zu verschlucken:

   ```r
   row <- d[as.character(d$MASTER_ID) == master_id_chr, , drop = FALSE]
   if (nrow(row) == 0) return(NULL)
   if (nrow(row) > 1) {
     warning("Mehrfachtreffer fuer MASTER_ID ", master_id_chr,
             " in ", inv$file[i], " (", nrow(row), " Zeilen) – nehme die erste.")
   }
   row <- row[1, , drop = FALSE]
   ```

b) Beim Stufen-Merge Konflikte explizit prüfen statt sie zu maskieren:

   ```r
   ~ {
       v <- unique(.x[!is.na(.x)])
       if (length(v) > 1) {
         warning("Widersprechende Werte fuer ", dplyr::cur_column(),
                 " bei MASTER_ID ", master_id_chr, ": ", paste(v, collapse=", "))
       }
       if (length(v)) v[1] else NA_character_
     }
   ```

c) `nr_csv_inventar()`: Anzahl ignorierter Dateien zurückgeben/loggen, z. B.
   als Attribut am Ergebnis (`attr(out, "ignoriert") <- n_ignoriert`), damit
   die aufrufende Funktion das im UI als Hinweis anzeigen kann.

---

## 🟠 5. `TRUE ~ "Keine Datengrundlage"` als Catch-all in allen `case_when`

**Betroffene Stellen:** u. a. `R/mod_karte.R:74-78`,
`R/mod_standort.R:259-263`, `R/mod_standort.R:321-325`,
`R/mod_standort.R:372-374`

**Code-Beispiel**

```r
# R/mod_karte.R:74-78 – map_stufe()
dplyr::case_when(
  v %in% names(m) ~ unname(m[v]),
  v == "pBv"      ~ "pBv",
  TRUE            ~ "Keine Datengrundlage"
)
```

**Kritik**

Jeder Wert, der weder ein gültiger Stufencode noch `"pBv"` ist – also auch
ein **Tippfehler in den Quelldaten, ein neuer/unbekannter Code, oder ein
Parsing-Fehler weiter oben in der Pipeline** – landet kommentarlos in der
Kategorie „Keine Datengrundlage". Diese Kategorie ist aber visuell und
fachlich für *fehlende* Daten gedacht, nicht für *fehlerhafte*. Ein echter
Datenfehler (z. B. ein Encoding-Problem, das aus `"3"` ein `"3 "` mit
Leerzeichen macht) wird so unsichtbar zu einer harmlos aussehenden grauen
Kachel – im schlimmsten Fall über viele Zeilen hinweg, ohne dass es jemand
merkt.

**Vorschlag**

Unerwartete, nicht-leere Werte von echten NA/fehlenden Werten unterscheiden:

```r
dplyr::case_when(
  is.na(v)        ~ "Keine Datengrundlage",
  v %in% names(m) ~ unname(m[v]),
  v == "pBv"      ~ "pBv",
  TRUE            ~ {
    warning("Unbekannter BAE-Code '", v, "' fuer Stufe ", st,
            " – wird als 'Keine Datengrundlage' behandelt.")
    "Keine Datengrundlage"
  }
)
```

(`case_when` mit Seiteneffekten ist nicht ideal – pragmatischer wäre, vor
dem `case_when` einmal `setdiff(unique(v), c(names(m), "pBv", NA))` zu
prüfen und ggf. zu loggen.)

---

## 🟡 6. Prozent-Nenner enthält „Keine Datengrundlage"

**Betroffene Stellen:** `R/mod_standort.R:329-334` (`auswertung_standort_ggplot`),
`app.R:1599-1604` (`analyse_data`, `balken_df`)

```r
# R/mod_standort.R:329-334
d %>%
  dplyr::group_by(Baumart, x, Kat, .drop = FALSE) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(Baumart, x) %>%
  dplyr::mutate(Pct = n / sum(n) * 100) %>%
  ...
```

**Kritik:** Der Nenner `sum(n)` enthält auch Zeilen mit Kategorie „Keine
Datengrundlage" bzw. „pBv". „Anteil sehr empfohlen" wird also durch fehlende
Daten verdünnt – fachlich vertretbar, aber nirgends dokumentiert, ob das so
gewollt ist oder ob der Nenner nur über *bewertete* Standorte laufen sollte.

**Vorschlag:** Kurzer Kommentar im Code, der die Entscheidung festhält, plus
ggf. eine optionale zweite Kennzahl „Anteil bezogen auf bewertete Standorte
(ohne 'Keine Datengrundlage')" als Toggle im UI.

---

## 🟡 7. Positionsbasiertes Datei-Parsing ist fragil

**Betroffene Stellen:** `R/mod_standort.R:81-114` (`nr_csv_inventar`),
`R/mod_karte.R:23-35` (`parse_klimalauf`)

```r
# R/mod_standort.R:91-109
teile  <- strsplit(bn, "_", fixed = TRUE)[[1]]
pos_nr <- which(tolower(teile) == tolower(nr_id))[1]
...
modell <- if (zr_pos - pos_nr >= 2)
  paste(teile[(pos_nr + 2):(zr_pos - 1)], collapse = "_") else "(OBS)"
...
Baumart  = paste(teile[(zr_pos + 1):length(teile)], collapse = "_")
```

**Kritik:** Funktioniert nur, solange das Namensschema exakt eingehalten
wird. Enthält ein Modellname zufällig eine Zeichenkette, die wie eine
Region (`nr_id`) oder ein Zeitraum (`\d{4}-\d{4}`) aussieht, verschiebt sich
die Positionslogik unbemerkt. Es gibt keine Validierung, dass das
zusammengesetzte Ergebnis (z. B. `Baumart`) tatsächlich ein bekannter Wert
aus `baumart_choices` ist.

**Vorschlag:** Nach dem Parsen eine Plausibilitätsprüfung einbauen, z. B.:

```r
unbekannt <- setdiff(unique(out$Baumart), baumart_choices)
if (length(unbekannt) > 0) {
  warning("Unbekannte Baumart-Tokens aus Dateinamen-Parsing: ",
          paste(unbekannt, collapse = ", "))
}
```

---

## 🟡 8. `unique(df$MASTER_ID)[1]` in Titeln

**Betroffene Stellen:** `R/mod_standort.R:276`, `R/mod_standort.R:344`, `R/mod_standort.R:388`

```r
title = paste0("BAE-Heatmap – ", unique(d$MASTER_ID)[1]),
```

**Kritik:** Implizite Annahme, dass `df` nur eine MASTER_ID enthält. Wird
eine dieser Funktionen künftig mit einem Mehrfach-Standort-`df` aufgerufen
(z. B. Vergleichsansicht mehrerer Standorte), zeigt der Titel nur die erste
ID, ohne Hinweis, dass weitere Standorte im Plot enthalten sind.

**Vorschlag:** Defensive Prüfung, die zumindest sichtbar macht, falls die
Annahme verletzt wird:

```r
mids <- unique(d$MASTER_ID)
title_id <- if (length(mids) == 1) mids else paste0(mids[1], " (+", length(mids)-1, " weitere)")
```

---

## Priorisierte Empfehlung

| Prio | Punkt | Aufwand | Risiko bei Nicht-Behebung |
|---|---|---|---|
| 1 | #1 Stufenzuordnung zentralisieren | klein, rein technisch | Karte/Standortblatt zeigen unterschiedliche Empfehlungen |
| 2 | #3 Stufen-Fallback an `input$stufe` koppeln | klein | Statistik und Kreuztabelle zeigen unterschiedliche Stufen |
| 3 | #2 Statistik-Semantik (Mean/SD über Ordinalskala) | mittel, fachliche Abstimmung nötig | Irreführende Kennzahlen im Statistik-Tab |
| 4 | #4 NR-Merge-Konflikte sichtbar machen | mittel | Stille Datenverluste bei Mehrfachquellen |
| 5 | #5–#8 | klein je Punkt | Einzelfälle, geringere Eintrittswahrscheinlichkeit |

Vorschlag für die Umsetzung: **#1 und #3** sind risikofrei und sofort
umsetzbar (reines Refactoring ohne fachliche Änderung der Ausgabe für den
Normalfall). **#2** sollte vorher fachlich abgestimmt werden, da es die
Interpretation der angezeigten Statistik-Werte verändert.
