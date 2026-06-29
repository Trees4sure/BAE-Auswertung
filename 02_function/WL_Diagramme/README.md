# Walther-Lieth-Klimadiagramme

Funktionen zum Erzeugen von Walther-Lieth-Klimadiagrammen je BWI-Punkt /
NR-Zelle und Klimalauf – zum Aufruf beim Klick auf einen Punkt.

## Eingangsdaten

Zwei Monats-Klimatologien aus dem Parameter-Katalog (je 12 Layer, Jan–Dez):

| ID   | Inhalt                                   | CDO-Ableitung            |
|------|------------------------------------------|--------------------------|
| 1155 | Monatsmitteltemperatur, 30a-Mittel [°C]  | `ymonmean(monmean(1112))`|
| 1157 | mittl. Monatsniederschlag, 30a-Mittel [mm]| `ymonmean(monsum(1114))`|

Die korrekte Logik (Temperatur = Mittel, Niederschlag = Summe-dann-Mittel)
steckt bereits in diesen IDs. Fehlen sie für einen Lauf, aus den Tagesdaten
nachgenerieren:

```bash
# Temperatur (degC)
cdo -b F32 ymonmean -monmean -mulc,0.1 -mergetime 1112_*.nc 1155_run.nc
# Niederschlag (mm/Monat)
cdo -b F32 ymonmean -monsum  -mulc,0.1 -mergetime 1114_*.nc 1157_run.nc
```

> Faktor `0.1` gemäß Tabelle 6 (Roh-Tagesdaten = Integer ×10) kurz gegenprüfen.

## Nutzung

```r
source("02_function/WL_Diagramme/walther_lieth_input.R")
source("02_function/WL_Diagramme/plot_walther_lieth.R")

# rast_list: benannte Liste (Name = Zeitlauf) mit je 24-Layer-SpatRaster
#            (12x 1155 Temp + 12x 1157 Niederschlag) ODER list(temp, prec).
# geom:      gemeinsame BWI-Geometrie/Metadaten (cbind aus nc.BWI.*-df,
#            inkl. Spalten id, Lon, Lat, altitude).

wl <- build_walther_lieth_input(rast_list, geom = geom)
# -> Long-Format: id | Zeitlauf | Monat | T_mean | P_sum

# Diagramm beim Klick auf einen Punkt:
plot_walther_lieth_from_long(wl, id_val = 70041234,
                             run = "RCP45_MPIWRF_2071-2100")
```

`plot_walther_lieth()` zeichnet ein klassisches Walther-Lieth-Diagramm
(4-Ecken-Kopf, humide Senkrechtschraffur, aride Punktschraffur, perhumide
Füllung > 100 mm, Frostbalken). Der Kopf wird mit **patchwork** gesetzt;
fehlt es, gibt es einen Titel-Fallback. Sonderzeichen liegen als `\u`-Escapes
im Quelltext → kein Mojibake, egal welches Encoding beim `source()`.

### Alternative: direkt aus den NetCDF-Tabellen (ncdf4, ohne terra)

Wenn die Monats-NetCDFs bereits per `ncdf4` in **breite Tabellen** überführt
wurden (`cell_id | x | y | Jan…Dez | name`), führt `wl_long_from_tables()` Temp
(1155) und Niederschlag (1157) direkt ins WL-Long-Format zusammen – ganz ohne
`terra`/`rast_list`.

```r
source("02_function/WL_Diagramme/walther_lieth_input.R")  # .wl_detect_scale()
source("02_function/WL_Diagramme/nc_monthly_tables.R")
source("02_function/WL_Diagramme/plot_walther_lieth.R")

# breite Tabellen je Datei einlesen (nc.1157_function existiert bereits)
temp_df <- nc.1155_function(nc.1155_list.files[5])   # MAT, Varname "tadm"
prec_df <- nc.1157_function(nc.1157_list.files[5])   # MAP, Varname "rrds"

# Bruecke -> Long-Format (id | Zeitlauf | Monat | T_mean | P_sum [+ x,y])
wl <- wl_long_from_tables(temp_df, prec_df)

plot_walther_lieth_from_long(wl, id_val = 1, run = wl$Zeitlauf[1])
```

Details:
- **Monate werden über die Spalten-Position (1..12) gemappt**, nicht über die
  (lokalisierten) Monatsnamen `Jan…Dez` – robust gegen die System-Locale.
- Der **Zeitlauf** wird aus `name` abgeleitet, indem die Parameter-ID am Anfang
  (`1155_`/`1157_`) und `.nc` entfernt werden → 1155 und 1157 desselben Laufs
  bekommen denselben Schlüssel und lassen sich joinen.
- **Skalierung** wie bei `wl_extract_run()`: ein automatisch (aus der Temperatur)
  erkannter Faktor wird auf Temperatur **und** Niederschlag gemeinsam angewandt
  (`scale = 1` bzw. `0.1` überschreibt). `cell_id` = `id_bwi_bze` (Join-Key zur
  Empfehlungs-Kette).

## 30-Jahres-Verlauf (Jahresparameter 1049 / 1050)

`wali_trend.R` – „eine Art Walther-Lieth", aber über die Jahre
statt über die Monate, aus den **Jahres**-Parametern:

| ID   | Inhalt                              |
|------|-------------------------------------|
| 1049 | Jahresmitteltemperatur [°C]         |
| 1050 | Jahresniederschlagssumme [mm]       |

Hier sind die zwei Achsen **unabhängig** skaliert (die strikte 1 °C : 2 mm-
Kopplung ergibt nur bei Monatswerten Sinn). Gezeigt: Temperatur (rot, links)
und Niederschlag (blau, rechts) über die ~30 Jahre, mit linearen Trendlinien.

`rast_list` ist genau deine bestehende Liste `nc.grep.variables_BWI_KS`
(je Lauf ein SpatRaster mit 60 Layern: 30× `tadm`/1049 + 30× `rrds`/1050).
**Wichtig: KEIN `tapp`** – das mittelt die 30 Jahre weg; der Builder braucht die
Jahres-Layer.

```r
source("02_function/WL_Diagramme/walther_lieth_input.R")  # .wl_detect_scale()
source("02_function/WL_Diagramme/wali_trend.R")

# 1) Geometrie je Rasterzelle (wie in deinem cbind, nur ohne Variablen-Spalten)
geom_bwi <- cbind(nc.BWI.rw.df, nc.BWI.hw.df, nc.BWI.el.df, nc.BWI.id.df)
geom_bwi <- dplyr::rename(geom_bwi, Lon = x_25832, Lat = y_25832,
                          altitude = elevation_250m, id = id)

# 2) WaLi-Trend Long-Format DIREKT aus den 60-Layer-Rastern (kein tapp!)
ts <- build_wali_trend_input(nc.grep.variables_BWI_KS, geom = geom_bwi)
# -> id | Lon | Lat | altitude | Jahr | Kalenderjahr | T_year | P_year | Zeitlauf

plot_wali_trend_from_long(ts, id_val = 70041234,
                          run = "RCP45_MPIWRF_2071-2100")
```

Layer-Erkennung läuft über `1049/MAT/tadm` bzw. `1050/MAP/rrds`; Läufe mit
21/29 Jahren (statt 30) werden automatisch korrekt getrennt.

## Klimaläufe vergleichen (Differenz sichtbar machen)

Um zu zeigen, **wie** sich das Klima zwischen Läufen verschiebt – und damit,
**weshalb** eine Baumartenempfehlung die Stufe wechselt – bleiben die Diagramme
im WL-Stil. Drei Bausteine:

- `walther_lieth_compare.R` – Monats-WL: **Small Multiples** (`*_facets`) +
  **Delta-WL** (`*_delta`, Vergleich − Referenz; bei Monatsdeltas wieder echte
  1 °C : 2 mm-Kopplung). Wrapper `compare_walther_lieth(mode = "both")`.
- `wali_trend_compare.R` – WaLi-Trend: **Small Multiples** + **Mittel-Shift**
  (Ø ΔT / Ø ΔP je Lauf vs. Referenz, da verschiedene Perioden kein jahrweises
  Delta erlauben). Wrapper `compare_wali_trend(mode = "both")`.
- `recommendation_strip.R` – farbcodierte **Empfehlungs-Leiste** (Art × Lauf);
  ein Stufenwechsel ist als Farbwechsel entlang der Art-Zeile sichtbar.
  `combine_climate_recommendation()` stapelt Klimasignal über die Leiste.

```r
source("02_function/WL_Diagramme/walther_lieth_helpers.R")
source("02_function/WL_Diagramme/walther_lieth_compare.R")
source("02_function/WL_Diagramme/wali_trend_compare.R")
source("02_function/WL_Diagramme/recommendation_strip.R")

wl  <- read.csv("02_function/WL_Diagramme/testdata/shift_monthly_test.csv")
rec <- read.csv("02_function/WL_Diagramme/testdata/recommendation_test.csv")
runs <- c("Referenz_1991-2020", "RCP45_2071-2100", "RCP85_2071-2100")

compare_walther_lieth(wl, 70041234, runs = runs, mode = "both")

# Empfehlungs-Leiste deckungsgleich UNTER die Klima-Small-Multiples (aligned):
combine_climate_recommendation(
  plot_walther_lieth_facets(wl, 70041234, runs = runs),
  rec, 70041234, runs = runs, aligned = TRUE)
```

`aligned = TRUE` rendert die Empfehlung als **facettierte** Leiste (ein Kachel-
Panel je Lauf), sodass jede Lauf-Spalte exakt unter ihrem Klimadiagramm sitzt;
`aligned = FALSE` legt eine kompakte Matrix (Läufe als Spalten) darunter.

### Standort-Gesamtübersicht (eine Grafik)

`compose_overview.R` → `compose_scenario_overview()` baut die zusammengesetzte
Übersicht: **links** alle WL-Diagramme klein untereinander (Referenz oben, dann
Szenarien), **rechts oben** die Empfehlungswechsel, **rechts darunter** je
Szenario das Δ-Diagramm. Die Zeilen sind ausgerichtet (Referenz-WL ↔ Empfehlung,
Szenario-WL ↔ sein Δ-Plot).

```r
source("02_function/WL_Diagramme/plot_walther_lieth.R")   # wl_panel
source("02_function/WL_Diagramme/compose_overview.R")
compose_scenario_overview(wl, rec, 70041234,
  ref = "Referenz_1991-2020",
  scenarios = c("RCP45_2071-2100", "RCP85_2071-2100"))
```

Komplettes Beispiel: `source("02_function/WL_Diagramme/testdata/demo_compare.R")`.

## Klima-Wolken-Diagramm (MAT vs. MAP über alle BWI-BZE-Punkte)

`climate_space.R` → `plot_climate_space()` zeichnet die deutschlandweite
**Klima-Punktwolke** (Streudiagramm Jahresmitteltemperatur **MAT** auf der
x-Achse gegen Jahresniederschlag **MAP** auf der y-Achse) für einen Klimalauf
(Default `OBS_DWD_1991-2020`). Drei Ebenen, von hinten nach vorne:

1. **alle** BWI-BZE-Punkte → dunkelgrau (`grey30`),
2. ein **Bundesland** (Spalte `BL`, Default `"MV"`) → hellgrau (`grey75`),
3. **ausgewählte MASTER_IDs** → rot (`COL_TEMP`), beschriftet (ggrepel, sonst
   `geom_text`).

Bundesland **und** MASTER_ID(s) sind frei wählbar; alle Spaltennamen lassen
sich über die `*_col`-Argumente anpassen.

```r
source("02_function/WL_Diagramme/walther_lieth_helpers.R")  # COL_TEMP (Rot)
source("02_function/WL_Diagramme/climate_space.R")

# df: eine Zeile je Punkt (je Lauf): MASTER_ID | BL | Zeitlauf | MAT | MAP
plot_climate_space(df,
  master_ids   = c(70000123, 70000456),   # zwei Standorte rot
  highlight_bl = "MV",                     # Bundesland hellgrau
  run          = "OBS_DWD_1991-2020")      # Klimalauf-Filter

# beliebiges anderes Bundesland / andere IDs:
plot_climate_space(df, master_ids = 70012345, highlight_bl = "BY")
```

Liegen die Jahreswerte nur als WaLi-Trend-Long-Format vor (`id | Zeitlauf |
T_year | P_year`), erzeugt `climate_space_from_wali_trend()` daraus die
Punkt-MAT/MAP (Mittel über die Jahre) und benennt `id → MASTER_ID`:

```r
cs <- climate_space_from_wali_trend(ts, run = "OBS_DWD_1991-2020")
plot_climate_space(cs, master_ids = c(...), highlight_bl = "MV", run = NULL)
```

Beispieldaten: `source("02_function/WL_Diagramme/testdata/make_climate_space_test.R")`
schreibt `testdata/climate_space_test.csv`
(`MASTER_ID | BL | Zeitlauf | MAT | MAP`) und zeichnet einen Kontroll-Plot.

## Hinweise

- `scale = NULL` prüft automatisch per Plausibilität, ob noch ×10 in den
  Werten steckt (warnt und korrigiert). Mit `scale = 1` / `scale = 0.1`
  explizit überschreiben.
- Layer-Reihenfolge muss Jan→Dez sein – bei `ymonmean` der Fall, vorab
  per `terra::time()` / Layer-Namen kurz kontrollieren.
- `plot_walther_lieth()` liefert ein **ggplot-Objekt** zurück (mit `print()`
  zeichnen, mit `ggplot2::ggsave()` speichern). Alternative mit identischer
  Konvention: `climatol::diagwl()`.

## Testdaten

Unter `02_function/WL_Diagramme/testdata/` liegen fertige Beispiel-CSVs im Long-Format
(3 Punkte, realistische Zufallswerte) – direkt von den `*_from_long()`-
Funktionen lesbar, ohne Raster:

**Einzeldiagramme** (3 Punkte, mehrere Läufe):
- `walther_lieth_monthly_test.csv` – `id | Zeitlauf | altitude | Lon | Lat | Monat | T_mean | P_sum`
- `wali_trend_test.csv` – `… | Jahr | Kalenderjahr | T_year | P_year`

**Verschiebungs-Szenario** (1 Punkt, 3 Läufe mit zunehmendem Shift –
für Differenz-Diagramme und Empfehlungswechsel):
- `shift_monthly_test.csv`, `shift_wali_trend_test.csv`
- `recommendation_test.csv` – `id | Zeitlauf | Baumart | Empfehlung`

```r
wl <- read.csv("02_function/WL_Diagramme/testdata/walther_lieth_monthly_test.csv")
plot_walther_lieth_from_long(wl, id_val = 70041234, run = "OBS_DWD_1961-1990")

ts <- read.csv("02_function/WL_Diagramme/testdata/wali_trend_test.csv")
plot_wali_trend_from_long(ts, id_val = 70041234, run = "RCP85_HADWRF_2071-2100")
```

Neu erzeugen:
- `source("02_function/WL_Diagramme/testdata/make_test_data.R")` – Einzeldiagramm-Daten
- `source("02_function/WL_Diagramme/testdata/make_shift_test_data.R")` – Verschiebungs-Szenario

## Pakete

- Aufbereitung: `terra`, `dplyr`, `tidyr`, `purrr`
- Plot: `ggplot2`, `dplyr` (optional `scales`)
- Vergleich/Kombination: `patchwork`
