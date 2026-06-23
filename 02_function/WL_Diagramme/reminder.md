# Reminder — Walther-Lieth- / WaLi-Trend-Diagramme (BAE-Auswertung)

Kurzgedächtnis, was in `02_function/WL_Diagramme/` gebaut wurde, wie es
zusammenhängt und was noch offen ist.

## Ziel
Pro BWI-Punkt / NR-Zelle und Klimalauf Klima-Diagramme erzeugen (Klick auf
Punkt), um Baumartenempfehlungen zwischen Klimaläufen zu vergleichen und den
**Grund** für einen Empfehlungswechsel sichtbar zu machen.

## Datengrundlage (Parameter-IDs aus dem Katalog)
| ID | Inhalt | CDO-Ableitung |
|----|--------|---------------|
| 1112 | Tagesmittel-Temperatur (Roh, ×10) | – |
| 1114 | Tagesniederschlagssumme (Roh, ×10) | – |
| 1049 | Jahresmitteltemperatur (Varname `tadm`) | `yearmean(1112)` |
| 1050 | Jahresniederschlagssumme (Varname `rrds`) | `yearsum(1114)` |
| 1137 / 1155 | Monatsmitteltemperatur / 30a-Klimatologie | `monmean` / `ymonmean` |
| 1145 / 1157 | Monatsniederschlag / 30a-Klimatologie | `monsum` / `ymonmean` |

- **Echtes Walther-Lieth (Monat):** braucht **1155 + 1157** (je 12 Layer).
- **WaLi-Trend (30 Jahre):** braucht **1049 + 1050** (je ~30 Jahres-Layer);
  diese liegen bereits als `nc.grep.variables_BWI_KS` vor (60 Layer/Lauf).
- Rohdaten-Skalierung `mulc,0.1` (Tabelle 6) nur bei 1112/1114; die abgeleiteten
  Produkte sind i.d.R. schon in echten Einheiten (Plausibilitätscheck eingebaut).
- Wichtig: **Temperatur = Mittel, Niederschlag = Summe** (steckt korrekt in den IDs).

## Dateien in `02_function/WL_Diagramme/`
| Datei | Inhalt |
|-------|--------|
| `walther_lieth_input.R` | `build_walther_lieth_input()`, `wl_extract_run()` — Monats-Long-Format aus 1155/1157; `.wl_detect_scale()` |
| `plot_walther_lieth.R` | klassisches WL-Diagramm (4-Ecken-Kopf, humide/aride/perhumide, Frostbalken); `plot_walther_lieth_from_long()` |
| `wali_trend.R` | `build_wali_trend_input()`, `plot_wali_trend()` — 30-Jahres-Verlauf aus 1049/1050 |
| `walther_lieth_helpers.R` | geteilte Skalierung, Farben, Theme, `wl_patch()` (patchwork), Empfehlungs-Palette |
| `walther_lieth_compare.R` | Monats-WL: Small Multiples (`*_facets`) + Delta-WL (`*_delta`) + `compare_walther_lieth()` |
| `wali_trend_compare.R` | WaLi-Trend: Small Multiples + Mittel-Shift + `compare_wali_trend()` |
| `recommendation_strip.R` | Empfehlungs-Leiste (grid/facets) + `combine_climate_recommendation()` |
| `compose_overview.R` | `compose_scenario_overview()` — Gesamtgrafik (links WL untereinander, rechts Empfehlung + Delta je Szenario) |
| `testdata/` | CSV-Testdaten + Generatoren (`make_test_data.R`, `make_shift_test_data.R`) + `demo_compare.R` |

Pakete: `terra, dplyr, tidyr, purrr, ggplot2, patchwork` (optional `scales`).

## Bridge: von den Rohrastern zum WaLi-Trend-Long-Format
`nc.grep.variables_BWI_KS` (je Lauf 60 Layer = 30 `tadm` + 30 `rrds`) ist direkt
die `rast_list`. **KEIN `tapp`** (das mittelt die Jahre weg).

```r
source("02_function/WL_Diagramme/walther_lieth_input.R")  # .wl_detect_scale()
source("02_function/WL_Diagramme/wali_trend.R")

# geom optional: ohne geom ist id == Zellindex == id_bwi_bze (1..92123)
ts <- build_wali_trend_input(
  nc.grep.variables_BWI_KS,
  runs = c("OBS_DWD_1961-1990", "RCP45_MPICLM_2071-2100"),  # Lauf-Filter!
  ids  = 70041234)                                          # Punkt-Filter!
```

- **Immer `runs=`/`ids=` filtern** für Klick-Diagramme. Ungefiltert entstehen
  ~92.123 Punkte × ~30 Jahre × ~34 Läufe ≈ **114 Mio. Zeilen**.
- Layer-Erkennung über `1049|MAT|tadm` bzw. `1050|MAP|rrds`; 21/29-Jahres-Läufe
  werden automatisch korrekt getrennt.
- `geom` (Lon/Lat/altitude/id) wird durchgereicht, falls angegeben.

## id vs. master_id (für die Empfehlungs-Anbindung)
- `id` = **zellbasiert** = `id_bwi_bze` (1..92123) → direkter Raster-Join-Key.
- `master_id_boden` = **standorts-/polygonbasiert**, kann leer sein → daran hängen
  Bodendaten **und** Baumartenempfehlungen.
- Kette: `WaLi-Trend.id` → `id_bwi_bze` → `MRS_Bod_Klima_Schl` → `master_id_boden`
  → Empfehlungen. (Die Test-Empfehlungstabelle keyt aktuell auf `id`; real über
  `master_id_boden` verknüpfen.)

## Diagrammtypen
- **WL Monat** (`plot_walther_lieth`): echtes Walther-Lieth, 1 °C : 2 mm.
- **WaLi-Trend** (`plot_wali_trend`): 30-Jahres-Verlauf, zwei unabhängige Achsen,
  Trendlinien (strikte 1:2-Kopplung nur bei Monatswerten sinnvoll).
- **Differenz:** Small Multiples + Delta (Monat: echtes ΔWL; Trend: Mittel-Shift).
- **Empfehlungs-Leiste:** Stufen sehr/empfohlen/bedingt/nicht, Farbwechsel =
  Stufenwechsel; `aligned=TRUE` deckungsgleich unter den Klima-Facetten.
- **Gesamtübersicht** (`compose_scenario_overview`): links WL je Lauf, rechts oben
  Empfehlungswechsel, rechts darunter Delta je Szenario.

## Bekannte Caveats / Stolpersteine
- **v2/v3-Duplikate (ECECMO, MPICLM):** `grep("1049|1050")` zieht z. B. `1050_v2`
  mit → Lauf hat 90 statt 60 Layer (zeigte sich als „45 Jahre"). Die neue
  Erkennung bricht dann sauber ab (30 `tadm` vs. 60 `rrds`). **Fix upstream:**
  `grep("_v2|_v3", ..., invert = TRUE)` vor `terra::rast`.
- **Encoding:** Sonderzeichen in den R-Dateien als `\u`-Escapes (reines ASCII) →
  kein Mojibake beim `source()` (war Ursache der `Â·`/`Ã`-Anzeige).
- **Zell-Alignment:** `geom` muss zeilenweise zur `terra::as.data.frame`-Reihenfolge
  passen (gleiche NA-Maske über alle Layer eines Laufs).
- **Nicht in R getestet** (Entwicklung erfolgte ohne lokale R-Installation) —
  bitte mit `demo_compare.R` gegenprüfen.

## Offene TODOs
- [ ] Echte Baumartenempfehlung über `master_id_boden` statt `id` anbinden.
- [ ] v2/v3-Dedup in der Datei-Liste vor dem Raster-Stack.
- [ ] WL-Plot lokal verifizieren (Legende/override.aes, patchwork-Alignment).
- [ ] Optional: 1155/1157 für alle Läufe vorhanden? Sonst aus 1112/1114 via CDO
      (`ymonmean`/`monsum`) nachgenerieren.
- [ ] Optional: Warnung in `compare_wali_trend()` bei sehr großem (ungefiltertem) ts.

## Branch
Entwicklung auf `claude/kind-hopper-30mip3`.
