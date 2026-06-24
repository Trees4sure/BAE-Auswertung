# Reminder — Walther-Lieth- / WaLi-Trend-Diagramme (BAE-Auswertung)

Kurzgedächtnis, was in `02_function/WL_Diagramme/` gebaut wurde, wie es
zusammenhängt und was noch offen ist.

## Ziel
Pro BWI-Punkt / NR-Region (MASTER_ID) und Klimalauf Klima-Diagramme erzeugen
(Klick auf Punkt/Polygon), um Baumartenempfehlungen zwischen Klimaläufen zu
vergleichen und den **Grund** für einen Empfehlungswechsel sichtbar zu machen.

## Datengrundlage (Parameter-IDs)
| ID | Inhalt | Varname BWI / NR |
|----|--------|------------------|
| 1049 | Jahresmitteltemperatur | `tadm` / `tadm` |
| 1050 | Jahresniederschlagssumme | `rrds` / `rain` |
| 1155 | Monatsmittel-Temp (12 Layer) | `tadm` / `temp` |
| 1157 | Monatsniederschlag (12 Layer) | `rrds` / `rain` |

- **WL Monat:** 1155 + 1157 (je 12 Layer). **WaLi-Trend:** 1049 + 1050 (~30 Jahre).
- **Variablennamen je Region verschieden** → die Leser nehmen einen Kandidaten-
  Vektor (`c("rrds","rain")`) und fallen sonst auf die EINZIGE 3D-Variable zurück.

## Georeferenzierung — die zentrale Asymmetrie
| | BWI (`*bwi-bze*`) | NR (`*nr*`) |
|---|---|---|
| `.nc`-Koordinaten | **synthetisch** 1..304, kein CRS | **echt** EPSG:25832, 250 m |
| echte Koordinaten | aus 7001/7002 (→ `geom_bwi`) | direkt aus dem `.nc` |
| MASTER_ID | Tabellen-Join (id_bwi_bze→Boden) | räumlich (Polygon-Mittel) |

## Dateien in `02_function/WL_Diagramme/`
| Datei | Inhalt |
|-------|--------|
| `nc_monthly_tables.R` | `.nc_layer_table()` (ncdf4→breite Tabelle), `nc.1155/1157/1049/1050_function()`, `wl_long_from_tables()` (Monat), `wali_trend_from_tables()` (Trend). Mit `runs=`-Vorfilter + `split_quelle=` |
| `walther_lieth_input.R` | `build_walther_lieth_input()`, `wl_extract_run()`, `.wl_detect_scale()` (terra-Pfad) |
| `plot_walther_lieth.R` | WL-Diagramm + `plot_walther_lieth_from_long(df, id_val, run, id_col="id")` |
| `wali_trend.R` | `build_wali_trend_input()`, `plot_wali_trend()`, `plot_wali_trend_from_long()` |
| `wali_timeline.R` | **NEU** `plot_wali_timeline()` — durchgehender 1961-2100-Szenario-Vergleich (alpha-Balken + Linien, `prec_mode="diff"`) |
| `wali_trend_compare.R` | `compare_wali_trend()` (Default `facets`; `delta`/Mittel-Shift nur noch optional) |
| `walther_lieth_compare.R` | Monats-WL-Vergleich (facets + Delta) |
| `wl_master_id.R` | `wl_split_quelle()` (idempotent), `read_nc_id_grid()`, `build_bwi_master_lookup(geom=)`, `attach_master_id()`, `wl_aggregate_master()`, `attach_bwi_geometry()`, `write_region_csvs()`, **`write_run_csvs()`** |
| `wl_master_id_nr.R` | NR-Pfad (streamend): `load_nr_polygons()`, `nr_build_raster()`, `nr_extract_long()`, `wl_month_nr_from_files()`, `wl_trend_nr_from_files()` |
| `recommendation_strip.R` / `compose_overview.R` | Empfehlungs-Leiste / Gesamtgrafik |
| `run_klimadiagramme.R` | **Orchestrator** (6.1–6.9): laden → WL/Trend → Region/Lauf-CSVs |

Pakete: `terra, sf, exactextractr, ncdf4, dplyr, tidyr, ggplot2, patchwork`.

## App-Datenmodell (Kern!)
**Einmal vorrechnen + ablegen, App liest nur + plottet, gekeyt auf MASTER_ID.**
```
<out_base>/<Region>/<Lauf>.csv          (write_run_csvs)
  03_parameters/WL_diagrams/BWI-BZE/OBS_DWD_1991-2020.csv ...  (Monat)
  03_parameters/WL_diagrams_trend/NR-08/...                    (Trend)
```
App: Klick wählt Region+Lauf → passende (schon vorgefilterte) CSV laden →
`plot_walther_lieth_from_long(df, id_val=<MASTER_ID>, run, id_col="MASTER_ID")`.

- **BWI:** `wl_long_from_tables` (cell) → `attach_master_id(lookup)` →
  `wl_aggregate_master()` (Mittel je MASTER_ID, sonst >12 Monatszeilen).
- **NR:** `wl_month_nr_from_files()` liefert direkt MASTER_ID-Polygonmittel.
- Beide → gleiches Schema → `write_run_csvs(out_dir, region_col="quelle")`.

## Wichtige Funktions-Details
- **`runs=`-Vorfilter** (`wl_long_from_tables`/`wali_trend_from_tables`): nur
  bestimmte Läufe; Teilstring-Treffer auf den BEREINIGTEN Namen
  (`"OBS_DWD_1991-2020"` matcht trotz `bwi-bze_`-Präfix).
- **`split_quelle=TRUE`** (Default): Ergebnis trägt `quelle` (BWI/NR-XX) +
  bereinigten `Zeitlauf` (ohne `bwi-bze_`) → Läufe direkt benennbar.
- **Monats-Mapping über Spalten-POSITION** (1..12), nicht `Jan..Dez` (Locale!).
- **`nr_build_raster`** baut aus den tabelleneigenen x/y (NR = echte Meter).
- **`attach_bwi_geometry`**: Höhe/Lon/Lat reihenfolge-sicher über die 8002-id-
  WERTE (read_nc_id_grid im selben Gitter) → füllt den Plot-Kopf.
- **Region-Cross-Check** in den NR-Treibern: lauter Abbruch, wenn Datei-Regionen
  und `polygons$nbrg` sich nicht überschneiden (statt stiller Leer-CSV).

## Bekannte Caveats
- **v2/v3 (RCP45):** Muster **`_v[23]`** verwenden — `[_v23]` ist eine
  Zeichenklasse und matcht das `_` überall. v2/v3 betreffen auch den
  Niederschlag, müssen separat nachprediziert werden → im Hauptlauf raus.
- **`nbrg` aus MASTER_ID:** Stellen 8-9 = Regionsnummer (Stellen 6-7 = "nr").
- **Encoding:** ältere Dateien als `\u`-Escapes; neue nutzen UTF-8 direkt.
- **Nicht in R getestet** (Entwicklung ohne lokale R-Installation).

## Offene Checks / TODOs
- [ ] `table(polygons$nbrg)` == NR-01..NR-11? (Region-Join verifizieren)
- [ ] `BWI-BZE_Klima_Boden_Join.csv`: heißen die Spalten `id_bwi_bze` /
      `master_id_boden`? Sonst `build_bwi_master_lookup(id_col=, master_col=)`.
- [ ] NR-1155-Temp-Variable: ist `temp` korrekt? (sonst greift Single-3D-Fallback)
- [ ] Fehlendes Jahr 1971 (Trend, Punkt 1): `P_year` dort NA/~0? Punkt-spezifisch
      prüfen — ggf. NA-Jahre im Plot interpolieren/auslassen statt Null-Balken.
- [ ] Plot-Kopf Höhe/Lon/Lat nach `attach_bwi_geometry` wirklich gefüllt?
- [ ] Echte Baumartenempfehlung über `master_id_boden` anbinden.

## Branch
Entwicklung auf `claude/kind-hopper-30mip3`.
