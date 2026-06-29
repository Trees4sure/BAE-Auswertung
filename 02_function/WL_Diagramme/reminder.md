# Reminder — Walther-Lieth- / WaLi-Trend-Diagramme (BAE-Auswertung)

Kurzgedächtnis, was in `02_function/WL_Diagramme/` gebaut wurde, wie es
zusammenhängt und was noch offen ist.

## Letzter Stand (Session-Updates, Branch-HEAD `f60c81f`)
- **WL-Plot Fix (`plot_walther_lieth.R`, `wl_panel`), perhumide Zone:** (1) humide
  Schraffur bei `pt=50` kappen (`yend=pmin(ptf,50)`); (2) perhumide Fläche als EIN
  `geom_ribbon` (`ymax=pmax(ptf,50)`, `ymin=50`, `has_wet`) statt `wet`/`grp` →
  läuft am 100-mm-Punkt aus; (3) `ptf` im pt-Raum interpolieren
  (`approx(monate, monthly$pt, xf)`) wg. `wl_p2t`-Knick bei 100 mm. **Sichtbar nur
  bei dicker Fläche** — sonst Ausgabe neu erzeugen (`source()` / PNGs neu rendern).
- **Code-Stil-Wunsch des Nutzers (in `CLAUDE.md` festgehalten):** flach statt
  verschachtelt, sichtbare/anpassbare `ggplot()`-Aufrufe im Skript, debugbar,
  Logik in Schleifen statt in Helfer-/Wrapper-Funktionen. Neue Abschnitte 6.11/
  6.12/6.14 sind bewusst flach (alles im Loop, Plots inline) gehalten.
- **Empfehlungsdaten gibt es real NICHT** (nur Testdaten aus `make_test_data.R`)
  → Empfehlungs-Leisten/`combine_climate_recommendation` bleiben ohne Daten aussen vor.
- **NEU 6.11 — Lauf-Vergleich je MASTER_ID:** Referenz `OBS_DWD_1991-2020` vs. je
  Modell die Zukunft 2021-2050 + 2071-2100; oben WL-Small-Multiples, unten Delta.
  Modell-Gruppierung über `[0-9]{4}-[0-9]{4}` (NICHT `(19|20)..` — sonst fällt
  Endjahr **2100** raus!), Auswahl über Anfangsjahr 2021/2071 (fängt auch
  `2071-2099` von HADWRF). → `04_results/WL_compare/<Region>/<id>_<Modell>.png`.
- **NEU 6.12 — gestaffelter WL-Vergleich je MASTER_ID:** alle Läufe überlagert
  (rot T, blau P als P/2 auf der Temp-Achse), Szenario-Familie OBS_DWD/RCP45/RCP85
  als **Linientyp** der Familien-Mittel + Label rechts; De-Martonne `12P/(T+10)`
  als grüne Mittellinie + min/max-Hülle (per Linear-Massstab `a/b` in die Mitte
  gelegt — ggplot kann nur EINE Zweitachse). Punkte über `MID_auswahl`.
- **NEU 6.14 — Regions-Mittel-WL je Lauf:** Schleife über die Lauf-CSVs der Region,
  `wl_region_diagram(csv)` (Mittel über alle MASTER_ID). NICHT
  `plot_walther_lieth_from_long(wl_csvs, id_val="NR-08", ...)` — wl_csvs sind Pfade.
- **BUG NR-Datenaufbereitung — T/P vertauscht:** in den NR-CSVs stand `T_mean` =
  Monatsniederschlag (Beweis: Mittel(T) = Jahressumme/12 exakt). Ursache liegt
  beim NR-Einlesen (1155-Datei/Variable liefert Niederschlag), NICHT in der
  Aggregation (die hält T/P sauber). **Guard** in `.nr_extract_files`: warnt laut,
  wenn `value_t` == `value_p`. **Debug 6.13** zeigt Variable + Wertebereich je
  1155/1157-Datei. Fix = Quelle/Variable korrigieren + **6.8 komplett neu laufen**
  (alle NR-Lauf-CSVs überschreiben). OBS_DWD danach korrekt (9.6 °C).
- **`run` ≠ MASTER_ID:** `run`/Zeitlauf wählt die DATEI (`<Lauf>.csv`, alle IDs
  drin), `id` die Zeile darin. Mehrere Läufe × Punkte = zwei Schleifen (aussen
  `run`+`fread`, innen `id`); ein Lauf = nur Schleife über IDs.
- **v2/v3 NICHT mehr rausfiltern, sondern als EIGENE Läufe führen** (BWI-Trend):
  `expand_precip_versions()` in 6.4 koppelt je Lauf die Temp (1049) mit JEDER
  vorhandenen 1050-Version → Original = `<run>`, nachprediziert = `<run>_v2`/`_v3`.
  Behebt den „30 vs. 60 Layer"-Abbruch; Original bleibt erhalten, v2/v3 daneben.
- **Schreibweise unterschiedlich:** BWI `_v2`/`_v3` (Unterstrich), **NR `-v2`/`-v3`
  (Bindestrich)** → überall Regex **`[_-]v[23]`** (nicht `_v[23]`!).
- **NR-CSV-Export streamt** je (Region, Lauf) extern in 6.8 (Schleife über die
  Dateien, Partner exakt über Datei-Stamm `Region_Lauf`), nicht ein Riesen-`bind_rows`.
- **NR v2/v3 in die CSVs:** `wl_month/trend_nr_from_files(..., run_label=)` paart
  Basis-Temp (nur v1) positional mit dem v2/v3-Niederschlag → Lauf `<Run>-v2`.
- **Geo/Höhe/DGM an NR-CSVs:** `nr_geo` (Polygon-Zentroid Lon/Lat in 25832 +
  `DGM_NR.csv`: altitude=Elevation, Aspect/Slope/Exposition/Hangseite) per
  `left_join` vor `write_run_csvs`. Pfad: `01_data/Grundlagen/Geodaten/DGM_NR.csv`
  (mit `data.table::fread`, `;`/Punkt-Dezimal — NICHT `read.csv2`!).
- **6.9 BWI-Master streamt** Lauf für Lauf (statt 42-Mio-Zeilen-`group_by`).
- **Timeline neu:** `plot_wali_timeline()` = zwei gestapelte Panels (Temp/Niederschlag,
  `facet_grid` `scales="free_y"`), je Lauf eine Linie — keine Doppelachsen-Quetschung.
- **`wl_region_diagram(csv_path)`** (in `wl_master_id.R`): WL-Diagramm aus einer
  fertigen `<Region>/<Lauf>.csv`, Mittel über alle MASTER_ID — liest CSV, NICHT .nc.

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
| `wali_timeline.R` | `plot_wali_timeline()` — 1961-2100-Szenario-Vergleich, **zwei gestapelte Panels** (Temp/Niederschlag, `facet_grid scales="free_y"`), je Lauf eine Linie, `prec_mode="diff"` |
| `wali_trend_compare.R` | `compare_wali_trend()` (Default `facets`; `delta`/Mittel-Shift nur noch optional) |
| `walther_lieth_compare.R` | Monats-WL-Vergleich (facets + Delta) |
| `wl_master_id.R` | `wl_split_quelle()` (idempotent), `read_nc_id_grid()`, `build_bwi_master_lookup(geom=)`, `attach_master_id()`, `wl_aggregate_master()`, `attach_bwi_geometry()`, `write_region_csvs()`, **`write_run_csvs()`**, **`wl_region_diagram(csv_path)`** (WL aus fertiger CSV) |
| `wl_master_id_nr.R` | NR-Pfad (streamend): `load_nr_polygons()`, `nr_build_raster()`, `nr_extract_long()`, `wl_month_nr_from_files(..., run_label=)`, `wl_trend_nr_from_files(..., run_label=)` (`run_label`=positionale 1:1-Paarung für v2/v3) |
| `recommendation_strip.R` / `compose_overview.R` | Empfehlungs-Leiste / Gesamtgrafik |
| `climate_space.R` | `plot_climate_space()` — Klima-Wolken-Diagramm MAT(x)/MAP(y): alle Punkte dunkelgrau, Bundesland (`BL`, Default `MV`) hellgrau, ausgewählte `MASTER_ID`s rot; `climate_space_from_wali_trend()` als Brücke aus dem WaLi-Trend-Long-Format |
| `run_klimadiagramme.R` | **Orchestrator** (6.1–6.14): laden → WL/Trend → Region/Lauf-CSVs (6.1–6.9); je-MASTER_ID-Einzeldiagramme (6.10); Lauf-Vergleich Ref vs. Zukunft (6.11); gestaffelter WL + De-Martonne (6.12); T/P-Debug (6.13); Regions-Mittel-WL je Lauf (6.14) |

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
- **NR:** `wl_month_nr_from_files()` liefert direkt MASTER_ID-Polygonmittel;
  6.8 streamt je (Region, Lauf) und hängt vor dem Schreiben `nr_geo`
  (Lon/Lat/altitude + DGM-Lage) per `left_join(by="MASTER_ID")` an.
- Beide → gleiches Schema → `write_run_csvs(out_dir, region_col="quelle")`.
- NR-CSVs tragen jetzt zusätzlich `Lon|Lat|altitude|Aspect|Slope|Exposition|Hangseite`.

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
- **v2/v3 (RCP45 ECECMO=v2, MPICLM=v3):** sind die NACHPREDIZIERTEN Niederschläge.
  → **NICHT rausfiltern**, sondern als eigene Läufe führen (`expand_precip_versions`,
  `run_label`). Original (kompromittiert, aber Basis weiterer Rechnungen) bleibt.
  Muster **`[_-]v[23]`** (BWI `_v2`, NR `-v2`) — NICHT `[_v23]` (Zeichenklasse,
  matcht `_` überall) und NICHT nur `_v[23]` (verfehlt die NR-Bindestrich-Variante).
- **`nbrg` aus MASTER_ID:** Stellen 8-9 = Regionsnummer (Stellen 6-7 = "nr").
- **Encoding:** ältere Dateien als `\u`-Escapes; neue nutzen UTF-8 direkt.
- **R im Web-Container:** CRAN gesperrt (403). Pakete via apt: `apt-get install
  r-base-core r-cran-ggplot2 r-cran-patchwork` → Plots real renderbar.

## Offene Checks / TODOs
- [ ] `table(polygons$nbrg)` == NR-01..NR-11? (Region-Join verifizieren)
- [ ] `BWI-BZE_Klima_Boden_Join.csv`: heißen die Spalten `id_bwi_bze` /
      `master_id_boden`? Sonst `build_bwi_master_lookup(id_col=, master_col=)`.
- [ ] **NR-1155-Temp-Variable FALSCH → T/P-Swap (akut!):** `T_mean` enthielt
      Niederschlag. Mit Debug 6.13 prüfen, ob `nc.1155_function` aus der NR-1155-
      Datei wirklich Temperatur (`temp`, Range ~-5..25) liest — sonst Quelle/
      Variable fixen und **6.8 für alle NR-Läufe neu** schreiben.
- [ ] Fehlendes Jahr 1971 (Trend, Punkt 1): `P_year` dort NA/~0? Punkt-spezifisch
      prüfen — ggf. NA-Jahre im Plot interpolieren/auslassen statt Null-Balken.
- [ ] Plot-Kopf Höhe/Lon/Lat nach `attach_bwi_geometry` wirklich gefüllt?
- [ ] **`DGM_NR.csv` MASTER_ID-Format == Polygon-MASTER_ID?** (Join `as.character`,
      aber führende Nullen o.Ä. müssen exakt passen, sonst `altitude`=NA).
- [ ] **NR v2/v3-Export:** zu jedem `-v2`/`-v3`-Niederschlag eine Basis-Temp
      (`nr_1155`/`nr_1049`) mit gleichem Stamm vorhanden? (sonst `warning` + skip).
- [ ] `n_years_for()` deckt evtl. v2/v3 zu 21/29-Jahres-Läufen ab? (Suffix wird
      gestrippt; aktuell sind RCP45-v2/v3 = 30 J. → unkritisch).
- [ ] Echte Baumartenempfehlung über `master_id_boden` anbinden; DGM-Lage
      (Aspect/Slope/Exposition/Hangseite) steht in den NR-CSVs bereit.
- [x] **WL-Plot in R gerendert/verifiziert** (ggplot2 via apt). **Daten-Pipeline**
      (6.x, NR/BWI `.nc`→CSV) noch nicht durchgelaufen → offen.

## Branch
Entwicklung auf `claude/kind-hopper-30mip3`.
