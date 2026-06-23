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
source("functions/walther_lieth_input.R")
source("functions/plot_walther_lieth.R")

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

## 30-Jahres-Verlauf (Jahresparameter 1049 / 1050)

`walther_lieth_timeseries.R` – „eine Art Walther-Lieth", aber über die Jahre
statt über die Monate, aus den **Jahres**-Parametern:

| ID   | Inhalt                              |
|------|-------------------------------------|
| 1049 | Jahresmitteltemperatur [°C]         |
| 1050 | Jahresniederschlagssumme [mm]       |

Hier sind die zwei Achsen **unabhängig** skaliert (die strikte 1 °C : 2 mm-
Kopplung ergibt nur bei Monatswerten Sinn). Gezeigt: Temperatur (rot, links)
und Niederschlag (blau, rechts) über die ~30 Jahre, mit linearen Trendlinien.

```r
source("functions/walther_lieth_input.R")      # fuer .wl_detect_scale()
source("functions/walther_lieth_timeseries.R")

# rast_list hier mit den JAHRES-Rastern (1049 + 1050) je Lauf,
# Reihenfolge erste Haelfte Temp, zweite Haelfte Niederschlag.
ts <- build_wl_timeseries_input(rast_list, geom = geom)
# -> id | Zeitlauf | Jahr | Kalenderjahr | T_year | P_year

plot_wl_timeseries_from_long(ts, id_val = 70041234,
                             run = "RCP45_MPIWRF_2071-2100")
```

## Hinweise

- `scale = NULL` prüft automatisch per Plausibilität, ob noch ×10 in den
  Werten steckt (warnt und korrigiert). Mit `scale = 1` / `scale = 0.1`
  explizit überschreiben.
- Layer-Reihenfolge muss Jan→Dez sein – bei `ymonmean` der Fall, vorab
  per `terra::time()` / Layer-Namen kurz kontrollieren.
- `plot_walther_lieth()` liefert ein **ggplot-Objekt** zurück (mit `print()`
  zeichnen, mit `ggplot2::ggsave()` speichern). Alternative mit identischer
  Konvention: `climatol::diagwl()`.

## Pakete

- Aufbereitung: `terra`, `dplyr`, `tidyr`, `purrr`
- Plot: `ggplot2`, `dplyr` (optional `scales`)
