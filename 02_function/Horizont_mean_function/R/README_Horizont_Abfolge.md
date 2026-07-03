# Horizontabfolge-Plots aus den SQLite-Bodendatenbanken

Skripte zum Einlesen der Horizontdaten aus den SQLite-Bodendatenbanken und
zum Zeichnen von Horizontabfolge-Profilen mit **standardisierter Farbwahl**
und **KA5-Koernungs-Symbolen** – als Ersatz fuer die frei gewaehlten
`hue/value/chroma`-Zuweisungen im urspruenglichen Skript.

## Dateien

Modul-Verzeichnis: `02_function/Horizont_mean_function/R/`

| Datei | Inhalt |
|-------|--------|
| `02_function/Horizont_mean_function/R/00_ka5_referenz.R` | Referenz-Tabellen: Standard-Horizontfarben (Munsell) und KA5-Bodenarten → Farbe + Koernungs-Symbol |
| `02_function/Horizont_mean_function/R/01_db_zugriff.R` | SQLite-Zugriff (ersetzt `sqlQuery`/RODBC), Komfort-Loader `lade_leitprofile()`, `lade_leitprofil_fuer()`, `lade_kartiereinheiten()`, `lade_alle_db()` |
| `02_function/Horizont_mean_function/R/02_horizont_abfolge_plot.R` | Aufbereitung + `horizont_abfolge_plot()`, `add_koernung_symbole()`, `koernung_legende()` |
| `Horizont_Abfolge_run.R` | Beispiel-Ausfuehrung (aus dem Projekt-Hauptverzeichnis) |

## Datenstruktur der SQLite-Dateien

Je Datenquelle eine Datei unter `01_data/Grundlagen/Bodendatenbank/`
(`MRS_BWI.sqlite3`, `MRS_BZE.sqlite3`, `MRS_NR.sqlite3`), darin die
Tabellen:

```
00_BESCHREIBUNG
01_KOPFDATEN
02_KARTIEREINHEITEN   <- fruehere Tabelle "..._02_Kartiereinheiten"
03_LEITPROFILE        <- fruehere Tabelle "..._03_Leitprofile"
04_BUNDESLAND
05_QUALITAETSSCHLUESSEL
```

Umstieg vom alten SQL-Server-Zugriff:

```r
# frueher:
DB_Lp_BWI <- sqlQuery(DB_Verbindung, "select * from dbo.MRS_BWI_03_Leitprofile")
# jetzt:
DB_Lp_BWI <- lade_leitprofile("BWI")     # liest 03_LEITPROFILE aus MRS_BWI.sqlite3
```

### Verknuepfung 03_LEITPROFILE ↔ 02_KARTIEREINHEITEN

`03_LEITPROFILE` traegt **kein** `SOEH_KRZ`/`BL` direkt, sondern nur `group_ID`:

```
group_ID = <BL>_<SOEH_KRZ>_<Version>     z.B. "MV_BiS_1", "ST_MüS_1", "MV_MüS/BiS_1"
```

`lade_leitprofile()` ergaenzt `SOEH_KRZ` (aus den eindeutigen
`(group_ID, SOEH_KRZ)`-Paaren der Kartiereinheiten) und leitet `BL` aus dem
`group_ID`-Praefix ab. **Wichtig:** die Kartiereinheiten werden vorher per
`distinct()` reduziert – ein direkter Join wuerde das Leitprofil um jede
`MASTER_ID` vervielfachen.

Gezielt eine Feinbodenform (entspricht der DB-Browser-Abfrage):

```r
bis_mv <- lade_leitprofil_fuer("BiS", region = "MV", quelle = "NR")
```

## Verwendung

```r
source("02_function/Horizont_mean_function/R/00_ka5_referenz.R")
source("02_function/Horizont_mean_function/R/01_db_zugriff.R")
source("02_function/Horizont_mean_function/R/02_horizont_abfolge_plot.R")

DB_Lp_NR <- lade_leitprofile("NR")

horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "BiS")               # alle Regionen
horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "BiS", region = "MV")# nur MV
horizont_abfolge_plot(DB_Lp_NR, soeh_krz = "MüS/BiS")           # Kombiform-Fallback
koernung_legende()

# Direkt ueber eine MASTER_ID (Quelle NR/BWI/BZE automatisch):
horizont_abfolge_plot_master("NR_130_08_66519")
```

### Plot ueber MASTER_ID

`horizont_abfolge_plot_master(master_id)` bzw. `lade_leitprofil_master(master_id)`
schlagen zur MASTER_ID die `group_ID` in `02_KARTIEREINHEITEN` nach und laden das
zugehoerige Leitprofil. Die **Quelle** wird aus dem MASTER_ID-Praefix erkannt
(`NR_…` → NR, `BWI_…` → BWI, `BZE_…` → BZE); ist das Praefix unklar, werden alle
drei Quellen durchsucht.

**Schluessel je Quelle:** Das Leitprofil wird zuerst ueber die `group_ID` gesucht
(NR/BWI: regionale Varianten je SOEH_KRZ). Gibt es dort keinen Treffer, wird ueber
die `SOEH_KRZ` gesucht – so funktioniert **BZE**, wo die `SOEH_KRZ` (die Nummer
hinter `BZE_`, z.B. `BZE_90742` → `90742`) direkt genau eine Horizontfolge
identifiziert.

**Ausweichlogik:** Hat die `group_ID` einer MASTER_ID kein eigenes Leitprofil –
typisch bei Kombiformen (`SOEH_KRZ = "MüS/BiS"`) – wird automatisch auf die
Teilformen **in derselben Region** ausgewichen (z.B. `MV_MüS_1` + `MV_BiS_1`).
Die Ausweichform wird in der Konsole gemeldet, im Plot als Untertitel vermerkt
und im Datensatz in den Spalten `AUSWEICH_VON` / `MASTER_ID_ANFRAGE` festgehalten.
Wird auch fuer die Teilformen kein Leitprofil gefunden, erfolgt eine klare Meldung.

### Region- und Kombiform-Logik

* **`region = NULL` (Standard):** es wird ueber **alle** Bundeslaender gesucht
  und geplottet. Kommt dieselbe `SOEH_KRZ` in mehreren Laendern vor, erscheinen
  beide als getrennte Profile (Profil-ID = `group_ID`, z.B. `MV_BiS_1`,
  `ST_BiS_1`). Mit `region = "MV"` wird auf ein Bundesland beschraenkt.
* **Kombiformen (`"MüS/BiS"`):** wird die Form nicht direkt gefunden, weicht das
  Script auf die Teilformen aus und plottet, was vorhanden ist (beide, nur
  `MüS` oder nur `BiS`) – mit entsprechender Meldung.

## Die „richtige" Farbwahl statt frei gewaehlter Munsell-Werte

Im Ausgangs-Skript wurden `hue/value/chroma` je Horizont-Buchstabe frei
gesetzt. Dieses Skript nutzt stattdessen zwei anerkannte Konventionen:

1. **Gemessene Munsell-Farbe (bevorzugt).** Enthaelt die Leitprofil-Tabelle
   eine Feldfarbe (Spalte wird automatisch erkannt: `MUNSELL`, `BODENFARBE`,
   `FARBE`, …), wird diese mit `aqp::parseMunsell()` in die tatsaechliche
   Bodenfarbe uebersetzt. Das ist die naturgetreueste Darstellung.

2. **KA5-Fallback ueber das genetische Horizont-Symbol.** Fehlt eine
   gemessene Farbe, greift eine an die Bodenkundliche Kartieranleitung (KA5)
   angelehnte Standard-Palette (`horizont_farb_referenz()`), z. B. Ah = dunkel
   humos, Bv = braun, Gr = blaugrau (reduziert), Sw = fahl marmoriert,
   C = hellgrau. Alle Werte sind als Munsell hinterlegt und zentral anpassbar.

## KA5-Koernungs-Symbole

Zusaetzlich zur Flaechenfarbe werden aus der **Bodenart** die vier
KA5-Hauptgruppen abgeleitet und als Textursymbole eingezeichnet. In den
NR/BWI/BZE-Datenbanken heisst diese Spalte **`BODART`** (wird automatisch
erkannt; Kandidatenliste in `01_db_zugriff.R`, `.BOART_KANDIDATEN`):

| Gruppe | Grossbuchstabe | Symbol |
|--------|----------------|--------|
| Sand    | S | Punkte |
| Schluff | U | kurze Striche |
| Lehm    | L | Punkte + Striche |
| Ton     | T | durchgehende Linien |

Die Zuordnung erfolgt ueber den **ersten Grossbuchstaben S/U/L/T** des Kuerzels;
die Korngroessen-Praefixe `f`/`m`/`g` (Fein-/Mittel-/Grobsand) sind klein und
werden korrekt uebersprungen (`mSfs` → S, `fS` → S). Liegt kein auswertbares
Kuerzel vor (z.B. `BODART = "NA"`, organische Horizonte), wird die Gruppe – wenn
moeglich – aus den Kornanteilen `SAND`/`SCHLUFF`/`TON` abgeleitet
(`gruppe_aus_anteilen()`).

> Hinweis: Die NR/BWI/BZE-Datenbanken enthalten **keine** gemessene
> Munsell-Feldfarbe – daher greift durchgaengig der KA5-Fallback ueber das
> genetische Horizont-Symbol. Die exakten KA5-Legendenfarben sind nicht frei
> verfuegbar; die hinterlegten Farb-/Munsell-Werte sind naturnahe Naeherungen
> und in `00_ka5_referenz.R` an einer Stelle definiert. Die Koernungs-Symbole
> werden ueber die Standard-Geometrie von `aqp::plotSPC()` (Breite 0.2,
> Tiefe = y) gezeichnet.
