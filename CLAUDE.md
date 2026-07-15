# Hinweise für die Zusammenarbeit

## Git / Zielbranch (WICHTIG)

- **Aller Code kommt auf `BAE_Auswertung_Grafiken`.** Das ist der feste
  Arbeitsbranch des Nutzers. NICHT auf einem separaten `claude/…`-Session-Branch
  liegen lassen – auch wenn pro Session einer vorgegeben wird: den fertigen
  Commit per Fast-Forward nach `BAE_Auswertung_Grafiken` schieben.
- KEINE neuen Feature-Branches anlegen, sofern der Nutzer nicht ausdrücklich
  danach fragt. Kein Wildwuchs an Branches.

## Code-Stil (Wunsch des Nutzers)

- **Flach statt verschachtelt.** Logik, die in eine Schleife passt, gehört in
  die Schleife – nicht in eigene Helfer-/Wrapper-Funktionen. Lieber ein paar
  Zeilen mehr im Loop als eine Funktion, die anderswo definiert ist.
- **dplyr statt verschachtelter Schleifen (mehrfach so besprochen).** Aggregationen
  als `group_by()/summarise()`, Zuordnungen als `left_join()` – KEINE `for`-Schleifen
  mit `rbind()` und keine `for`-in-`for`-Konstrukte. Plots als EINEN durchgehenden
  `ggplot() + … + …`-Aufruf (jede Ebene eine Zeile), nicht schrittweise `p <- p + …`.
- **Sichtbarer, anpassbarer Code.** Plots als direkte, einfache `ggplot()`-Aufrufe
  im Skript, damit man Achsen/Farben/Layer direkt ändern kann. Keine
  Plot-Erzeugung in tief geschachtelten Funktionsketten verstecken.
- **Debugbar.** Der Nutzer will an jeder Stelle Zwischenergebnisse ansehen und
  Schritt für Schritt nachvollziehen können. Wenig "alles ist miteinander
  verwoben".
- Der Nutzer schreibt selbst eher wenig verschachtelt – daran orientieren.
- **Abschnitts-Überschriften im RStudio-Stil.** KEINE mehrzeiligen `#===`-Kästen,
  sondern einzeilig mit `----`-Markern, damit sie in der RStudio-Gliederung
  (Inhaltsverzeichnis) auftauchen:
  `# ----  ÜBERSCHRIFT (ggf. Zusatz) ----`

## Projekt-Notizen

- Empfehlungsdaten (`recommendation_test.csv`) existieren NUR als Testdaten aus
  `make_test_data.R`. Real liegen sie nicht vor → Empfehlungs-Leisten
  (`combine_climate_recommendation` etc.) sind ohne diese Daten nicht nutzbar.

### Auswertungs-Grafiken (`bae_auswertung_grafiken_standalone.R`)

- **Konsens-Balken** (`bae_konsens_function`, Skizze 1a): sortieren die Baumart-
  Achse **PRO FACETTE** (je Klimalauf/Zeit/Szenario eigene Reihenfolge, best-
  empfohlene rechts). Technik: `Baumart___Gruppe`-Schlüssel (reorder_within von
  Hand, KEIN tidytext) + `facet_wrap(scales = "free_x")`, Label strippt den
  Gruppen-Teil. `order_ref` (Default = obere Stufe) macht, dass zwei Stufen je
  Facette gleich liegen. Das ist so GEWOLLT (nur Baumarten ranken, unabhängig vom
  TV). Die Konsens-Kurve (1b) bleibt global sortiert.
- **`bae_konsens_facet_paar()`**: legt zwei Konsens-Stufen (Default oben 4st,
  unten 2st) mit patchwork UNTEREINANDER unter eine Überschrift. Analog zu
  `bae_modus_facet_paar` (das die Modus-Matrix zweier Stufen nebeneinander legt).
- **Modus-Matrix** (`bae_modus_matrix_function`, Skizze 2): Sortierung ist
  **GEWICHTET** (Summe der Empfehlungsstufe, `sum(Stufe)`) und **GLOBAL über alle
  Facetten** (eine einheitliche Zeilen-/Spalten-Reihenfolge, via `order_ref` /
  `.bae_ref_levels`). **Beides bewusst so – NICHT ändern:**
  - „Stumpf auszählen" statt gewichten wurde probiert und vom Nutzer **verworfen**
    („das ist schlechter"). Nicht wieder vorschlagen/umbauen.
  - Per-Facette-Sortierung (wie bei Konsens) wurde probiert und **verworfen** –
    der Nutzer will hier EINE einheitliche Reihenfolge über alle Facetten.
- **Folge der globalen Modus-Sortierung (WICHTIG, war ein langes Missverständnis):**
  Die Reihenfolge hängt von ALLEN einbezogenen Klimaläufen ab. Ändert man den
  `data <-` Filter bzw. `szenarien` (welche RCP-Läufe drin sind), verschiebt sich
  die Zeilen-/Spalten-Reihenfolge in **JEDEM** Panel – auch in OBS, obwohl dessen
  eigene Daten/Farben gleich bleiben. Das ist KEIN Bug, sondern Folge von „global".
- **Beschriftungsgrößen:** `bae_modus_facet_paar` und der `facet = TRUE`-Zweig der
  Modus-Matrix nutzen große Labels (Streifen/Achsen/Titel 25, Legende 20/25,
  Legende in 2 Zeilen). Der `facet = FALSE`-Zweig ebenfalls (auf Wunsch angeglichen).
- **Modus-y-Achse = Buchstaben (FEST/identitätsbasiert):** `.bae_tv_labeller(canon)`
  ist eine Factory; die Buchstaben A, B, C … werden in der **kanonischen
  (sortierten) TV_M-Reihenfolge** vergeben, NICHT nach Achsenposition – damit A/B/C
  über Grafiken/Standorte hinweg **denselben** TV meinen. **„TV2" ist AUS der
  Zählung ausgenommen** (behält seinen Namen, verbraucht keinen Buchstaben).
  `tv_letters = FALSE` zeigt die echten TV×Methode-Namen. Reine Anzeige, Daten
  unberührt. (War erst positions-basiert + TV2-mitzählend – bewusst verworfen,
  weil dann Standorte nicht vergleichbar sind.)
- **`bae_modus_standort_paar()`**: legt die facettierte Modus-Matrix ZWEIER
  Standorte (MASTER_IDs) für DIESELBE Stufe nebeneinander (patchwork ncol=2),
  je Standort EIGENE Baumart-Spalten (order_ref=NULL), aber gemeinsame Legende
  (`guides="collect"`) und gemeinsame TV→Buchstaben-Zuordnung (Union der TV_M via
  `tv_canon`). Eigener Ordner (`…/auswertung_standortpaar`).

### Zusammenarbeit / Missverständnisse vermeiden

- Nach JEDEM Commit **sowohl** `BAE_Auswertung_Grafiken` per Fast-Forward setzen
  **als auch pushen**. Einmal nur committet, nicht gepusht → der Nutzer arbeitete
  mit einer veralteten Version und suchte lange den „Fehler".
- Wenn der Nutzer seine „aktuelle Standalone" pastet, kann sie ÄLTER sein als der
  Branch (z. B. ohne `bae_konsens_facet_paar`). NICHT blind die ganze Datei damit
  überschreiben – gezielt nur die gemeinte Änderung übernehmen und darauf hinweisen.

### Doppelte Sidebar / doppelte Element-IDs (WICHTIG bei UI-Änderungen)

- `karte_sidebar` wird in **zwei** Tabs eingebaut (Tab „Karte" **und** Tab
  „Analyse"). Das erzeugt für JEDE ID im Sidebar-Block eine Dublette im DOM
  (`stufe`, `run_karte`, `save_png`, …). Für normale Inputs verkraftet Shiny das
  (beide Kopien melden auf denselben `input$…`), **aber Download-Links
  (`downloadButton`) werden bei doppelter ID NICHT verdrahtet** → Button grau /
  nicht klickbar, OHNE Konsolenfehler. (Bug 2026-07: „PNG speichern" tot.)
- Fix (Weg 1, minimal): `karte_sidebar` ist jetzt eine **Funktion**
  `karte_sidebar(with_export = TRUE)`. Der Export-Block (HTML/PNG/Schleife)
  erscheint nur bei `with_export = TRUE` → nur im Karte-Tab, also nur EINMAL.
  Analyse-Tab ruft `karte_sidebar(with_export = FALSE)` und hat eigene Downloads.
- **Konsequenz:** Neue `downloadButton()`/`downloadHandler()` gehören in den
  Export-Block (oder an eine sonst eindeutige Stelle) – NIE ungeschützt in den
  doppelt genutzten Sidebar-Teil, sonst brechen sie wieder still.

### Schleifen-Export (Batch-PNG)

- Button „Als Schleife abspeichern" (Export) → Dialog (Baumart alle/Auswahl,
  Stufe alle/3st/4st/5st) → `observeEvent(input$run_schleife)`: rendert je
  Baumart × Stufe eine Karte (Look wie `output$save_png`, Einfärbung nach
  Empfehlung) und legt sie unter `04_results/BAE_Auswertung/maps/<TV>/` ab.
  Dateiname `BAE_{NR|BWI}_{Szenario}_{Modell}_{Zeitraum}_{Baumart}_{stufe}.png`
  (ohne Zeitstempel, überschreibbar). Lädt unabhängig von `filtered_raw()`.
- **Einzel-Export „PNG/HTML speichern"** sind KEINE `downloadButton` mehr,
  sondern `actionButton` + `observeEvent`: sie schreiben die AKTUELLE Karte
  (aktuelle Baumart-Auswahl, Stufe, Farb-Modus) in denselben Ordner mit
  gleichem Namensschema (Baumart-Token = gewählte Baumarten mit „-" verbunden).
  HTML: Geometrie-Dispatch (NR → `addPolygons`, sonst `addCircleMarkers`),
  sonst zerfällt jedes NR-Polygon in tausende Vertex-Punkte.
- Auflösung PNG (Einzel + Schleife): `ggsave(width=40, height=34, units="cm",
  dpi=600)` → ~9450×8030 px (~76 MP). dpi ist der Regler für die Auflösung.
- NR-Polygone im PNG **ohne Rand** (`color = NA`): bei ~15k dichten Polygonen
  bildet ein Rand sonst ein dunkles Gitter, das die Fläche grob wirken lässt.
  (Interaktive Karte + HTML behalten dünnen Rand für Klickbarkeit.)
