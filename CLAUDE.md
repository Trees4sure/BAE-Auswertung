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

## Projekt-Notizen

- Empfehlungsdaten (`recommendation_test.csv`) existieren NUR als Testdaten aus
  `make_test_data.R`. Real liegen sie nicht vor → Empfehlungs-Leisten
  (`combine_climate_recommendation` etc.) sind ohne diese Daten nicht nutzbar.

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
