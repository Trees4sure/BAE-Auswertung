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
