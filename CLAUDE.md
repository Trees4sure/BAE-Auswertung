# Hinweise für die Zusammenarbeit

## Code-Stil (Wunsch des Nutzers)

- **Flach statt verschachtelt.** Logik, die in eine Schleife passt, gehört in
  die Schleife – nicht in eigene Helfer-/Wrapper-Funktionen. Lieber ein paar
  Zeilen mehr im Loop als eine Funktion, die anderswo definiert ist.
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
