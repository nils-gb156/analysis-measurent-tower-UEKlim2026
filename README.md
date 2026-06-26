# analysis-measurement-tower-UEKlim2026

Auswertung der **Temperatur- und Feuchtedaten** des eigens errichteten Messturms am Horstmarer Landweg (Münster). Klimatologie-Übung 2026, Gruppe 8 – Datenvorstellung „Temperatur und Feuchte" der Messstation.

Messzeitraum der vorliegenden Daten: **20.05.–14.06.2026**, 10-Minuten-Mittelwerte.

## Struktur

- `data/metMastHorstmarerLandweg.CSV` – Rohdaten der Messstation (`;`-getrennt, Dezimalpunkt, ISO-8859-1, 3 Kopfzeilen)
- `data/raw/` – Original-Excel-Datei der Wetterstation
- `analyse_temp_feuchte.R` – Auswertungsskript (Import, Aufbereitung, Grafiken)
- `output/figures/` – erzeugte Grafiken (PNG, 300 dpi)
- `output/aufbereitete_daten.csv` – aufbereiteter Datensatz inkl. berechneter absoluter Feuchte
- `Klimauebung_Gruppe8_Temperatur_Feuchte.pdf` – Präsentation
- `Messstation.JPEG` – Foto des Messturms

## Auswertungsskript

`analyse_temp_feuchte.R` führt folgende Schritte aus:

1. Import und Bereinigung (Encoding, Zeitstempel, Qualitätskontrolle)
2. Berechnung der **absoluten Feuchte** aus Temperatur und rel. Feuchte (Magnus-Formel)
3. Verläufe von Temperatur sowie relativer und absoluter Feuchte (Min/Mittel/Max)
4. Mittlere Tagesgänge und Zusammenhang Temperatur ↔ Feuchte (inkl. Korrelation)
5. Ereignisanalysen: strahlungsreicher Schönwettertag und Niederschlagsereignis im Vergleich mit Strahlung und Niederschlag

Datenlücken (29.–30.05. und Ende der Messperiode) werden in den Verlaufsgrafiken grau markiert und nicht überbrückt.

### Ausführen

Arbeitsverzeichnis auf das Repo-Root setzen, dann:

```r
source("analyse_temp_feuchte.R")
```

Benötigte R-Pakete: `tidyverse`, `lubridate`, `scales`, `patchwork`
(werden beim ersten Lauf bei Bedarf installiert). Die Grafiken landen in `output/figures/`.
