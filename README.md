# Analyse der Einflussfaktoren auf Schlafqualität – Bachelorarbeit

## Projektübersicht
Dieses Repository enthält den Code und die Daten zur Analyse der Einflussfaktoren auf die Schlafqualität, basierend auf der Bachelorarbeit **"Bayesianische Analyse der Einflussfaktoren auf die Schlafqualität: Eine Fallstudie basierend auf Oura-Ring-Daten"**. Das Ziel dieser Arbeit ist es, die Hauptfaktoren zu identifizieren, die die Schlafqualität beeinflussen, mithilfe von bayesianischen Modellen und statistischen Analysen.

---

## Verzeichnisstruktur
Die Dateien und Ordner sind wie folgt strukturiert:

### **Hauptordner**
- **R/**: Enthält alle R-Skripte zur Datenvorbereitung, Analyse und Modellierung.
- **data/**: Beinhaltet die Datensätze, die für die Analyse verwendet wurden.
- **plots/**: Enthält generierte Plots und Visualisierungen aus den Analysen.
- **Bachelorarbeit_Zettler_elektronisch.pdf**: Die vollständige Bachelorarbeit als PDF.

### **R-Skripte**
1. **Datenvorbereitung.R**: Skript zur Bereinigung und Transformation der Daten.
2. **Deskriptive_Analyse.R**: Berechnung und Visualisierung von deskriptiven Statistiken.
3. **Variablenselektion_ZV_Schlafwert.R**: Auswahl relevanter Prädiktoren für die Zielvariable *Schlafwert*.
4. **Variablenselektion_ZV_Atemfrequenz.R**: Auswahl relevanter Prädiktoren für die Zielvariable *Atemfrequenz*.
5. **Modelle_ZV_Schlafwert.R**: Aufbau und Fit von Modellen mit der Zielvariable *Schlafwert*.
6. **Modelle_ZV_Atemfrequenz.R**: Aufbau und Fit von Modellen mit der Zielvariable *Atemfrequenz*.
7. **Modellevaluierung.R**: Vergleich der Modellgüte und Durchführung von Residuenanalysen.
8. **Alternative_Modelle.R**: Implementierung alternativer Modellansätze zur Validierung der Ergebnisse.

---

# Nutzung

## Schritte zur Reproduktion der Analyse:

### 1. Klonen Sie das Repository:
```bash
git clone https://github.com/biancazettler/oura.git
```

### 2. Laden Sie das Projekt in RStudio:
Öffnen Sie die `.Rproj`-Datei, falls vorhanden, oder navigieren Sie im Arbeitsverzeichnis zur gewünschten Datei.

### 3. Laden Sie die notwendigen Pakete:
Installieren Sie alle in den Voraussetzungen genannten R-Pakete.

### 4. Führen Sie die Datenvorbereitung durch:
Starten Sie mit dem Skript `Datenvorbereitung.R`, um die Rohdaten zu bereinigen und fehlende Werte zu imputieren.

### 5. Deskriptive Analyse:
Führen Sie das Skript `Deskriptive_Analyse.R` aus, um deskriptive Statistiken zu berechnen und Visualisierungen zu erstellen.

### 6. Variablenselektion:
- `Variablenselektion_ZV_Schlafwert.R`: Auswahl relevanter Prädiktoren für die Zielvariable *Schlafwert*.
- `Variablenselektion_ZV_Atemfrequenz.R`: Auswahl relevanter Prädiktoren für die Zielvariable *Atemfrequenz*.

### 7. Modelle fitten:
- `Modelle_ZV_Schlafwert.R`: Bayesianische Modelle für den *Schlafwert*.
- `Modelle_ZV_Atemfrequenz.R`: Bayesianische Modelle für die *Atemfrequenz*.

### 8. Modellbewertung:
Führen Sie das Skript `Modellevaluierung.R` aus, um die Modellgüte zu bewerten und Residuenanalysen durchzuführen.

---

## Kontakt

Für Rückfragen oder Anregungen:
- **Name:** Bianca Zettler
- **E-Mail:** [b.zettler@campus.lmu.de](mailto:b.zettler@campus.lmu.de)
- **GitHub:** [biancazettler](https://github.com/biancazettler)

