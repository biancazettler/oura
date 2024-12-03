# Lade die notwendigen Bibliotheken
# Bei Bedarf müssen alle mit library() geladenen pakete noch installiert werden mit install.packages("")
library(tidyverse) # Umfasst viele nützliche Pakete für Datenmanipulation und Visualisierung
library(dplyr)     # Datenmanipulation
library(rlang)     # Fehlerbehandlung und Programmierung
library(tidyr)     # Arbeiten mit Datenstrukturen
library(mice)      # Fehlende Daten behandeln

# Daten einlesen
data_raw <- read.csv("data/oura_2020-08-01_2024-10-13_trends.csv")

# Datenübersicht
head(data_raw)         # Zeige die ersten Zeilen der Daten
nrow(data_raw)         # Anzahl der Zeilen
ncol(data_raw)         # Anzahl der Spalten
summary(data_raw)      # Statistische Übersicht
min(data_raw$date)     # Startdatum
max(data_raw$date)     # Enddatum (Daten von mehr als 3,5 Jahren)

# Datenbereinigung und Vorverarbeitung
# Überprüfung der fehlenden Werte
sum(is.na(data_raw))   # Gesamtanzahl der fehlenden Werte

# Ermittlung der Spalten mit fehlenden Werten
missing_data_cols <- data_raw %>% summarise_all(~any(is.na(.)))
print(missing_data_cols) # Zeige Spalten mit fehlenden Werten
sum(missing_data_cols == TRUE) # Anzahl der betroffenen Spalten

# Ermittlung der Zeilen mit fehlenden Werten
missing_data_rows <- data_raw %>%
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique_missing_rows <- unique(missing_data_rows$row_id) # Betroffene Zeilen
length(unique_missing_rows)                             # Anzahl der betroffenen Tage
data_raw[unique_missing_rows, ]$date                   # Datumsangaben der betroffenen Tage

# Entfernen der Zeilen mit fehlenden Werten
data_no_nas <- data_raw[-unique_missing_rows, ]
sum(is.na(data_no_nas)) # Überprüfe auf fehlende Werte

# Anpassen der Datentypen
data_no_nas <- data_no_nas %>%
  mutate(
    Activity.Score = as.integer(Activity.Score),
    Sleep.Score = as.integer(Sleep.Score),
    Readiness.Score = as.integer(Readiness.Score),
    HRV.Balance.Score = as.integer(HRV.Balance.Score)
  )

# Entferne erneut fehlende Werte (falls durch Konvertierung entstanden)
data_no_nas <- na.omit(data_no_nas)
summary(data_no_nas)
nrow(data_no_nas) # Bereinigt: ~2,3 Jahre an Daten

# Alternative Methode: Imputation fehlender Werte
data_imputed <- data_raw # Kopiere den ursprünglichen Datensatz
data_imputed[data_imputed == "None"] <- NA # "None" als NA interpretieren

# Konvertiere relevante Datumsfelder
data_imputed$date <- as.Date(data_imputed$date, format = "%Y-%m-%d")
data_imputed$Bedtime.Start <- as.POSIXct(data_imputed$Bedtime.Start, format = "%Y-%m-%dT%H:%M:%OS")
data_imputed$Bedtime.End <- as.POSIXct(data_imputed$Bedtime.End, format = "%Y-%m-%dT%H:%M:%OS")

# Umwandlung der Datumsfelder in numerische Werte
data_imputed$date <- as.Date(data_imputed$date, format = "%Y-%m-%d")
start_date <- as.Date("2021-02-09")
data_imputed$date_numeric <- as.numeric(data_imputed$date - start_date)

# Numerische Imputation (Mittelwert)
data_imputed <- data_imputed %>%
  mutate_if(is.character, as.numeric)

for (i in 1:ncol(data_imputed)) {
  data_imputed[is.na(data_imputed[, i]), i] <- mean(data_imputed[, i], na.rm = TRUE)
}

nrow(data_imputed) # Zusätzliche Datenpunkte durch Imputation (~5 Monate mehr)

# Extraktion der Score-Daten (bereinigt und imputiert)
# Daten ohne fehlende Werte
scores_no_nas <- data_no_nas %>%
  select(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
         Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
         Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
         HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
         Resting.Heart.Rate.Score) %>%
  mutate_all(as.numeric) %>%
  na.omit()

# Daten mit imputierten Werten
scores_data <- data_imputed %>%
  select(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
         Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
         Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
         HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
         Resting.Heart.Rate.Score) %>%
  mutate_all(as.numeric)

# Zusammenfassung
cat("Anzahl der bereinigten Score-Daten:", nrow(scores_no_nas), "\n")
cat("Anzahl der imputierten Score-Daten:", nrow(scores_data), "\n")

# Konvertiere alle relevanten Variablen in numerische Werte
data_imputed$Temperature.Deviation...C. <- as.numeric(data_imputed$Temperature.Deviation...C.)
data_imputed$Temperature.Trend.Deviation <- as.numeric(data_imputed$Temperature.Trend.Deviation)
data_imputed$Stay.Active.Score <- as.numeric(data_imputed$Stay.Active.Score)
data_imputed$Move.Every.Hour.Score <- as.numeric(data_imputed$Move.Every.Hour.Score)
data_imputed$Meet.Daily.Targets.Score <- as.numeric(data_imputed$Meet.Daily.Targets.Score)
data_imputed$Training.Frequency.Score <- as.numeric(data_imputed$Training.Frequency.Score)
data_imputed$Training.Volume.Score <- as.numeric(data_imputed$Training.Volume.Score)
data_imputed$Previous.Day.Activity.Score <- as.numeric(data_imputed$Previous.Day.Activity.Score)
data_imputed$Activity.Balance.Score <- as.numeric(data_imputed$Activity.Balance.Score)
data_imputed$Temperature.Score <- as.numeric(data_imputed$Temperature.Score)
data_imputed$Resting.Heart.Rate.Score <- as.numeric(data_imputed$Resting.Heart.Rate.Score)
data_imputed$HRV.Balance.Score <- as.numeric(data_imputed$HRV.Balance.Score)
data_imputed$Recovery.Index.Score <- as.numeric(data_imputed$Recovery.Index.Score)
data_imputed$date <- as.Date(data_imputed$date)

# Wochenende identifizieren
data_imputed$is_weekend <- ifelse(weekdays(data_imputed$date) %in% c("Samstag", "Sonntag"), 1, 0)

# Externe Variable: Mondphasen
library(lunar)

# Berechne die Mondphasen für jedes Datum
data_imputed$moon_phase <- lunar.phase(data_imputed$date, name = TRUE)
data_imputed$moon_phase <- factor(data_imputed$moon_phase)

# Mondphasen in numerische Werte umwandeln
data_imputed$moon_phase_numeric <- as.numeric(factor(data_imputed$moon_phase, 
                                                   levels = c("New", "Waxing", "Full", "Waning")))

# Externe Variable: Wetterdaten für München
library(xlsx)
weather <- read.xlsx("data/weather.xlsx", sheetIndex = 1)
weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

# Wetterdaten mit den Hauptdaten verbinden
library(dplyr)
data_imputed <- left_join(data_imputed, weather, by = "date")

# Überprüfe die Daten nach der Verbindung
head(data_imputed)
summary(data_imputed)

