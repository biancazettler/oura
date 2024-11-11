#source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

# correlation analysis
# 1. data with scores and no na's

cor_scores <- cor(scores_data_no_nas_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores <- scores_data_no_nas_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main <- cor(main_scores, use = "complete.obs")
print(cor_scores_main)

corrplot(cor_scores_main, method = "circle")

# 2. data with scores and imputed nas

cor_scores <- cor(scores_imputed_nas_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores_imputed <- scores_imputed_nas_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main_im <- cor(main_scores_imputed, use = "complete.obs")
print(cor_scores_main_im)

corrplot(cor_scores_main_im, method = "circle")

# Korrelation zwischen Sleep.Score und anderen Variablen mit daten mit imputierten nas
cor_matrix_sleep <- cor(data_imputed_jc %>% select(Sleep.Score, Total.Sleep.Duration, REM.Sleep.Score,
                                           Deep.Sleep.Score, Light.Sleep.Duration,
                                           Sleep.Latency.Score, Total.Sleep.Score, Sleep.Latency,
                                           REM.Sleep.Duration), use = "complete.obs")
print(cor_matrix_sleep)

# (relativ) hohe korrelation von sleep score mit total sleep duration, rem sleep score,
# deep sleep score, total sleep score, rem sleep duration und rem sleep score -> 
# sleep score aussagekräftig für die gesamt schlafqualität als zielgröße

# 3. with data with na's

# Korrelationen berechnen und anzeigen
cor_matrix <- cor(data_jc %>% select(Sleep.Score, Activity.Score, Average.Resting.Heart.Rate, Average.HRV, Temperature.Deviation...C.), use = "complete.obs")
print(cor_matrix)

# Korrelationen visualisieren (mit ggplot2 oder corrplot)
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Berechnung der Korrelationen zwischen Sleep.Score und potenziellen Prädiktoren
cor_matrix_prädiktoren <- cor(data_imputed_jc %>% 
                                select(Sleep.Score, Activity.Score, Total.Burn, Steps, 
                                       Average.Resting.Heart.Rate, Lowest.Resting.Heart.Rate, 
                                       Average.HRV, Temperature.Deviation...C.), 
                              use = "complete.obs")
print(cor_matrix_prädiktoren)

# Optional: Visualisierung der Korrelationen
library(corrplot)
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Lowest.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
library(car)
vif_model <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Lowest.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model)

# zu hohe Korrelation zwischen lowest resting heart rate und average resting heart rate 
# -> nur eins aufnehmen:
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
vif_model_easy <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model_easy)
# korrelation ist ok

# Korrelation aller Daten (für ZG Sleep.Score)

data_no_nas_jc[] <- lapply(data_no_nas_jc, function(x) {
  if (is.character(x) | is.factor(x)) {
    # Konvertiere in numerisch (NA bei Fehlern)
    as.numeric(as.character(x))
  } else {
    x # Lasse den ursprünglichen Wert, wenn schon numerisch
  }
})
# Datentypen aller Spalten anzeigen
str(data_no_nas_jc)

# für Korrelationsanalyse: alle Werte in numerische umwandeln
# Konvertiere die Spalte `date` in das Datumsformat
data_no_nas_jc$date <- as.Date(data_no_nas_jc$date, format = "%Y-%m-%d")
# Konvertiere `Bedtime.Start` und `Bedtime.End` in das POSIXct-Datumszeitformat
data_no_nas_jc$Bedtime.Start <- as.POSIXct(data_no_nas_jc$Bedtime.Start, format = "%Y-%m-%dT%H:%M:%S")
data_no_nas_jc$Bedtime.End <- as.POSIXct(data_no_nas_jc$Bedtime.End, format = "%Y-%m-%dT%H:%M:%S")

# Umwandlung von `date` in die Anzahl der Tage seit einem festen Startdatum
start_date <- as.Date("2021-02-17")
data_no_nas_jc$date_numeric <- as.numeric(data_no_nas_jc$date - start_date)

# Optional: Extrahiere Wochentag oder Monat
data_no_nas_jc$weekday <- as.numeric(format(data_no_nas_jc$date, "%w"))  # 0 = Sonntag, 1 = Montag, ...
data_no_nas_jc$month <- as.numeric(format(data_no_nas_jc$date, "%m"))

# Extrahiere die Stunde als numerischen Wert
data_no_nas_jc$Bedtime.Start_numeric <- as.numeric(format(data_no_nas_jc$Bedtime.Start, "%H")) + 
  as.numeric(format(data_no_nas_jc$Bedtime.Start, "%M")) / 60
data_no_nas_jc$Bedtime.End_numeric <- as.numeric(format(data_no_nas_jc$Bedtime.End, "%H")) + 
  as.numeric(format(data_no_nas_jc$Bedtime.End, "%M")) / 60

# Führe die Korrelationsanalyse mit den numerischen Variablen durch
numeric_columns <- sapply(data_no_nas_jc, is.numeric)
cor_num <- cor(data_no_nas_jc[, numeric_columns], use = "complete.obs")
print(cor_num)

library(corrplot)
corrplot(cor_num, method = "circle")

