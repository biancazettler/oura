source("R/JC_correlation_analysis.R")


################## VARIABLENSELEKTION #################

### Korrelationen
# Entfernen aller Spalten mit "Schlaf" im Namen, außer "Sleep.Score", da das die ZG ist
data_no_nas_jc_clean <- data_no_nas_jc %>%
  select(-matches("Sleep"), Sleep.Score)

# Konvertiere alle relevanten Variablen, die als character statt als numerisch gespeichert sind
#data_no_nas_jc_clean$Bedtime.Start <- as.numeric(as.POSIXct(data_no_nas_jc_clean$Bedtime.Start, format="%Y-%m-%dT%H:%M:%S"))
#data_no_nas_jc_clean$Bedtime.End <- as.numeric(as.POSIXct(data_no_nas_jc_clean$Bedtime.End, format="%Y-%m-%dT%H:%M:%S"))
data_no_nas_jc_clean$Temperature.Deviation...C. <- as.numeric(data_no_nas_jc_clean$Temperature.Deviation...C.)
data_no_nas_jc_clean$Temperature.Trend.Deviation <- as.numeric(data_no_nas_jc_clean$Temperature.Trend.Deviation)
data_no_nas_jc_clean$Stay.Active.Score <- as.numeric(data_no_nas_jc_clean$Stay.Active.Score)
data_no_nas_jc_clean$Move.Every.Hour.Score <- as.numeric(data_no_nas_jc_clean$Move.Every.Hour.Score)
data_no_nas_jc_clean$Meet.Daily.Targets.Score <- as.numeric(data_no_nas_jc_clean$Meet.Daily.Targets.Score)
data_no_nas_jc_clean$Training.Frequency.Score <- as.numeric(data_no_nas_jc_clean$Training.Frequency.Score)
data_no_nas_jc_clean$Training.Volume.Score <- as.numeric(data_no_nas_jc_clean$Training.Volume.Score)
data_no_nas_jc_clean$Previous.Day.Activity.Score <- as.numeric(data_no_nas_jc_clean$Previous.Day.Activity.Score)
data_no_nas_jc_clean$Activity.Balance.Score <- as.numeric(data_no_nas_jc_clean$Activity.Balance.Score)
data_no_nas_jc_clean$Temperature.Score <- as.numeric(data_no_nas_jc_clean$Temperature.Score)
data_no_nas_jc_clean$Resting.Heart.Rate.Score <- as.numeric(data_no_nas_jc_clean$Resting.Heart.Rate.Score)
data_no_nas_jc_clean$HRV.Balance.Score <- as.numeric(data_no_nas_jc_clean$HRV.Balance.Score)
data_no_nas_jc_clean$Recovery.Index.Score <- as.numeric(data_no_nas_jc_clean$Recovery.Index.Score)

# wochenend effekt
data_no_nas_jc_clean$is_weekend <- ifelse(weekdays(data_no_nas_jc_clean$date) %in% c("Samstag", "Sonntag"), 1, 0)

# externe Variable Mondphasen mit ins Modell:
library(lunar)

# Mondphasen für jedes Datum berechnen
data_no_nas_jc_clean$moon_phase <- lunar.phase(data_no_nas_jc_clean$date, name = TRUE)

# Mondphasen als Faktor umwandeln, um sie im Modell zu verwenden
data_no_nas_jc_clean$moon_phase <- factor(data_no_nas_jc_clean$moon_phase)

# Umwandeln von moon_phase in eine numerische Variable
data_no_nas_jc_clean$moon_phase_numeric <- as.numeric(factor(data_no_nas_jc_clean$moon_phase, 
                                                             levels = c("New", "Waxing", "Full", "Waning")))
# new = 1, waxing = 2, full = 3, waning = 4

library("xlsx")
# externe einflussgröße wetter für münchen:
weather <- read.xlsx("data/weather.xlsx", sheetIndex = 1)
head(data_no_nas_jc_clean)
summary(data_no_nas_jc_clean)

weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

# Verbinde die Wetterdaten mit Daten
library(dplyr)
data_no_nas_jc_clean <- left_join(data_no_nas_jc_clean, weather, by = "date")

head(data_no_nas_jc_clean)

# Überprüfen, ob die Umwandlung korrekt erfolgt ist
summary(data_no_nas_jc_clean)



# Führe die Korrelationsanalyse mit den numerischen Variablen durch
numeric_columns_clean <- sapply(data_no_nas_jc_clean, is.numeric)
cor_num_clean <- cor(data_no_nas_jc_clean[, numeric_columns_clean], use = "complete.obs")
print(# Zeigt nur die Spalte "Sleep.Score" mit allen Zeilennamen
  cor_num_clean[, "Sleep.Score", drop = FALSE]
)

library(corrplot)
corrplot(cor_num_clean, method = "circle")

# interpretation: siehe notizen

### Multikollinearitätsprüfung:
# Berechnung des Variance Inflation Factor (VIF) für die ausgewählten Variablen:
# Notwendige Bibliothek laden
library(car)

# Auswahl der spezifischen Spalten für die Multikollinearitätsanalyse
ausgewaehlte_variablen <- data_no_nas_jc_clean[, c("date_numeric", 
                                                   "Resting.Heart.Rate.Score", 
                                                   "Respiratory.Rate", 
                                                   "HRV.Balance.Score", 
                                                   "Recovery.Index.Score", 
                                                   "Temperature.Score", 
                                                   "Previous.Day.Activity.Score", 
                                                   "Meet.Daily.Targets.Score",
                                                   "tavg", "Sleep.Score")]

# Lineares Modell mit den ausgewählten Variablen für die VIF-Berechnung erstellen
vif_modell <- lm(Sleep.Score ~ ., data = ausgewaehlte_variablen)

# VIF für jede unabhängige Variable berechnen
vif_werte <- vif(vif_modell)

# VIF-Werte anzeigen
print(vif_werte)

### LASSO Regularisierung:
library(glmnet)

# Daten vorbereiten: Prädiktoren und Zielvariable
# Angenommen, data_no_nas_jc_clean ist unser Dataframe und Sleep.Score ist die Zielvariable
x <- as.matrix(data_no_nas_jc_clean[, c("date_numeric", "Resting.Heart.Rate.Score", 
                                        "Respiratory.Rate", "HRV.Balance.Score", 
                                        "Recovery.Index.Score", "Temperature.Score", 
                                        "Previous.Day.Activity.Score", 
                                        "Meet.Daily.Targets.Score", "tavg")])
y <- data_no_nas_jc_clean$Sleep.Score

# Fehlende Werte in x durch den Spaltenmittelwert ersetzen
x[is.na(x)] <- colMeans(x, na.rm = TRUE)[col(x)[is.na(x)]]

# Lasso-Modell fitten
lasso_model <- cv.glmnet(x, y, alpha = 1)


# Bestes Lambda (Regularisierungsparameter) anzeigen
best_lambda <- lasso_model$lambda.min
print(paste("Bestes Lambda:", best_lambda)) # "Bestes Lambda: 0.022711870059618"

# Modell mit dem besten Lambda anzeigen
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print("Lasso-Koeffizienten:")
print(lasso_coefficients)

# Visualisierung der Feature Importance
plot(lasso_model)
