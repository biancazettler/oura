source("R/JC_correlation_analysis.R")


################## VARIABLENSELEKTION #################

### Korrelationen
# Entfernen aller Spalten mit "Schlaf" im Namen, außer "Sleep.Score", da das die ZG ist
data_imputed_jc_clean <- data_imputed_jc %>% rename(schlaf = Sleep.Score)
data_imputed_jc_clean <- data_imputed_jc_clean %>%
  select(-matches("Sleep"))
data_imputed_jc_clean <- data_imputed_jc_clean %>% rename(Sleep.Score = schlaf)


# Konvertiere alle relevanten Variablen, die als character statt als numerisch gespeichert sind
#data_imputed_jc_clean$Bedtime.Start <- as.numeric(as.POSIXct(data_imputed_jc_clean$Bedtime.Start, format="%Y-%m-%dT%H:%M:%S"))
#data_imputed_jc_clean$Bedtime.End <- as.numeric(as.POSIXct(data_imputed_jc_clean$Bedtime.End, format="%Y-%m-%dT%H:%M:%S"))
data_imputed_jc_clean$Temperature.Deviation...C. <- as.numeric(data_imputed_jc_clean$Temperature.Deviation...C.)
data_imputed_jc_clean$Temperature.Trend.Deviation <- as.numeric(data_imputed_jc_clean$Temperature.Trend.Deviation)
data_imputed_jc_clean$Stay.Active.Score <- as.numeric(data_imputed_jc_clean$Stay.Active.Score)
data_imputed_jc_clean$Move.Every.Hour.Score <- as.numeric(data_imputed_jc_clean$Move.Every.Hour.Score)
data_imputed_jc_clean$Meet.Daily.Targets.Score <- as.numeric(data_imputed_jc_clean$Meet.Daily.Targets.Score)
data_imputed_jc_clean$Training.Frequency.Score <- as.numeric(data_imputed_jc_clean$Training.Frequency.Score)
data_imputed_jc_clean$Training.Volume.Score <- as.numeric(data_imputed_jc_clean$Training.Volume.Score)
data_imputed_jc_clean$Previous.Day.Activity.Score <- as.numeric(data_imputed_jc_clean$Previous.Day.Activity.Score)
data_imputed_jc_clean$Activity.Balance.Score <- as.numeric(data_imputed_jc_clean$Activity.Balance.Score)
data_imputed_jc_clean$Temperature.Score <- as.numeric(data_imputed_jc_clean$Temperature.Score)
data_imputed_jc_clean$Resting.Heart.Rate.Score <- as.numeric(data_imputed_jc_clean$Resting.Heart.Rate.Score)
data_imputed_jc_clean$HRV.Balance.Score <- as.numeric(data_imputed_jc_clean$HRV.Balance.Score)
data_imputed_jc_clean$Recovery.Index.Score <- as.numeric(data_imputed_jc_clean$Recovery.Index.Score)

data_imputed_jc_clean$date <- as.Date(data_imputed_jc_clean$date)

# wochenend effekt
data_imputed_jc_clean$is_weekend <- ifelse(weekdays(data_imputed_jc_clean$date) %in% c("Samstag", "Sonntag"), 1, 0)

# externe Variable Mondphasen mit ins Modell:
library(lunar)

# Mondphasen für jedes Datum berechnen
data_imputed_jc_clean$moon_phase <- lunar.phase(data_imputed_jc_clean$date, name = TRUE)

# Mondphasen als Faktor umwandeln, um sie im Modell zu verwenden
data_imputed_jc_clean$moon_phase <- factor(data_imputed_jc_clean$moon_phase)

# Umwandeln von moon_phase in eine numerische Variable
data_imputed_jc_clean$moon_phase_numeric <- as.numeric(factor(data_imputed_jc_clean$moon_phase, 
                                                             levels = c("New", "Waxing", "Full", "Waning")))
# new = 1, waxing = 2, full = 3, waning = 4

library("xlsx")
# externe einflussgröße wetter für münchen:
weather <- read.xlsx("data/weather.xlsx", sheetIndex = 1)
head(data_imputed_jc_clean)
summary(data_imputed_jc_clean)

weather$date <- as.Date(weather$date, format = "%Y-%m-%d")

# Verbinde die Wetterdaten mit Daten
library(dplyr)
data_imputed_jc_clean <- left_join(data_imputed_jc_clean, weather, by = "date")

head(data_imputed_jc_clean)

# Überprüfen, ob die Umwandlung korrekt erfolgt ist
summary(data_imputed_jc_clean)



# Führe die Korrelationsanalyse mit den numerischen Variablen durch
numeric_columns_clean <- sapply(data_imputed_jc_clean, is.numeric)
cor_num_clean <- cor(data_imputed_jc_clean[, numeric_columns_clean], use = "complete.obs")
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
ausgewaehlte_variablen <- data_imputed_jc_clean[, c("date_numeric", 
                                                   "Resting.Heart.Rate.Score", 
                                                   "Respiratory.Rate", 
                                                   "HRV.Balance.Score", 
                                                   "Recovery.Index.Score", 
                                                   "Temperature.Score", 
                                                   "Previous.Day.Activity.Score", 
                                                   "Meet.Daily.Targets.Score",
                                                   "tavg", "Sleep.Score", "tsun")]

# Lineares Modell mit den ausgewählten Variablen für die VIF-Berechnung erstellen
vif_modell <- lm(Sleep.Score ~ ., data = ausgewaehlte_variablen)

# VIF für jede unabhängige Variable berechnen
vif_werte <- vif(vif_modell)

# VIF-Werte anzeigen
print(vif_werte)

### LASSO Regularisierung:
library(glmnet)

# Daten vorbereiten: Prädiktoren und Zielvariable
# Angenommen, data_imputed_jc_clean ist unser Dataframe und Sleep.Score ist die Zielvariable
x <- as.matrix(data_imputed_jc_clean[, c("date_numeric", "Resting.Heart.Rate.Score", 
                                        "Respiratory.Rate", "HRV.Balance.Score", 
                                        "Recovery.Index.Score", "Temperature.Score", 
                                        "Previous.Day.Activity.Score", 
                                        "Meet.Daily.Targets.Score", "tavg", "tsun")])
y <- data_imputed_jc_clean$Sleep.Score

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

# bayesianische lineare regression
library(brms)
model_linear <- brm(Sleep.Score ~ date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
                      HRV.Balance.Score + Recovery.Index.Score + Temperature.Score + 
                      Previous.Day.Activity.Score + Meet.Daily.Targets.Score + tavg + tsun, 
                    data = data_imputed_jc_clean, family = gaussian())

summary(model_linear)

# interaktionsterme:
library(brms)

# Definiere die Formel mit allen Haupt- und möglichen Interaktionstermen
formula <- bf(Sleep.Score ~ (date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
                               HRV.Balance.Score + Recovery.Index.Score + Temperature.Score +
                               Previous.Day.Activity.Score + Meet.Daily.Targets.Score + tavg)^2)
# durch ^2 werden alle möglichen Haupt- und Interaktionsterme bis zum zweiten Grad aufgenommen

# Setze einen Horseshoe-Prior für die Koeffizienten für variablenselektion
priors <- prior(horseshoe(scale_global = 1), class = "b")
# Horseshoe-Prior: Ein sparsamer Prior, der dazu führt, dass unwichtige Koeffizienten gegen Null gehen

# Fitte das Modell
###### dauert zu lange #########
# model_bayes <- brm(formula,
#                    data = data_imputed_jc_clean,
#                    family = gaussian(),
#                    prior = priors,
#                    control = list(adapt_delta = 0.95), #control-Parameter: Anpassungen zur Verbesserung der Konvergenz des Modells
#                    cores = 4)

# Zusammenfassung des Modells
summary(model_bayes) # keine ergebnisse

library(brms)

# Formel definieren, die nur die gewünschten Interaktionen enthält
formula_in <- bf(Sleep.Score ~ 
                (Resting.Heart.Rate.Score + Respiratory.Rate + HRV.Balance.Score + Recovery.Index.Score) * 
                (Previous.Day.Activity.Score + Meet.Daily.Targets.Score) +
                (tsun * tavg))

# Definiere die Horseshoe-Regularisierung als Prior
priors_in <- prior(horseshoe(scale_global = 1), class = "b")

# Fitte das Modell mit den definierten Interaktionen
model_bayes_in <- brm(
  formula = formula_in,
  data = data_imputed_jc_clean,
  family = gaussian(),
  prior = priors_in,
  control = list(adapt_delta = 0.99),
  cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_bayes_in)

# -> lineares modell mit keiner interaktion:
formula <- bf(Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate + 
                HRV.Balance.Score + Recovery.Index.Score + 
                Previous.Day.Activity.Score + Meet.Daily.Targets.Score + 
                tavg + date_numeric + tsun) # moon_phase hatKI um 0

# Priors festlegen
priors <- prior(normal(0, 1), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

# Modell fitten
model_bayes <- brm(
  formula = formula,
  data = data_imputed_jc_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_bayes)

# Berechnung der Residuen
residuals <- as.vector(residuals(model_bayes))

fitted_values <- fitted(model_bayes)

fitted_values <- fitted(model_bayes)
plot(fitted_values, residuals, main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# nur für stetige variabeln: 
# Extrahiere die Residuen und bilde den Mittelwert über die Posterior-Samples
residuals_samples <- residuals(model_bayes, summary = FALSE)  # Hole Residuen ohne Zusammenfassung
residuals_mean <- apply(residuals_samples, 2, mean)           # Mittlere Residuen berechnen

# Überprüfe die Länge, um sicherzustellen, dass sie der Anzahl der Beobachtungen entspricht
length(residuals_mean) # Sollte 811 sein

# Filtere die Daten auf die Zeilen ohne fehlende Werte für die Variablen, die im Modell verwendet wurden
data_filtered <- data_imputed_jc_clean[complete.cases(data_imputed_jc_clean[c("Resting.Heart.Rate.Score", 
                                                                            "Respiratory.Rate", 
                                                                            "HRV.Balance.Score", 
                                                                            "Recovery.Index.Score", 
                                                                            "Previous.Day.Activity.Score", 
                                                                            "Meet.Daily.Targets.Score", 
                                                                            "tavg", "tsun",
                                                                            "date_numeric")]), ]


# Plot der Residuen vs. Respiratory Rate
plot(data_filtered$Respiratory.Rate, residuals_mean,
     main = "Residuals vs. Respiratory Rate",
     xlab = "Respiratory Rate",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Tavg
plot(data_filtered$tavg, residuals_mean,
     main = "Residuals vs. Tavg",
     xlab = "Tavg",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Tsun
plot(data_filtered$tsun, residuals_mean,
     main = "Residuals vs. Tsun",
     xlab = "Tsun",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Resting.Heart.Rate.Score
plot(data_filtered$Resting.Heart.Rate.Score, residuals_mean,
     main = "Residuals vs. Resting.Heart.Rate.Score",
     xlab = "Resting.Heart.Rate.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. HRV.Balance.Score
plot(data_filtered$HRV.Balance.Score, residuals_mean,
     main = "Residuals vs. HRV.Balance.Score",
     xlab = "HRV.Balance.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Recovery.Index.Score
plot(data_filtered$Recovery.Index.Score, residuals_mean,
     main = "Residuals vs. Recovery.Index.Score",
     xlab = "Recovery.Index.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Previous.Day.Activity.Score
plot(data_filtered$Previous.Day.Activity.Score, residuals_mean,
     main = "Residuals vs. Previous.Day.Activity.Score",
     xlab = "Previous.Day.Activity.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. Meet.Daily.Targets.Score
plot(data_filtered$Meet.Daily.Targets.Score, residuals_mean,
     main = "Residuals vs. Meet.Daily.Targets.Score",
     xlab = "Meet.Daily.Targets.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs. date_numeric
plot(data_filtered$date_numeric, residuals_mean,
     main = "Residuals vs. date_numeric",
     xlab = "date_numeric",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
