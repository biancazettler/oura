source("R/JC_correlation_analysis.R")


################## VARIABLENSELEKTION #################
# zielvariable Respiratory Rate

### Korrelationen
# Entfernen aller Spalten mit "Schlaf" im Namen, außer "Sleep.Score", da das die ZG ist
data_no_nas_jc_clean <- data_no_nas_jc %>%
  select(-matches("Sleep"))

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

data_no_nas_jc_clean$date <- as.Date(data_no_nas_jc_clean$date)

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
  cor_num_clean[, "Respiratory.Rate", drop = FALSE]
)

library(corrplot)
corrplot(cor_num_clean, method = "circle")

# interpretation: potenziellen prädiktoren ausgewählt: Resting.Heart.Rate.Score,
# HRV.Balance.Score, Temperature.Score, Recovery.Index.Score, date_numeric, tsun, 
# tavg, pres (Luftdruck) und Meet.Daily.Targets.Score

### Multikollinearitätsprüfung:
# Berechnung des Variance Inflation Factor (VIF) für die ausgewählten Variablen:
# Notwendige Bibliothek laden
library(car)

# Ausgewählte Prädiktoren für die Multikollinearitätsprüfung
selected_data <- data_no_nas_jc_clean[, c("Resting.Heart.Rate.Score", "HRV.Balance.Score", 
                                          "Temperature.Score", "Recovery.Index.Score", 
                                          "date_numeric", "tsun", "tavg", "pres", 
                                          "Meet.Daily.Targets.Score")]

# VIF-Berechnung
vif_values <- vif(lm(as.formula(paste("Respiratory.Rate ~ ", paste(names(selected_data),
                                                                   collapse=" + "))), data = data_no_nas_jc_clean))
print(vif_values)

### LASSO Regularisierung:
library(glmnet)

# Nur vollständige Zeilen für die Lasso-Regression auswählen
data_complete <- data_no_nas_jc_clean[complete.cases(data_no_nas_jc_clean[c("Resting.Heart.Rate.Score", 
                                                                            "HRV.Balance.Score", 
                                                                            "Temperature.Score", 
                                                                            "Recovery.Index.Score", 
                                                                            "date_numeric", 
                                                                            "tsun", 
                                                                            "tavg", 
                                                                            "pres", 
                                                                            "Meet.Daily.Targets.Score", 
                                                                            "Respiratory.Rate")]), ]

# Prädiktorenmatrix (ohne Intercept-Spalte) und Zielvariable neu erstellen
x <- model.matrix(Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score +
                    Temperature.Score + Recovery.Index.Score + date_numeric + 
                    tsun + tavg + pres + Meet.Daily.Targets.Score, 
                  data = data_complete)[, -1]

y <- data_complete$Respiratory.Rate

# Lasso-Regression mit Kreuzvalidierung durchführen
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Bestes Lambda auswählen und Koeffizienten anzeigen
best_lambda <- lasso_model$lambda.min
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print(lasso_coefficients)

# Visualisierung der Feature Importance
plot(lasso_model)

# bayesianische lineare regression
library(brms)
model_linear <- brm(Sleep.Score ~ date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
                      HRV.Balance.Score + Recovery.Index.Score + Temperature.Score + 
                      Previous.Day.Activity.Score + Meet.Daily.Targets.Score + tavg + tsun, 
                    data = data_no_nas_jc_clean, family = gaussian())

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
#                    data = data_no_nas_jc_clean,
#                    family = gaussian(),
#                    prior = priors,
#                    control = list(adapt_delta = 0.95), #control-Parameter: Anpassungen zur Verbesserung der Konvergenz des Modells
#                    cores = 4)

# Zusammenfassung des Modells
summary(model_bayes) # keine ergebnisse

library(brms)

# Modellformel mit Interaktionen zwischen den Gruppen
formula_interaction <- bf(
  Respiratory.Rate ~ (Resting.Heart.Rate.Score * HRV.Balance.Score) + (Recovery.Index.Score * Meet.Daily.Targets.Score) * HRV.Balance.Score +
    (date_numeric * tsun * tavg * pres)
)

# Prior für das Modell setzen (hier z.B. eine horseshoe-Prior)
priors <- prior(horseshoe(scale_global = 1), class = "b")

# Modell mit Interaktionen anpassen
interaction_model <- brm(
  formula = formula_interaction,
  data = data_no_nas_jc_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.99),
  cores = 4
)


# Zusammenfassung des Modells anzeigen
summary(interaction_model)

# Formel mit nur den Interaktionstermen > |0.05| in der Schätzung
formula_interactions <- bf(
  Respiratory.Rate ~ 
    Resting.Heart.Rate.Score * HRV.Balance.Score +
    Resting.Heart.Rate.Score * Recovery.Index.Score +
    HRV.Balance.Score * Meet.Daily.Targets.Score +
    date_numeric * tsun +
    date_numeric * tavg +
    tsun * pres
)

# Definiere Priors (beispielsweise Normal(0,1) für alle Prädiktoren)
priors <- prior(normal(0, 1), class = "b")

# Modell mit den ausgewählten Interaktionen
model_selected_interactions <- brm(
  formula = formula_interactions,
  data = data_no_nas_jc_clean,  # Daten ohne fehlende Werte
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),  # Kontrollparameter für bessere Konvergenz
  cores = 4  # Anzahl der genutzten Rechenkerne
)

# Zusammenfassung des Modells
summary(model_selected_interactions)

# Definiere die Formel für das Modell ohne Interaktionen
formula <- Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
  Temperature.Score + Recovery.Index.Score + date_numeric + tsun + tavg + 
  pres + Meet.Daily.Targets.Score

# Definiere die Priors, falls du spezifische Priors setzen möchtest
priors <- c(
  prior(normal(0, 5), class = "b"),  # Beispiel für schwach informative Priors für die Koeffizienten
  prior(normal(0, 10), class = "Intercept")  # Prior für den Intercept
)

# Fit das bayesianische lineare Modell
model_bayes_res <- brm(
  formula = formula,
  data = data_no_nas_jc_clean,  # Datensatz ohne NA-Werte
  family = gaussian(),           # Familie für lineares Modell
  prior = priors,                # Verwende die definierten Priors
  control = list(adapt_delta = 0.95),  # Anpassung für Stabilität bei der NUTS-Sampling
  cores = 4                      # Anzahl der CPU-Kerne für paralleles Rechnen
)

# Modellzusammenfassung anzeigen
summary(model_bayes_res)

# Assuming 'model' is your Bayesian model
fitted_values <- fitted(model_bayes_res)  # Get fitted values from the model
residuals <- residuals(model_bayes_res)   # Get residuals from the model

# Plot Residuals vs Fitted Values for Resting.Heart.Rate.Score
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for Resting.Heart.Rate.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for HRV.Balance.Score
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for HRV.Balance.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for Temperature.Score
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for Temperature.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for Recovery.Index.Score
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for Recovery.Index.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for date_numeric
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for date_numeric",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for tsun
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for tsun",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for tavg
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for tavg",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for pres
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values for pres",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

