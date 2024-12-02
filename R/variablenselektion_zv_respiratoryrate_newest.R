source("R/JC_correlation_analysis.R")


################## VARIABLENSELEKTION #################
# zielvariable Respiratory Rate

### Korrelationen
# Entfernen aller Spalten mit "Schlaf" im Namen
data_imputed_jc_clean <- data_imputed_jc %>%
  select(-matches("Sleep"))

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
print(# Zeigt nur die Spalte "Respiratory.Rate" mit allen Zeilennamen
  cor_num_clean[, "Respiratory.Rate", drop = FALSE]
)

library(corrplot)
corrplot(cor_num_clean[c("Resting.Heart.Rate.Score", "HRV.Balance.Score", 
                         "Temperature.Score", "Recovery.Index.Score", 
                         "date_numeric", "tsun", "tavg", "pres", 
                         "Meet.Daily.Targets.Score"), c("Resting.Heart.Rate.Score", "HRV.Balance.Score", 
                           "Temperature.Score", "Recovery.Index.Score", 
                           "date_numeric", "tsun", "tavg", "pres", 
                           "Meet.Daily.Targets.Score")], method = "circle")

# interpretation: potenziellen prädiktoren ausgewählt: Resting.Heart.Rate.Score,
# HRV.Balance.Score, Temperature.Score, Recovery.Index.Score, date_numeric, tsun, 
# tavg, pres (Luftdruck) und Meet.Daily.Targets.Score

### Multikollinearitätsprüfung:
# Berechnung des Variance Inflation Factor (VIF) für die ausgewählten Variablen:
# Notwendige Bibliothek laden
library(car)

# Ausgewählte Prädiktoren für die Multikollinearitätsprüfung
selected_data <- data_imputed_jc_clean[, c("Resting.Heart.Rate.Score", "HRV.Balance.Score", 
                                          "Temperature.Score", "Recovery.Index.Score", 
                                          "date_numeric", "tsun", "tavg", "pres", 
                                          "Meet.Daily.Targets.Score")]

# VIF-Berechnung
vif_values <- vif(lm(as.formula(paste("Respiratory.Rate ~ ", paste(names(selected_data),
                                                                   collapse=" + "))), data = data_imputed_jc_clean))
print(vif_values)

### LASSO Regularisierung:
library(glmnet)

# Nur vollständige Zeilen für die Lasso-Regression auswählen
data_complete <- data_imputed_jc_clean[complete.cases(data_imputed_jc_clean[c("Resting.Heart.Rate.Score", 
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
model_linear <- brm(Respiratory.Rate ~ date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
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
  data = data_imputed_jc_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
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

# Definiere die Priors mit stärkeren Annahmen
priors <- prior(normal(0, 0.5), class = "b") + # Engerer Prior für Regressionskoeffizienten
  prior(normal(0, 2), class = "Intercept") +  # Engerer Prior für den Intercept
  prior(student_t(3, 0, 2.5), class = "sigma") # Prior für den Fehlerterm

# Passe das Modell mit angepassten Kontrollparametern an
model_selected_interactions <- brm(
  formula = formula_interactions,   # Modellformel mit Interaktionen
  data = data_imputed_jc_clean,     # Datensatz ohne fehlende Werte
  family = gaussian(),              # Modellannahme für kontinuierliche Zielvariable
  prior = priors,                   # Definierte Priors
  control = list(
    adapt_delta = 0.99,             # Höhere Schwelle für Akzeptanzrate
    max_treedepth = 15             # Erhöhte maximale Baumtiefe
  ),                    # Angepasste Warmup-Phase
  cores = 4                         # Nutzung mehrerer CPU-Kerne für paralleles Rechnen
)

# Zusammenfassung des angepassten Modells anzeigen
summary(model_selected_interactions)

# Definiere die Formel für das Modell ohne Interaktionen
formula <- Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
  Temperature.Score + Recovery.Index.Score + date_numeric + tsun + tavg + 
  pres 

# Definiere die Priors, falls du spezifische Priors setzen möchtest
priors <- prior(normal(0, 1), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

set.seed(12345)  # Festlegen des Seeds vor dem Modelllauf
# Fit das bayesianische lineare Modell
model_bayes_res <- brm(
  formula = formula,
  data = data_imputed_jc_clean,  # Datensatz ohne NA-Werte
  family = gaussian(),           # Familie für lineares Modell
  prior = priors,                # Verwende die definierten Priors
  control = list(adapt_delta = 0.95),  # Anpassung für Stabilität bei der NUTS-Sampling
  cores = 4                      # Anzahl der CPU-Kerne für paralleles Rechnen
)

# Modellzusammenfassung anzeigen
summary(model_bayes_res)

# Berechnung der Residuen
residuals <- as.vector(residuals(model_bayes_res))

fitted_values <- fitted(model_bayes_res)

fitted_values <- fitted(model_bayes_res)
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
                                                                              "Temperature.Score", 
                                                                              "HRV.Balance.Score", 
                                                                              "Recovery.Index.Score", 
                                                                              "pres", 
                                                                              "Meet.Daily.Targets.Score", 
                                                                              "tavg", "tsun",
                                                                              "date_numeric")]), ]


# Plot Residuals vs Fitted Values for Resting.Heart.Rate.Score
plot(data_filtered$Resting.Heart.Rate.Score, residuals_mean,
     main = "Residuals vs Fitted Values for Resting.Heart.Rate.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for HRV.Balance.Score
plot(data_filtered$HRV.Balance.Score, residuals_mean,
     main = "Residuals vs Fitted Values for HRV.Balance.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for Temperature.Score
plot(data_filtered$Temperature.Score, residuals_mean,
     main = "Residuals vs Fitted Values for Temperature.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for Recovery.Index.Score
plot(data_filtered$Recovery.Index.Score, residuals_mean,
     main = "Residuals vs Fitted Values for Recovery.Index.Score",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for date_numeric
plot(data_filtered$date_numeric, residuals_mean,
     main = "Residuals vs Fitted Values for date_numeric",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for tsun
plot(data_filtered$tsun, residuals_mean,
     main = "Residuals vs Fitted Values for tsun",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for tavg
plot(data_filtered$tavg, residuals_mean,
     main = "Residuals vs Fitted Values for tavg",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for pres
plot(data_filtered$pres, residuals_mean,
     main = "Residuals vs Fitted Values for pres",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot Residuals vs Fitted Values for Meet daily targets
plot(data_filtered$Meet.Daily.Targets.Score, residuals_mean,
     main = "Residuals vs Fitted Values for pres",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

