source("R/variablenselektion_zv_sleepscore.R")


################## VARIABLENSELEKTION zv respiratory rate #################


# Führe die Korrelationsanalyse mit den numerischen Variablen durch
print(# Zeigt nur die Spalte "Sleep.Score" mit allen Zeilennamen
  cor_num_clean[, "Respiratory.Rate", drop = FALSE]
)

library(corrplot)
corrplot(cor_num_clean, method = "circle")

# interpretation: siehe notizen

### Multikollinearitätsprüfung:
# Berechnung des Variance Inflation Factor (VIF) für die ausgewählten Variablen:
# Notwendige Bibliothek laden
library(car)


# ändert sich noch:

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

# bayesianische lineare regression
library(brms)
model_linear <- brm(Sleep.Score ~ date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
                      HRV.Balance.Score + Recovery.Index.Score + Temperature.Score + 
                      Previous.Day.Activity.Score + Meet.Daily.Targets.Score + tavg, 
                    data = data_no_nas_jc_clean, family = gaussian())

summary(model_linear)

# interaktionsterme:
# Schritt 1: Fitten eines Modells mit allen potenziellen Interaktionstermen
full_model <- brm(Sleep.Score ~ date_numeric * Resting.Heart.Rate.Score * Respiratory.Rate 
                  * tavg * HRV.Balance.Score * Recovery.Index.Score * Temperature.Score *
                    Previous.Day.Activity.Score * Meet.Daily.Targets.Score, data = data_no_nas_jc_clean, family = gaussian())

# Schritt 2: Verwenden von `projpred` für die Variablenselektion
library(projpred)
varsel_results <- cv_varsel(full_model, method = 'forward')  # forward selection für wichtige Prädiktoren
summary(varsel_results)
library(MuMIn)

# modell mit allem haupttermen definieren
model_base <- lm(Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate + HRV.Balance.Score + 
                   date_numeric + Recovery.Index.Score + Temperature.Score + Previous.Day.Activity.Score +
                   Meet.Daily.Targets.Score + tavg, data = data_no_nas_jc_clean)
