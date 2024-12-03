# Lade vorbereitende Daten und Abhängigkeiten
source("R/Datenvorbereitung.R")

# Lade notwendige Bibliotheken
library(corrplot)  # Für die Visualisierung von Korrelationen
library(car)       # Für Multikollinearitätsanalysen
library(glmnet)    # Für LASSO-Regularisierung
library(brms)      # Für Bayesianische Modelle

################### VARIABLENSELEKTION ###################
# Zielvariable: Respiratory Rate

### Korrelationen
# Entferne alle Spalten mit "Sleep" im Namen
data_clean <- data_imputed %>%
  select(-matches("Sleep"))

# Führe die Korrelationsanalyse durch
numeric_columns <- sapply(data_clean, is.numeric)
cor_matrix <- cor(data_clean[, numeric_columns], use = "complete.obs")

# Zeige Korrelationen mit der Zielvariablen Respiratory.Rate
print(cor_matrix[, "Respiratory.Rate", drop = FALSE])

# Visualisiere ausgewählte Korrelationen
corrplot(
  cor_matrix[c("Resting.Heart.Rate.Score", "HRV.Balance.Score", "Temperature.Score", 
               "Recovery.Index.Score", "date_numeric", "tsun", "tavg", "pres", 
               "Meet.Daily.Targets.Score"), 
             c("Resting.Heart.Rate.Score", "HRV.Balance.Score", "Temperature.Score", 
               "Recovery.Index.Score", "date_numeric", "tsun", "tavg", "pres", 
               "Meet.Daily.Targets.Score")], 
  method = "circle"
)

# Interpretation: Potenzielle Prädiktoren ausgewählt:
# Resting.Heart.Rate.Score, HRV.Balance.Score, Temperature.Score, Recovery.Index.Score,
# date_numeric, tsun, tavg, pres (Luftdruck), Meet.Daily.Targets.Score

### Multikollinearitätsprüfung
# Ausgewählte Prädiktoren für die Analyse
selected_predictors <- c(
  "Resting.Heart.Rate.Score", "HRV.Balance.Score", "Temperature.Score", 
  "Recovery.Index.Score", "date_numeric", "tsun", "tavg", "pres", "Meet.Daily.Targets.Score"
)

# Berechnung der VIF-Werte
vif_values <- vif(lm(
  as.formula(paste("Respiratory.Rate ~", paste(selected_predictors, collapse = " + "))),
  data = data_clean
))
print(vif_values)

### LASSO-Regularisierung
# Daten ohne fehlende Werte für ausgewählte Prädiktoren
data_complete <- data_clean[complete.cases(data_clean[c(selected_predictors, "Respiratory.Rate")]), ]

# Prädiktorenmatrix und Zielvariable erstellen
x <- model.matrix(
  Respiratory.Rate ~ ., 
  data = data_complete[, c(selected_predictors, "Respiratory.Rate")]
)[, -1]
y <- data_complete$Respiratory.Rate

# Fitte das Lasso-Modell
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Bestes Lambda anzeigen
best_lambda <- lasso_model$lambda.min
print(paste("Bestes Lambda:", best_lambda))

# Lasso-Koeffizienten anzeigen
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print("Lasso-Koeffizienten:")
print(lasso_coefficients)

# Visualisiere die Feature-Importance
plot(lasso_model)
