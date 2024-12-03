# Lade vorbereitende Daten und Abhängigkeiten
source("R/Variablenselektion_ZV_Atemfrequenz.R")

# Lade notwendige Bibliotheken
library(brms)

################### BAYESIANISCHE MODELLE ###################

### Lineares Modell
# Einfaches lineares Modell ohne Transformationen
model_bayes_res <- brm(
  formula = Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
    Temperature.Score + Recovery.Index.Score + date_numeric + tsun + tavg + 
    pres + Meet.Daily.Targets.Score,
  data = data_clean,
  family = gaussian()
)

# Modellzusammenfassung
summary(model_bayes_res)

### Interaktionsmodell
# Definiere die Formel mit Interaktionen zwischen ausgewählten Prädiktoren
formula_interaction <- bf(
  Respiratory.Rate ~ (Resting.Heart.Rate.Score * HRV.Balance.Score) + 
    (Recovery.Index.Score * Meet.Daily.Targets.Score) + 
    (date_numeric * tsun * tavg * pres)
)

# Setze sparsamen Prior (Horseshoe)
priors_interaction <- prior(horseshoe(scale_global = 1), class = "b")

# VORSICHT: dauert sehr lange!
# Fitte das Modell mit Interaktionen
interaction_model <- brm(
  formula = formula_interaction,
  data = data_clean,
  family = gaussian(),
  prior = priors_interaction,
  control = list(adapt_delta = 0.95),
  cores = 4
)

# Modellzusammenfassung
summary(interaction_model)

### Lineares Modell ohne Interaktionen
formula_no_interaction <- Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
  Temperature.Score + Recovery.Index.Score + date_numeric + tsun + tavg + pres

# Priors für das Modell
priors_no_interaction <- prior(normal(0, 1), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

# Fitte das Modell
model_no_interaction <- brm(
  formula = formula_no_interaction,
  data = data_clean,
  family = gaussian(),
  prior = priors_no_interaction,
  control = list(adapt_delta = 0.95),
  cores = 4
)

# Modellzusammenfassung
summary(model_no_interaction)

################### RESIDUENANALYSE ###################
# Berechnung der Residuen und Fitted Values
residuals_mean <- apply(residuals(model_no_interaction, summary = FALSE), 2, mean)
fitted_values <- fitted(model_no_interaction)

# Residuenplot für verschiedene Prädiktoren
par(mfrow = c(3, 3))  # 3x3 Subplots
predictors <- c(
  "Resting.Heart.Rate.Score", "HRV.Balance.Score", "Temperature.Score", 
  "Recovery.Index.Score", "date_numeric", "tsun", "tavg", "pres", 
  "Meet.Daily.Targets.Score"
)

for (predictor in predictors) {
  plot(
    data_clean[[predictor]], residuals_mean,
    main = paste("Residuals vs", predictor),
    xlab = predictor,
    ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)
}

################### NICHTLINEARE MODELLE ###################

### Modell mit Transformationen und Splines
# Quadratische Terme für `pres` und `tsun`, Spline für `date_numeric`
model_custom <- brm(
  formula = Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
    Temperature.Score + Recovery.Index.Score + Meet.Daily.Targets.Score + 
    s(date_numeric, k = 5) + I(pres^2) + I(tsun^2) + tavg,
  data = data_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 2000,
  warmup = 1000
)

# Modellzusammenfassung
summary(model_custom)

### Modell mit komplexeren Transformationen
# Formel mit Splines und quadratischen Transformationen
model_formula <- bf(
  Respiratory.Rate ~ 
    I(Recovery.Index.Score^2) +               # Quadratischer Term für Recovery Index Score
    s(Temperature.Score, k = 5) +             # Spline für Temperature Score
    I(HRV.Balance.Score^2) +                  # Quadratischer Term für HRV Balance Score
    s(Resting.Heart.Rate.Score, k = 5) +      # Spline für Resting Heart Rate Score
    date_numeric + tsun + tavg + pres         # Weitere Variablen ohne Transformationen
)

# Fitte das Modell
model_nonlinear <- brm(
  formula = model_formula,
  data = data_no_nas_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 2000,
  warmup = 1000
)

# Modellzusammenfassung
summary(model_nonlinear)
