# Lade vorbereitende Daten und Abhängigkeiten
source("R/Variablenselektion_ZV_Schlafwert.R")

# Lade notwendige Bibliothek
library(brms)

################### MODELLE MIT ZIELVARIABLER SCHLAFWERT ###################

### Bayesianische lineare Regression
# Einfaches Modell ohne Interaktionen
model_bayes <- brm(
  formula = Sleep.Score ~ date_numeric + Resting.Heart.Rate.Score + Respiratory.Rate +
    HRV.Balance.Score + Recovery.Index.Score + Temperature.Score + 
    Previous.Day.Activity.Score + Meet.Daily.Targets.Score + tavg + tsun, 
  data = data_clean,
  family = gaussian()
)

# Zusammenfassung des Modells anzeigen
summary(model_bayes)

### Modelle mit Interaktionstermen
# Formel mit Interaktionen und Sparsamem Prior
formula_interaction <- bf(
  Sleep.Score ~ (Resting.Heart.Rate.Score + Respiratory.Rate + HRV.Balance.Score + Recovery.Index.Score) * 
    (Previous.Day.Activity.Score + Meet.Daily.Targets.Score) + (tsun * tavg)
)
priors_interaction <- prior(horseshoe(scale_global = 1), class = "b")

# VORSICHT: dauert sehr lange beim ausführen!
# Bayesianisches Modell mit Interaktionen
model_bayes_interaction <- brm(
  formula = formula_interaction,
  data = data_clean,
  family = gaussian(),
  prior = priors_interaction,
  control = list(adapt_delta = 0.99),
  cores = 4
)

# Zusammenfassung des Interaktionsmodells
summary(model_bayes_interaction)

### Modelle mit nichtlinearen Transformationen
# Priors für Modelle
priors <- prior(normal(0, 1), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

# Quadratischer Term für Respiratory Rate
formula_poly <- bf(
  Sleep.Score ~ Resting.Heart.Rate.Score + poly(Respiratory.Rate, 2) +
    HRV.Balance.Score + Recovery.Index.Score +
    Previous.Day.Activity.Score + tavg + date_numeric + tsun + Temperature.Score
)
model_poly <- brm(
  formula = formula_poly,
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_poly)

# Spline für Respiratory Rate
formula_spline <- bf(
  Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) +
    HRV.Balance.Score + Recovery.Index.Score +
    Previous.Day.Activity.Score + tavg + date_numeric + tsun + Temperature.Score
)
model_spline <- brm(
  formula = formula_spline,
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_spline)
