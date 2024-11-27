source("R/variablenselektion_zv_respiratoryrate_newest.R")

# nicht lineare modelle: zv respiratory rate 

library(brms)

# Modell mit pres und tsun als quadratische Terme, date als Spline und restlichen Variablen ohne Interaktionen
model_custom <- brm(
  formula = Respiratory.Rate ~ Resting.Heart.Rate.Score + HRV.Balance.Score + 
    Temperature.Score + Recovery.Index.Score + Meet.Daily.Targets.Score + 
    s(date_numeric, k = 5) + I(pres^2) + I(tsun^2) + tavg,
  data = data_no_nas_jc_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  chains = 4,
  iter = 2000,
  warmup = 1000
)

# Modellzusammenfassung anzeigen
summary(model_custom)

# Modellformel mit den Transformationen
model_formula <- bf(
  Respiratory.Rate ~ 
    I(Recovery.Index.Score^2) +               # Quadratischer Term für Recovery Index Score
    s(Temperature.Score, k = 5) +             # Spline für Temperature Score (alternativ log(Temperature.Score))
    I(HRV.Balance.Score^2) +                  # Quadratischer Term für HRV Balance Score
    s(Resting.Heart.Rate.Score, k = 5) +      # Spline für Resting Heart Rate Score (alternativ sqrt(Resting.Heart.Rate.Score))
    date_numeric + tsun + tavg + pres         # Weitere ausgewählte Variablen ohne Transformationen
)

# Erstellen des Modells
model_nonlinear <- brm(
  formula = model_formula,
  data = data_no_nas_jc_clean,                # Ihre Daten
  family = gaussian(),                        # Gaussian family für ein lineares Modell
  chains = 4,# Anzahl der MCMC-Ketten
  control = list(adapt_delta = 0.99), 
  iter = 2000,                                # Anzahl der Iterationen
  warmup = 1000                               # Anzahl der Warm-up-Iterationen
)

# Zusammenfassung des Modells
summary(model_nonlinear)
