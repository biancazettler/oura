source("R/variablenselektion_zv_sleepscore")

# nicht lineare modelle: zv sleep score

# Modell mit einem quadratischen Term für Respiratory Rate
formula_poly <- bf(Sleep.Score ~ Resting.Heart.Rate.Score + poly(Respiratory.Rate, 2) +
                     HRV.Balance.Score + Recovery.Index.Score +
                     Previous.Day.Activity.Score + Meet.Daily.Targets.Score + 
                     tavg + date_numeric)

# Bayesianisches Modell fitten
model_poly <- brm(formula_poly,
                  data = data_no_nas_jc_clean,
                  family = gaussian(),
                  prior = priors,  # Setze deine bevorzugten Priors hier ein
                  control = list(adapt_delta = 0.99),
                  cores = 4)
summary(model_poly)

# Modell mit einem Spline-Term für Respiratory Rate
formula_spline <- bf(Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) +
                       HRV.Balance.Score + Recovery.Index.Score +
                       Previous.Day.Activity.Score + Meet.Daily.Targets.Score + 
                       tavg + date_numeric)

# Bayesianisches Modell fitten
model_spline <- brm(formula_spline,
                    data = data_no_nas_jc_clean,
                    family = gaussian(),
                    prior = priors,  # Setze deine bevorzugten Priors hier ein
                    control = list(adapt_delta = 0.99),
                    cores = 4)
summary(model_spline)

# modell mit quadratischem term für tavg (siehe deskriptive analyse)
# Lade die brms Bibliothek, falls noch nicht geladen
library(brms)

# Erstelle das Modell mit dem quadratischen Term für tavg
model_quad_tavg <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate + 
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score + 
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2),
  data = data_no_nas_jc_clean,
  family = gaussian(),
  prior = priors,  # Falls du spezielle Priors gesetzt hast
  control = list(adapt_delta = 0.99),  # Anpassungen für die Konvergenz
  cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_quad_tavg)

# Modell mit quadratischem Term für tavg und Spline für Respiratory Rate
model_quad_spline <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) + 
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score + 
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2),
  data = data_no_nas_jc_clean,
  family = gaussian(),
  prior = priors, # ggf. definierte Priors
  control = list(adapt_delta = 0.99),
  cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_quad_spline)

# Quadratischer Term für tsun
model_tsun_quad <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate + HRV.Balance.Score +
    Recovery.Index.Score + Previous.Day.Activity.Score + Meet.Daily.Targets.Score +
    date_numeric + tavg + I(tavg^2) + I(tsun^2),
  data = data_no_nas_jc_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_tsun_quad)

# Alle Transformationen und Interaktionen
model_tsun_tavg_resp <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2) + I(tsun^2),
  data = data_no_nas_jc_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_tsun_tavg_resp)

# datum quadratisch:
model_date_quad <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + 
    Respiratory.Rate + 
    HRV.Balance.Score + 
    Recovery.Index.Score + 
    Previous.Day.Activity.Score + 
    Meet.Daily.Targets.Score + 
    tavg + 
    tsun + 
    date_numeric + 
    I(date_numeric^2),  # Quadratischer Term für Datum
  data = data_no_nas_jc_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_date_quad)

# datum spline:
model_date_spline <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + 
    Respiratory.Rate + 
    HRV.Balance.Score + 
    Recovery.Index.Score + 
    Previous.Day.Activity.Score + 
    Meet.Daily.Targets.Score + 
    tavg + 
    tsun + 
    s(date_numeric, k = 5),  # Spline für Datum
  data = data_no_nas_jc_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_date_spline)
