# Lade vorbereitende Daten und Bibliotheken
source("Variablenselektion_ZV_Schlafwert")

# Notwendige Pakete laden
install.packages("brms")
install.packages("tidyverse")  # Für Datenmanipulation und ggplot2
library(brms)
library(tidyverse)


### Modelle mit Transformationen für tavg
# Quadratischer Term für tavg
model_quad_tavg <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2),
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_quad_tavg)

# Quadratischer Term für tavg und Spline für Respiratory Rate
model_quad_spline <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2),
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_quad_spline)

### Transformationen für tsun
# Quadratischer Term für tsun
model_tsun_quad <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate + HRV.Balance.Score +
    Recovery.Index.Score + Previous.Day.Activity.Score + Meet.Daily.Targets.Score +
    date_numeric + tavg + I(tavg^2) + I(tsun^2),
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_tsun_quad) # schlechtes Modell, Rhat >>1

# VORSICHT: dauert sehr lange beim ausführen!
# Kombination aller Transformationen und Interaktionen
model_tsun_tavg_resp <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + s(Respiratory.Rate, k = 5) +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + date_numeric + tavg + I(tavg^2) + I(tsun^2),
  data = data_clean,
  family = gaussian(),
  prior = priors,
  control = list(adapt_delta = 0.95),
  cores = 4
)

summary(model_tsun_tavg_resp) # schlechtes Modell, Rhat >>1

### Modelle für das Datum
# Quadratischer Term für date_numeric
model_date_quad <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + tavg + tsun + date_numeric + I(date_numeric^2),
  data = data_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_date_quad) # schlechtes Modell, Rhat >>1

# Spline für date_numeric
model_date_spline <- brm(
  formula = Sleep.Score ~ Resting.Heart.Rate.Score + Respiratory.Rate +
    HRV.Balance.Score + Recovery.Index.Score + Previous.Day.Activity.Score +
    Meet.Daily.Targets.Score + tavg + tsun + s(date_numeric, k = 5),
  data = data_clean,
  family = gaussian(),
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(model_date_spline) 

################## ROBUSTE MODELLE ###################

### Gamma-Modell
# Bayesianisches Gamma-Modell mit log-Link
gamma_model <- brm(
  Sleep.Score ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = data_imputed,                    # Datensatz
  family = Gamma(link = "log"),           # Gamma-Verteilung
  iter = 4000,                            # Anzahl der Iterationen
  warmup = 2000,                          # Aufwärmphase
  chains = 4,                             # Anzahl der Chains
  control = list(max_treedepth = 15)      # Maximale Baumtiefe
)

# Zusammenfassung des Modells
summary(gamma_model)

### Beta-Modell
# Normalisierung der Schlaf-Scores
data_imputed$Normalized.Sleep.Score <- data_imputed$Sleep.Score / 100

# Normalisierte Scores prüfen
shapiro_test_beta <- shapiro.test(data_imputed$Normalized.Sleep.Score)
print(shapiro_test_beta)

# Histogramm der normalisierten Scores
ggplot(data_imputed, aes(x = Normalized.Sleep.Score)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Histogramm der normalisierten Schlaf-Scores")

# Bayesianisches Beta-Modell
beta_model <- brm(
  Normalized.Sleep.Score ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = data_imputed,
  family = Beta(link = "logit"),          # Beta-Verteilung
  iter = 4000,
  warmup = 2000,
  chains = 4
)

# Zusammenfassung und Visualisierung
summary(beta_model)
plot(beta_model)

################### ORDINALE UND MULTINOMIALE MODELLE ###################

### Ordinales Modell
# Schlaf-Scores in Kategorien einteilen
data_imputed$Score_Category <- cut(
  data_imputed$Sleep.Score,
  breaks = c(-Inf, 50, 75, 100),
  labels = c("low", "medium", "high")
)

# Umwandeln in geordneten Faktor
data_imputed$Score_Category <- factor(
  data_imputed$Score_Category,
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Bayesianisches ordinales Modell
ordinal_model <- brm(
  Score_Category ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = data_imputed,
  family = cumulative(link = "logit"),
  iter = 4000,
  warmup = 2000,
  chains = 4
)

# Zusammenfassung des Modells
summary(ordinal_model)

### Multinomiales Modell
# Bayesianisches multinomiales Modell
multinomial_model <- brm(
  Score_Category ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = data_imputed,
  family = categorical(),
  iter = 4000,
  warmup = 2000,
  chains = 4
)

# Zusammenfassung des Modells
summary(multinomial_model)

################### LINEARE MODELLE ###################

### Bayesianisches lineares Regressionsmodell
# Modell mit hoch korrelierten Prädiktoren
model <- brm(
  Sleep.Score ~ Average.Resting.Heart.Rate + Lowest.Resting.Heart.Rate + Average.HRV,
  data = data_imputed,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 10)", class = "b"),         # Priors für Regressionskoeffizienten
    set_prior("normal(0, 10)", class = "Intercept")  # Prior für den Intercept
  ),
  iter = 2000,
  chains = 4,
  warmup = 500,
  cores = 4
)

# Zusammenfassung des Modells
summary(model)

### Einfaches Modell nach Variablenselektion
formula <- bf(Sleep.Score ~ Average.HRV + Average.Resting.Heart.Rate + date)

# Priors definieren
priors <- c(
  set_prior("normal(0, 10)", class = "b"),
  set_prior("normal(0, 10)", class = "Intercept")
)

# Bayesianisches Modell mit selektierten Prädiktoren
model_heart_rate <- brm(
  formula = formula, 
  data = data_imputed,
  prior = priors,
  family = gaussian(),
  iter = 2000,
  chains = 4,
  warmup = 500,
  cores = 4
)

# Zusammenfassung und Posterior-Dichteplots
summary(model_heart_rate)
plot(model_heart_rate)

################### WEITERE MODELLE ###################

### Wochenend-Effekt
# Wochenendspalte erstellen
data_imputed$date <- as.Date(data_imputed$date)
data_imputed$is_weekend <- ifelse(
  weekdays(data_imputed$date) %in% c("Samstag", "Sonntag"), 1, 0
)

# Modell mit Wochenend-Effekt
model_sleep_weekend <- brm(
  sqrt_Sleep_Score ~ Average.HRV + Average.Resting.Heart.Rate + is_weekend,
  data = data_imputed,
  family = gaussian(),
  prior = c(set_prior("normal(0, 10)", class = "b"))
)

# Zusammenfassung des Modells
summary(model_sleep_weekend)
