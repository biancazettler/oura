install.packages("brms")
install.packages("tidyverse")  # für Datenmanipulation und ggplot2
library(brms)
library(tidyverse)

# Modellformel
formula <- Sleep.Score ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score

# Fit des Modells
model_main_scores<- brm(formula, data = main_scores, 
                               family = gaussian(),  # Normalverteilung für Sleep Score
                               prior = c(
                                 set_prior("normal(0, 5)", class = "b"),  # Prior für die Regressionskoeffizienten
                                 set_prior("normal(0, 5)", class = "Intercept"),  # Prior für den Intercept
                                 set_prior("cauchy(0, 2)", class = "sigma")  # Prior für die Standardabweichung des Fehlerterms
                               ),
                               iter = 2000,   # Anzahl der Iterationen
                               chains = 4,    # Anzahl der Chains
                               cores = 4      # Anzahl der Kerne für paralleles Rechnen
)


summary(model_main_scores)  # Zusammenfassung der Modellanpassung
plot(model_main_scores)     # Visualisierung der Posteriorverteilungen
# -> signifikanter positiver einfluss von resting heart rate score auf sleep score

############## PROBLEM: sleep score ist nicht normalverteilt! siehe pre_processing
# -> robusteres gamma modell :

# Gamma-Modell mit bayesianischer Modellierung
library(brms)

gamma_model <- brm(
  Sleep.Score ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = main_scores,
  family = Gamma(link = "log"),  # Gamma-Verteilung mit log-Link
  iter = 4000,                    # Anzahl der Iterationen
  warmup = 2000,                  # Aufwärmphase
  chains = 4,                    # Anzahl der Chains
  control = list(max_treedepth = 15)
  )

# Zusammenfassung des Modells
summary(gamma_model)

# problem: keinen signifikanten einfluss

# Beta verteilung:

main_scores$Normalized.Sleep.Score <- main_scores$Sleep.Score / 100

# Normalisierte Scores prüfen
shapiro_test_beta <- shapiro.test(main_scores$Normalized.Sleep.Score)
print(shapiro_test_beta)

# Histogramm der normalisierten Scores
ggplot(main_scores, aes(x = Normalized.Sleep.Score)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Histogramm der normalisierten Sleep Scores")

beta_model <- brm(
  Normalized.Sleep.Score ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = main_scores,
  family = Beta(link = "logit"),
  iter = 4000,
  warmup = 2000,
  chains = 4
)

summary(beta_model)  # Zusammenfassung der Modellanpassung
plot(beta_model)     # Visualisierung der Posteriorverteilungen

# problem: auch keine ergebnisse

# Schlaf Scores in Kategorien unterteilen (z.B. niedrig, mittel, hoch)
main_scores$Score_Category <- cut(main_scores$Sleep.Score,
                                  breaks = c(-Inf, 50, 75, 100),
                                  labels = c("low", "medium", "high"))
# Umwandeln in einen geordneten Faktor
main_scores$Score_Category <- factor(main_scores$Score_Category,
                                     levels = c("low", "medium", "high"),
                                     ordered = TRUE)
# Ordinales Modell
ordinal_model <- brm(
  Score_Category ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = main_scores,
  family = cumulative(link = "logit"),
  iter = 4000,
  warmup = 2000,
  chains = 4
)

summary(ordinal_model)
