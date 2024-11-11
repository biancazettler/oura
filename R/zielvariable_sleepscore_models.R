source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

install.packages("brms")
install.packages("tidyverse")  # für Datenmanipulation und ggplot2
library(brms)
library(tidyverse)

###### TO DO: modelle mit daten ohne nas und daten mit imputation der nas vergleichen

############## models with scores data ##########################################
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

# ergebnis: nur resting heart rate score zeigt signifikanten (positiven) einfluss
# auf sleep score

multinomial_model <- brm(
  Score_Category ~ Activity.Score + HRV.Balance.Score + Temperature.Score + Resting.Heart.Rate.Score,
  data = main_scores,
  family = categorical(),
  iter = 4000,
  warmup = 2000,
  chains = 4
)

summary(multinomial_model)

################## models with sleep score and highly correlated predictors #################

# Bayesianisches lineares Regressionsmodell erstellen
model <- brm(
  formula = Sleep.Score ~ Average.Resting.Heart.Rate + Lowest.Resting.Heart.Rate + Average.HRV,
  data = data_imputed_jc,
  family = gaussian(),  # Annahme normalverteilter Residuen
  prior = c(set_prior("normal(0, 10)", class = "b"),  # Vage Priors für die Regressionskoeffizienten
            set_prior("normal(0, 10)", class = "Intercept")),
  iter = 2000,          # Anzahl der Iterationen (je mehr, desto stabiler die Schätzung)
  chains = 4,           # Anzahl der MCMC-Ketten
  warmup = 500,         # Aufwärmphase für das Modell
  cores = 4             # Anzahl der CPU-Kerne für paralleles Sampling
)

# Modellzusammenfassung anzeigen
summary(model)

# nach Korrelationsanalyse und Überprüfung der Modellannahmen: einfaches Modell
# mit Average Resting Heart Rate, datum und Average HRV

# Modellformel definieren
formula <- bf(Sleep.Score ~ Average.HRV + Average.Resting.Heart.Rate + date)
# Priorverteilungen festlegen
priors <- c(
  set_prior("normal(0, 10)", class = "b"),         # Prior für die Regressionskoeffizienten
  set_prior("normal(0, 10)", class = "Intercept")  # Prior für den Intercept
)
# Bayesianisches Modell anpassen
model_heart_rate <- brm(
  formula = formula, 
  data = data_imputed_jc,          # Deine Daten
  prior = priors,                  # Definierte Priorverteilungen
  family = gaussian(),             # Für kontinuierliche Daten (normalverteilte Residuen)
  iter = 2000,                     # Anzahl der Iterationen
  chains = 4,                      # Anzahl der MCMC-Ketten
  warmup = 500,                    # Anzahl der Warmup-Iterationen
  cores = 4                        # Anzahl der verwendeten Prozessorkerne (zur Beschleunigung)
)
summary(model_heart_rate)
plot(model_heart_rate)

# Posterior-Dichteplots anzeigen
plot(model_heart_rate, ask = FALSE)

# Posterior-Proben für bestimmte Koeffizienten extrahieren
posterior_samples <- posterior_samples(model_heart_rate, pars = c("b_Average.HRV", "b_Average.Resting.Heart.Rate"))
head(posterior_samples)

# wochenend effekt:
# Datumsspalte in ein Date-Format umwandeln
data_imputed_jc$date <- as.Date(data_imputed_jc$date)

# Erstellen einer neuen Spalte, die angibt, ob es sich um einen Wochenendtag handelt
data_imputed_jc$is_weekend <- ifelse(weekdays(data_imputed_jc$date) %in% c("Samstag", "Sonntag"), 1, 0)

# Das Modell mit dem Wochenend-Effekt
model_sleep_weekend <- brm(
  sqrt_Sleep_Score ~ Average.HRV + Average.Resting.Heart.Rate + is_weekend,
  data = data_imputed_jc,
  family = gaussian(),
  prior = c(set_prior("normal(0, 10)", class = "b"))
)

# Modell zusammenfassen
summary(model_sleep_weekend)

# dasselbe für sleep.score untransformiert:
# wochenend effekt:
# Datumsspalte in ein Date-Format umwandeln
data_imputed_jc$date <- as.Date(data_imputed_jc$date)

# Erstellen einer neuen Spalte, die angibt, ob es sich um einen Wochenendtag handelt
data_imputed_jc$is_weekend <- ifelse(weekdays(data_imputed_jc$date) %in% c("Samstag", "Sonntag"), 1, 0)

# Das Modell mit dem Wochenend-Effekt
model_sleep_score_weekend <- brm(
  Sleep.Score ~ Average.HRV + Average.Resting.Heart.Rate + is_weekend,
  data = data_imputed_jc,
  family = gaussian(),
  prior = c(set_prior("normal(0, 10)", class = "b"))
)

# Modell zusammenfassen
summary(model_sleep_score_weekend)

# externe Variable Mondphasen mit ins Modell:
library(lunar)

# Datum in das richtige Format bringen, falls noch nicht geschehen
data_imputed_jc$date <- as.Date(data_imputed_jc$date)

# Mondphasen für jedes Datum berechnen
data_imputed_jc$moon_phase <- lunar.phase(data_imputed_jc$date, name = TRUE)

# Mondphasen als Faktor umwandeln, um sie im Modell zu verwenden
data_imputed_jc$moon_phase <- factor(data_imputed_jc$moon_phase)

model_sleep_moon <- brm(
  Sleep.Score ~ Average.HRV + Average.Resting.Heart.Rate + is_weekend + moon_phase,
  data = data_imputed_jc,
  family = gaussian(),
  prior = c(set_prior("normal(0, 10)", class = "b"))
)

# Modell zusammenfassen
summary(model_sleep_moon)


