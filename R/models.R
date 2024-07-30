source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")


# Bayesian models
library(brms)
library(dynaTree)
library(rstan)
library(caret)
library(rstanarm)

# sleep quality and HFV:
# multivariat bayesian model
fit1_bz <- brm(
  bf(Average.Resting.Heart.Rate ~ Sleep.Score + Total.Sleep.Duration + REM.Sleep.Duration + (1|date)) +
    bf(Average.HRV ~ Sleep.Score + Total.Sleep.Duration + Deep.Sleep.Duration + (1|date)),
  data = data_no_nas_bz,
  family = gaussian(), 
  prior = c(set_prior("normal(0, 5)", class = "b"), set_prior("normal(0, 10)", class = "Intercept")),
  chains = 4, iter = 2000
)

summary(fit1_bz)

fit1_jc <- brm(
  bf(Average.Resting.Heart.Rate ~ Sleep.Score + Total.Sleep.Duration + REM.Sleep.Duration + (1|date)) +
    bf(Average.HRV ~ Sleep.Score + Total.Sleep.Duration + Deep.Sleep.Duration + (1|date)),
  data = data_no_nas_jc,
  family = gaussian(), 
  prior = c(set_prior("normal(0, 5)", class = "b"), set_prior("normal(0, 10)", class = "Intercept")),
  chains = 4, iter = 2000
)

summary(fit1_jc)

# Bayesian linear model 
model2_jc <- stan_glm(Average.Resting.Heart.Rate ~ Total.Sleep.Duration, 
                  data = data_no_nas_jc, 
                  family = gaussian(), # normal distribution of residuals 
                  prior = normal(0, 10), # normal distribution of priors 
                  chains = 4, 
                  iter = 2000)

print(summary(model2_jc))
plot(model2_jc) # no effect od sleep duration on average resting heart rate

model2_bz <- stan_glm(Average.Resting.Heart.Rate ~ Total.Sleep.Duration, 
                      data = data_no_nas_bz, 
                      family = gaussian(), # normal distribution of residuals 
                      prior = normal(0, 10), # normal distribution of priors 
                      chains = 4, 
                      iter = 2000)

print(summary(model2_bz))
plot(model2_bz) # no effect of sleep duration on average resting heart rate

model_activity_jc <- stan_glm(Activity.Score ~ Sleep.Score + Readiness.Score + HRV.Balance.Score,
                           data = data_no_nas_jc,
                           family = gaussian(),
                           prior = normal(0, 10),
                           chains = 4,
                           iter = 2000)

print(summary(model_activity_jc))
plot(model_activity_jc)

model_activity_bz <- stan_glm(Activity.Score ~ Sleep.Score + Readiness.Score + HRV.Balance.Score,
                              data = data_no_nas_bz,
                              family = gaussian(),
                              prior = normal(0, 10),
                              chains = 4,
                              iter = 2000)

print(summary(model_activity_bz))
plot(model_activity_bz)

# Modell für die Vorhersage des Sleep Scores basierend auf Activity Score
model_sleep_jc <- stan_glm(Sleep.Score ~ Activity.Score + Total.Bedtime + Average.Resting.Heart.Rate,
                  data = data_no_nas_jc,
                  family = gaussian(),
                  prior = normal(0, 2, autoscale = TRUE),  # Priors leicht restriktiv gewählt
                  chains = 4,
                  iter = 2000)
print(summary(model_sleep_jc))
plot(model_sleep_jc)

model_sleep_bz <- stan_glm(Sleep.Score ~ Activity.Score + Total.Bedtime + Average.Resting.Heart.Rate,
                        data = data_no_nas_bz,
                        family = gaussian(),
                        prior = normal(0, 2, autoscale = TRUE),  # Priors leicht restriktiv gewählt
                        chains = 4,
                        iter = 2000)
print(summary(model_sleep_bz))
plot(model_sleep_bz)

# Build a Bayesian linear regression model
# Model: Sleep Score predicted by sleep variables
bayesian_model_sleep_bz <- brm(
  formula = Sleep.Score ~ Total.Sleep.Score + Average.Resting.Heart.Rate 
  + REM.Sleep.Score + Deep.Sleep.Score + Sleep.Efficiency.Score + Restfulness.Score + Sleep.Latency.Score 
  + Sleep.Timin.Score + Total.Sleep.Duration + Total.Bedtime + Awake.Time + REM.Sleep.Duration + Light.Sleep.Duration 
  + Sleep.Efficiency + Sleep.Latency + Sleep.Timing, 
  data = data_no_nas_bz, 
  family = gaussian(),  # Assuming Sleep Score is a continuous variable
  prior = c(
    set_prior("normal(0, 5)", class = "b"),  # Priors for the regression coefficients
    set_prior("normal(0, 10)", class = "Intercept"),  # Prior for the intercept
    set_prior("exponential(1)", class = "sigma")  # Prior for the error term
  ),
  chains = 4,  # Number of Markov Chain Monte Carlo chains
  iter = 4000,  # Number of iterations per chain
  warmup = 1000,  # Number of warmup iterations
  control = list(adapt_delta = 0.95)  # Increasing adapt_delta to handle potential convergence issues
)

# Print the summary of the model
summary(bayesian_model_sleep_bz)

# Check diagnostics using built-in plot functions
plot(bayesian_model_sleep_bz)

# check model fit
pp_check(bayesian_model_sleep_bz)

# other data:
bayesian_model_sleep_jc <- brm(
  formula = Sleep.Score ~ Total.Sleep.Score + Average.Resting.Heart.Rate 
  + REM.Sleep.Score + Deep.Sleep.Score + Sleep.Efficiency.Score + Restfulness.Score + Sleep.Latency.Score 
  + Sleep.Timin.Score + Total.Sleep.Duration + Total.Bedtime + Awake.Time + REM.Sleep.Duration + Light.Sleep.Duration 
  + Sleep.Efficiency + Sleep.Latency + Sleep.Timing, 
  data = data_no_nas_jc, 
  family = gaussian(),  # Assuming Sleep Score is a continuous variable
  prior = c(
    set_prior("normal(0, 5)", class = "b"),  # Priors for the regression coefficients
    set_prior("normal(0, 10)", class = "Intercept"),  # Prior for the intercept
    set_prior("exponential(1)", class = "sigma")  # Prior for the error term
  ),
  chains = 4,  # Number of Markov Chain Monte Carlo chains
  iter = 4000,  # Number of iterations per chain
  warmup = 1000,  # Number of warmup iterations
  control = list(adapt_delta = 0.95)  # Increasing adapt_delta to handle potential convergence issues
)

# Print the summary of the model
summary(bayesian_model_sleep_jc)

# Check diagnostics using built-in plot functions
plot(bayesian_model_sleep_jc)

# check model fit
pp_check(bayesian_model_sleep_jc)

# another model
model2_sleep_jc <- brm(
  formula = Sleep.Score ~ Activity.Score + Average.HRV + Total.Burn + Temperature.Deviation...C.,
  data = data_no_nas_jc,  # Setze voraus, dass data_no_nas_bz bereits vorbereitet ist
  family = gaussian(),    # Normalverteilung der Residuen
  prior = c(
    set_prior("normal(0, 5)", class = "b"),  # Priors für die Steigungen
    set_prior("normal(50, 10)", class = "Intercept")  # Prior für den Achsenabschnitt
  ),
  chains = 4,
  iter = 6000
)

summary(model2_sleep_jc)

# another model
model2_sleep_bz <- brm(
  formula = Sleep.Score ~ Activity.Score + Average.HRV + Total.Burn + Temperature.Deviation...C.,
  data = data_no_nas_bz,  # Setze voraus, dass data_no_nas_bz bereits vorbereitet ist
  family = gaussian(),    # Normalverteilung der Residuen
  prior = c(
    set_prior("normal(0, 5)", class = "b"),  # Priors für die Steigungen
    set_prior("normal(50, 10)", class = "Intercept")  # Prior für den Achsenabschnitt
  ),
  chains = 4,
  iter = 6000
)

summary(model2_sleep_bz)

# Lineares Modell
linear_model_jc <- lm(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score, data = data_no_nas_jc)
summary(linear_model_jc)

bayesian_model_jc <- brm(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score,
                      data = data_no_nas_jc, family = gaussian(),
                      prior = c(set_prior("normal(0,5)", class = "b"),
                                set_prior("cauchy(0,2)", class = "sigma")),
                      chains = 4, iter = 2000, warmup = 1000)
summary(bayesian_model_jc)

linear_model_bz <- lm(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score, data = data_no_nas_bz)
summary(linear_model_bz)

bayesian_model_bz <- brm(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score,
                         data = data_no_nas_bz, family = gaussian(),
                         prior = c(set_prior("normal(0,5)", class = "b"),
                                   set_prior("cauchy(0,2)", class = "sigma")),
                         chains = 4, iter = 2000, warmup = 1000)
summary(bayesian_model_bz)

# Kreuzvalidierung
nas_removed_jc <- na.omit(data_no_nas_jc) 
train_control <- trainControl(method = "cv", number = 10)
model_cv_jc <- train(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score,
                  data = nas_removed_jc, method = "lm",
                  trControl = train_control)
print(model_cv_jc)

nas_removed_bz <- na.omit(data_no_nas_bz) 
train_control <- trainControl(method = "cv", number = 10)
model_cv_bz <- train(Sleep.Score ~ Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score,
                     data = nas_removed_bz, method = "lm",
                     trControl = train_control)
print(model_cv_bz)

# sleep score and activity score 

sleep_activity <- brm(Sleep.Score ~ Activity.Score,
                      data = data_no_nas_jc, 
                      family = gaussian(),
                      prior = c(set_prior("normal(0,10)", class = "b"),
                                set_prior("exponential(1)", class = "sigma")),
                      chains = 4, iter = 2000, warmup = 1000, 
                      control = list(adapt_delta = 0.95))
