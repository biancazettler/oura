source("pre_processing.R")

# Bayesian models
library(brms)
library(dynaTree)
library(rstan)
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
