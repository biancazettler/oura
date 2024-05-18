source("pre_processing.R")

# Bayesian models
library(brms)
library(rstan)

# sleep quality and HFV:
# multivariat bayesian model
fit1 <- brm(
  bf(Average.Resting.Heart.Rate ~ Sleep.Score + Total.Sleep.Duration + REM.Sleep.Duration + (1|date)) +
    bf(Average.HRV ~ Sleep.Score + Total.Sleep.Duration + Deep.Sleep.Duration + (1|date)),
  data = data_no_nas_bz,
  family = gaussian(), 
  prior = c(set_prior("normal(0, 5)", class = "b"), set_prior("normal(0, 10)", class = "Intercept")),
  chains = 4, iter = 2000
)

summary(fit1)
