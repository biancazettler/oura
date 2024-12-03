# Lade vorbereitende Daten und Modell-Ergebnisse
source("R/Modelle_ZV_Schlafwert")
source("R/Modelle_ZV_Atemfrequenz")

# Lade notwendige Bibliotheken
library(ggplot2)
library(patchwork)
library(gridExtra)
library(bayesplot)

################### MODELLGÜTE ###################

### Posterior Predictive Checks (PPC)
# Schlafwert-Modelle
pp_check(model_bayes, type = "dens_overlay")
pp_check(model_poly, type = "dens_overlay")
pp_check(model_spline, type = "dens_overlay")
pp_check(model_quad_spline, type = "dens_overlay")

# Atemfrequenz-Modelle
pp_check(model_bayes_res, type = "dens_overlay")
pp_check(model_custom, type = "dens_overlay")
pp_check(model_nonlinear, type = "dens_overlay")

### Modellvergleich mit WAIC
# WAIC-Berechnung für Schlafwert-Modelle
waic_poly <- waic(model_poly)
waic_spline <- waic(model_spline)
waic_bayes <- waic(model_bayes)

# Ergebnisse ausgeben
print(waic_poly)
print(waic_spline)
print(waic_bayes)

### Modellvergleich mit LOO-CV
# LOO-Kriterien für Schlafwert-Modelle
model_poly <- add_criterion(model_poly, "loo")
model_spline <- add_criterion(model_spline, "loo")
model_linear <- add_criterion(model_bayes, "loo")
model_tavg <- add_criterion(model_quad_tavg, "loo")
model_tavg_resp <- add_criterion(model_quad_spline, "loo")
model_tsun <- add_criterion(model_tsun_quad, "loo")
model_tsun_tavg_resp <- add_criterion(model_tsun_tavg_resp, "loo")

# LOO-Kriterien für Atemfrequenz-Modelle
model_resp <- add_criterion(model_bayes_res, "loo")
model_nonlin <- add_criterion(model_custom, "loo")
model_nonlinear <- add_criterion(model_nonlinear, "loo")

# Modellvergleich basierend auf LOO
# Schlafwert-Modelle
loo_compare(model_poly, model_spline, model_linear)
loo_compare(model_poly, model_spline, model_linear, model_tavg, model_tavg_resp, model_tsun, model_tsun_tavg_resp)

# Atemfrequenz-Modelle
loo_compare(model_nonlin, model_nonlinear, model_resp)

################### VISUALISIERUNG: POSTERIOR PREDICTIVE PLOTS ###################

# Definiere Farbskala
custom_colors <- c("y" = "black", "yrep" = "blue")

# Posterior Predictive Plots erstellen
pp1 <- pp_check(model_bayes, type = "dens_overlay") +
  ggtitle("Lineares Modell: Schlafwert") +
  xlab("Schlafwert (y)") +
  ylab("Dichte") +
  theme_minimal() +
  scale_color_manual(values = custom_colors, name = NULL) +
  theme(legend.position = "bottom")

pp2 <- pp_check(model_poly, type = "dens_overlay") +
  ggtitle("Polynomiales Modell: Schlafwert") +
  xlab("Schlafwert (y)") +
  ylab("Dichte") +
  theme_minimal() +
  scale_color_manual(values = custom_colors, name = NULL) +
  theme(legend.position = "bottom")

pp3 <- pp_check(model_spline, type = "dens_overlay") +
  ggtitle("Spline-Modell: Schlafwert") +
  xlab("Schlafwert (y)") +
  ylab("Dichte") +
  theme_minimal() +
  scale_color_manual(values = custom_colors, name = NULL) +
  theme(legend.position = "bottom")

# Kombinierte Plots
final_plot <- (pp1 + pp2 + pp3) +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom")

# Speichern der kombinierten Plots
ggsave("ppc_combined_sleep_models.pdf", plot = final_plot, width = 12, height = 5)

# Posterior Predictive Plot für Atemfrequenz
pp_res <- pp_check(model_bayes_res, type = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Atemfrequenz") +
  xlab("Atemfrequenz (y)") +
  ylab("Dichte") +
  theme_minimal() +
  scale_color_manual(values = custom_colors, name = NULL) +
  theme(legend.position = "bottom")

# Speichern als PDF
ggsave("ppc_respiratory_rate.pdf", plot = pp_res, width = 10, height = 6)

################### RESIDUENANALYSE ###################

# Residuen und Fitted Values berechnen
residuals <- residuals(model_bayes, summary = FALSE)
residuals_mean <- apply(residuals, 2, mean)
fitted_values <- fitted(model_bayes)

# Residuenplot für Schlafwert-Modelle
pdf("Residuals_vs_Fitted_Sleep.pdf", width = 12, height = 8)
par(mfrow = c(3, 3))  # 3x3 Subplots

# Plots für ausgewählte Variablen
predictors <- c("Resting.Heart.Rate.Score", "HRV.Balance.Score", "Temperature.Score", 
                "Recovery.Index.Score", "date_numeric", "tsun", "tavg", "pres", 
                "Meet.Daily.Targets.Score")

for (predictor in predictors) {
  plot(
    data_filtered[[predictor]], residuals_mean,
    main = paste("Residuals vs", predictor),
    xlab = predictor,
    ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)
}
dev.off()

# Residuenplots für Atemfrequenz-Modelle
pdf("Residuals_vs_Fitted_Respiratory.pdf", width = 12, height = 8)

# Plots für Atemfrequenz-Modelle
plot(fitted_values, residuals_mean, 
     main = "Residuals vs. Fitted Values: Atemfrequenz",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "black")
abline(h = 0, col = "red", lty = 2)

dev.off()
