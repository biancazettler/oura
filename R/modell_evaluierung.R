source("R/variablenselektion_zv_sleepscore.R")
source("R/modelle_nichtlinear_sv_sleepscore.R")


# modellgüte 

# Posterior Predictive Check
pp_check(model_bayes)
pp_check(model_poly)
pp_check(model_spline)
pp_check(model_quad_spline)
# model linear resp rate:
pp_check(model_bayes_res)
# nicht lineare resp rate modelle:
pp_check(model_custom)
pp_check(model_nonlinear)

# Vergleich der Modelle mit WAIC
waic_poly <- waic(model_poly)
waic_spline <- waic(model_spline)


# WAIC-Ergebnisse vergleichen
print(waic_poly)
print(waic_spline)
waic(model_bayes)

# Optional: LOO-CV für detaillierteren Modellvergleich
# Berechne LOO für das Modell mit polynomialen Termen
model_poly <- add_criterion(model_poly, "loo")

# Berechne LOO für das Modell mit Splines
model_spline <- add_criterion(model_spline, "loo")

# lineares modell
model_linear <- add_criterion(model_bayes, "loo")

# modell quadratischer term tavg
model_tavg <- add_criterion(model_quad_tavg, "loo")

# quad tav und spline respiratory:
model_tavg_resp <- add_criterion(model_quad_spline, "loo")

# quad tsun:
model_tsun <- add_criterion(model_tsun_quad, "loo")

# alle bisherigen terme:
model_tsun_tavg_resp <- add_criterion(model_tsun_tavg_resp, "loo")

# resp rate linear:
model_resp <- add_criterion(model_bayes_res, "loo")

# resp rate nicht lineare:
model_nonlin <- add_criterion(model_custom, "loo")
model_nonlinear <- add_criterion(model_nonlinear, "loo")


# Vergleiche die Modelle basierend auf LOO für sleep score
loo_compare(model_poly, model_spline, model_linear)

loo_compare(model_poly, model_spline, model_linear, model_tavg, model_tavg_resp,
            model_tsun, model_tsun_tavg_resp)

# Vergleiche die Modelle basierend auf LOO für respiratory rate
loo_compare(model_nonlin, model_nonlinear, model_resp)

# darstellungen:
library(ggplot2)
library(patchwork)

# Definiere die Farbskala
custom_colors <- c("y" = "black", "yrep" = "blue")

# Erstelle Posterior Predictive Plots für drei Modelle
pp1 <- pp_check(model_bayes, type = "dens_overlay") +
  ggtitle("Lineares Modell") +
  xlab("Schlafscore (y)") +
  ylab("Dichte") +
  theme_minimal(base_size = 12) +
  scale_color_manual(
    values = custom_colors, 
    labels = c("Beobachtete Werte", "Simulierte Werte"),
    name = NULL # Entfernt "colour" aus der Legende
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

pp2 <- pp_check(model_poly, type = "dens_overlay") +
  ggtitle("Polynomiales Modell") +
  xlab("Schlafscore (y)") +
  ylab("Dichte") +
  theme_minimal(base_size = 12) +
  scale_color_manual(
    values = custom_colors, 
    labels = c("Beobachtete Werte", "Simulierte Werte"),
    name = NULL # Entfernt "colour" aus der Legende
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

pp3 <- pp_check(model_spline, type = "dens_overlay") +
  ggtitle("Spline-Modell") +
  xlab("Schlafscore (y)") +
  ylab("Dichte") +
  theme_minimal(base_size = 12) +
  scale_color_manual(
    values = custom_colors, 
    labels = c("Beobachtete Werte", "Simulierte Werte"),
    name = NULL # Entfernt "colour" aus der Legende
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

# Anordnen der Plots nebeneinander
final_plot <- (pp1 + pp3 + pp2) +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom") # Gemeinsame Legende unten

# Speichern als PDF
ggsave("pp_plots_combined_improved.pdf", plot = final_plot, width = 12, height = 5)

# Speichern des 3-in-1-Plot als PNG
ggsave("ppc_combined.png", plot = final_plot, width = 12, height = 5, dpi = 300)

# Lade ggplot2 und bayesplot, falls noch nicht geladen
library(ggplot2)
library(bayesplot)

# Definiere die Farbskala
custom_colors <- c("y" = "black", "yrep" = "blue")

# Erstelle den Posterior Predictive Plot
pp_res <- pp_check(model_bayes_res, type = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Respiratory Rate") +
  xlab("Atemfrequenz (y)") +
  ylab("Dichte") +
  theme_minimal(base_size = 12) +
  scale_color_manual(
    values = custom_colors, 
    labels = c("Beobachtete Werte", "Simulierte Werte"),
    name = NULL # Entfernt "colour" aus der Legende
  ) +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2, "cm")
  )

# Speichere den Plot als PDF in der gewünschten Größe
pdf("pp_plot_respiratory_rate.pdf", width = 1600 / 150, height = 800 / 150, bg = "white") # Breite und Höhe in Zoll
print(pp_res)
dev.off()


# Berechnung der Residuen und Fitted Values
residuals <- residuals(model_bayes)  # Residuen aus dem Modell
fitted_values <- fitted(model_bayes)  # Fitted Values aus dem Modell

# Plot erstellen
plot(fitted_values, residuals, 
     main = "Residuals vs. Fitted Values Lineares Modell", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20,  # Punktstil für bessere Lesbarkeit
     col = "black")  # Farbe der Punkte
abline(h = 0, col = "blue", lty = 2)  # Horizontale Linie bei 0

# nur für stetige variabeln: 
# Extrahiere die Residuen und bilde den Mittelwert über die Posterior-Samples
residuals_samples <- residuals(model_bayes, summary = FALSE)  # Hole Residuen ohne Zusammenfassung
residuals_mean <- apply(residuals_samples, 2, mean)           # Mittlere Residuen berechnen

# Überprüfe die Länge, um sicherzustellen, dass sie der Anzahl der Beobachtungen entspricht
length(residuals_mean) # Sollte 811 sein

# Filtere die Daten auf die Zeilen ohne fehlende Werte für die Variablen, die im Modell verwendet wurden
data_filtered <- data_imputed_jc_clean[complete.cases(data_imputed_jc_clean[c("Resting.Heart.Rate.Score", 
                                                                              "Respiratory.Rate", 
                                                                              "HRV.Balance.Score", 
                                                                              "Recovery.Index.Score", 
                                                                              "Previous.Day.Activity.Score", 
                                                                              "Temperature.Score", 
                                                                              "tavg", "tsun",
                                                                              "date_numeric")]), ]
# Speicherort und PDF-Datei definieren
pdf("Residuen_vs_angepasste_Werte_Schlafwert.pdf", width = 12, height = 10)

# Layout für 3x3 Grid (9 Plots)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))  # mar legt die Ränder fest

# Plot 1: Residuals vs. Respiratory Rate
plot(data_filtered$Respiratory.Rate, residuals_mean,
     main = "Residuals vs. Respiratory Rate",
     xlab = "Respiratory Rate",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 2: Residuals vs. Tavg
plot(data_filtered$tavg, residuals_mean,
     main = "Residuals vs. Tavg",
     xlab = "Tavg",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 3: Residuals vs. Tsun
plot(data_filtered$tsun, residuals_mean,
     main = "Residuals vs. Tsun",
     xlab = "Tsun",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 4: Residuals vs. Resting.Heart.Rate.Score
plot(data_filtered$Resting.Heart.Rate.Score, residuals_mean,
     main = "Residuals vs. Resting.Heart.Rate.Score",
     xlab = "Resting.Heart.Rate.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 5: Residuals vs. HRV.Balance.Score
plot(data_filtered$HRV.Balance.Score, residuals_mean,
     main = "Residuals vs. HRV.Balance.Score",
     xlab = "HRV.Balance.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 6: Residuals vs. Recovery.Index.Score
plot(data_filtered$Recovery.Index.Score, residuals_mean,
     main = "Residuals vs. Recovery.Index.Score",
     xlab = "Recovery.Index.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 7: Residuals vs. Previous.Day.Activity.Score
plot(data_filtered$Previous.Day.Activity.Score, residuals_mean,
     main = "Residuals vs. Previous.Day.Activity.Score",
     xlab = "Previous.Day.Activity.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 8: Residuals vs. Meet.Daily.Targets.Score
plot(data_filtered$Temperature.Score, residuals_mean,
     main = "Residuals vs. Temperature.Score",
     xlab = "Temperature.Score",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 9: Residuals vs. date_numeric
plot(data_filtered$date_numeric, residuals_mean,
     main = "Residuals vs. date_numeric",
     xlab = "date_numeric",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Beende die PDF-Ausgabe
dev.off()


# nichtlineare modelle mit Zielvariable Schlafwert:

# Berechnung der Residuen und Fitted Values für das Spline-Modell
residuals_spline <- residuals(model_spline)  # Residuen aus dem Modell
fitted_values_spline <- fitted(model_spline)  # Fitted Values aus dem Modell

# Plot erstellen
plot(fitted_values_spline, residuals_spline, 
     main = "Residuals vs. Fitted Values Spline Modell", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20,  # Punktstil für bessere Lesbarkeit
     col = "black")  # Farbe der Punkte
abline(h = 0, col = "blue", lty = 2)  # Horizontale Linie bei 0

# Berechnung der Residuen und Fitted Values
residuals_poly <- residuals(model_poly)  # Residuen aus dem Modell
fitted_values_poly <- fitted(model_poly)  # Fitted Values aus dem Modell

# Plot erstellen
plot(fitted_values_poly, residuals_poly, 
     main = "Residuals vs. Fitted Values Poly Modell", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 20,  # Punktstil für bessere Lesbarkeit
     col = "black")  # Farbe der Punkte
abline(h = 0, col = "blue", lty = 2)  # Horizontale Linie bei 0

# speichern der Plots:
# Bibliothek laden
library(gridExtra)

pdf("Residuals_vs_Fitted_All_Models.pdf", width = 14, height = 6)

# Layout für drei nebeneinander liegende Plots
par(mfrow = c(1, 3), mar = c(4, 4, 4, 1))  # mar passt die Ränder an

# Lineares Modell
plot(fitted_values, residuals,
     main = "Residuals vs. Angepasste Werte: Lineares Modell",
     xlab = "Angepasste Werte (Fitted Values)",
     ylab = "Residuen",
     pch = 20,
     col = "black",
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1)
abline(h = 0, col = "blue", lty = 2)

# Spline-Modell
plot(fitted_values_spline, residuals_spline,
     main = "Residuals vs. Angepasste Werte: Spline Modell",
     xlab = "Angepasste Werte (Fitted Values)",
     ylab = "Residuen",
     pch = 20,
     col = "black",
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1)
abline(h = 0, col = "blue", lty = 2)

# Polynomiales Modell
plot(fitted_values_poly, residuals_poly,
     main = "Residuals vs. Angepasste Werte: Polynomiales Modell",
     xlab = "Angepasste Werte (Fitted Values)",
     ylab = "Residuen",
     pch = 20,
     col = "black",
     cex.main = 1.5,
     cex.lab = 1.2,
     cex.axis = 1.1)
abline(h = 0, col = "blue", lty = 2)

dev.off()
