source("R/variablenselektion_zv_sleepscore.R")
source("R/modelle_nichtlinear_zv_sleepscore.R")


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
final_plot <- (pp1 + pp2 + pp3) +
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom") # Gemeinsame Legende unten

# Speichern als PDF
ggsave("pp_plots_combined_improved.pdf", plot = final_plot, width = 12, height = 5)

# Speichern des 3-in-1-Plot als PNG
ggsave("ppc_combined.png", plot = final_plot, width = 12, height = 5, dpi = 300)

# respiratory rate: 
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

# Speichere den Plot als PNG mit weißem Hintergrund
png("pp_plot_respiratory_rate.png", width = 1600, height = 800, res = 150, bg = "white")
print(pp_res)
dev.off()
