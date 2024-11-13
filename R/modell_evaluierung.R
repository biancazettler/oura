source("R/variablenselektion_zv_sleepscore.R")
source("R/modelle_nichtlinear_zv_sleepscore.R")


# modellgüte 

# Posterior Predictive Check
pp_check(model_bayes)
pp_check(model_poly)
pp_check(model_spline)
pp_check(model_quad_spline)

# Vergleich der Modelle mit WAIC
waic_poly <- waic(model_poly)
waic_spline <- waic(model_spline)


# WAIC-Ergebnisse vergleichen
print(waic_poly)
print(waic_spline)

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

# Vergleiche die Modelle basierend auf LOO
loo_compare(model_poly, model_spline, model_linear, model_tavg, model_tavg_resp,
            model_tsun, model_tsun_tavg_resp)
