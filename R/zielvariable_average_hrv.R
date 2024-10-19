library(ggplot2)
### average hrv und average resting heart rate haben den größten einfluss -> neue zielgrößen

# erste möglichkeit: Average HRV
# 1. Einflussgrößen finden
# Verteilungen visualisieren
hist(data_imputed_jc$Average.HRV, main="Verteilung der durchschnittlichen HRV", xlab="Average HRV")
# -> Average HRV annähernd normalverteilt
hist(data_imputed_jc$Activity.Score, main="Verteilung des Activity Scores", xlab="Activity Score")
hist(data_imputed_jc$Total.Burn, main="Verteilung des Total Burn", xlab="Total Burn")
# ...

# Scatterplots mit Regressionslinien der Beziehung zwischen Prädiktoren und Zielgrößen
ggplot(data_imputed_jc, aes(x = Temperature.Deviation...C., y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Temperature.Deviation...C. und Average HRV")
# stark negativer trend -> ins modell

ggplot(data_imputed_jc, aes(x = Activity.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Activity Score und Average HRV")
# waagrechter trend -> nicht mit ins modell

ggplot(data_imputed_jc, aes(x = date, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Datum und Average HRV")
# waagrecht -> nicht ins modell

ggplot(data_imputed_jc, aes(x = Respiratory.Rate, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Respiratory.Rate")
# stark negativer trend -> ins modell

ggplot(data_imputed_jc, aes(x = Stay.Active.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Stay.Active.Score")
# waagrecht -> nicht ins modell

ggplot(data_imputed_jc, aes(x = Move.Every.Hour.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Move.Every.Hour.Score")
# waagrecht -> nicht ins modell

# so ist es mit jedem aktivitätsmerkmal, außer:

ggplot(data_imputed_jc, aes(x = Rest.Time, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Rest Time")
# negativer zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Low.Activity.Time, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und low activity Time")
# positiver zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Average.MET, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Average.MET")
# positiver zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Readiness.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Readiness Score")
# starker positiver zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Previous.Night.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Previous.Night.Score")
# starker positiver zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Previous.Day.Activity.Score, y = Average.HRV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average HRV und Previous.Day.Activity.Score")
# starker positiver zusammenhang -> ins modell

# 2. Korrelationen bzw. Multikollinearität:
# Berechne die Korrelationen
cor_matrix <- cor(data_imputed_jc[c("Temperature.Deviation...C.", "Respiratory.Rate", "Rest.Time", "Low.Activity.Time", "Average.MET", 
                         "Readiness.Score", "Previous.Night.Score", "Previous.Day.Activity.Score")], use = "complete.obs")

# Multikollinearität (VIF) überprüfen
library(car)
model <- lm(Average.HRV ~ Temperature.Deviation...C. + Respiratory.Rate + Rest.Time + Low.Activity.Time + Average.MET +
              Readiness.Score + Previous.Night.Score + Previous.Day.Activity.Score, data = data_imputed_jc)

vif(model) # -> KEINE multikollinearität vorhanden 

# 3. bayesianische Modelle mit ZG Average HRV
# Lade das brms Paket
library(brms)

# Bayesianisches Modell für Average HRV als Zielgröße
model_hrv <- brm(
  formula = Average.HRV ~ Temperature.Deviation...C. + Respiratory.Rate + Rest.Time + Low.Activity.Time + 
    Average.MET + Readiness.Score + Previous.Night.Score + Previous.Day.Activity.Score, 
  data = data_imputed_jc, 
  family = gaussian(),
  prior = c(set_prior("normal(0, 5)", class = "b")),
  iter = 2000, warmup = 500, chains = 4, cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_hrv)

# Posterior Plot
plot(model_hrv)
# Ergebnisse: siehe notizen