# Zielvariable average resting heart rate (mit Average HRV größten einfluss auf die schlafqualität)

# 1. Einflussgrößen finden
# Scatterplots mit Regressionslinien der Beziehung zwischen Prädiktoren und Zielgrößen
ggplot(data_imputed_jc, aes(x = Temperature.Deviation...C., y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Temperature.Deviation...C. und Average Resting Heart Rate")
# stark positiver trend -> ins modell

ggplot(data_imputed_jc, aes(x = Activity.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Activity Score und Average Resting Heart Rate")
# waagrechter trend -> nicht mit ins modell

ggplot(data_imputed_jc, aes(x = date, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Datum und Average Resting Heart Rate")
# waagrecht -> nicht ins modell

ggplot(data_imputed_jc, aes(x = Respiratory.Rate, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Respiratory.Rate")
# stark positiver trend -> ins modell

ggplot(data_imputed_jc, aes(x = Stay.Active.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Stay.Active.Score")
# waagrecht -> nicht ins modell

ggplot(data_imputed_jc, aes(x = Move.Every.Hour.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Move.Every.Hour.Score")
# waagrecht -> nicht ins modell

# so ist es mit jedem aktivitätsmerkmal, außer:

ggplot(data_imputed_jc, aes(x = Rest.Time, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Rest Time")
# positiver zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Low.Activity.Time, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und low activity Time")
# negativer zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Average.MET, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Average.MET")
# negativer zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Readiness.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Readiness Score")
# starker negativer zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Previous.Night.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Previous.Night.Score")
# starker negativer zusammenhang -> ins modell

ggplot(data_imputed_jc, aes(x = Previous.Day.Activity.Score, y = Average.Resting.Heart.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Beziehung zwischen Average Resting Heart Rate und Previous.Day.Activity.Score")
# starker positiver zusammenhang -> ins modell

# externe Variable Mondphasen mit ins Modell:
library(lunar)

# Datum in das richtige Format bringen, falls noch nicht geschehen
data_imputed_jc$date <- as.Date(data_imputed_jc$date)

# Mondphasen für jedes Datum berechnen
data_imputed_jc$moon_phase <- lunar.phase(data_imputed_jc$date, name = TRUE)

# Mondphasen als Faktor umwandeln, um sie im Modell zu verwenden
data_imputed_jc$moon_phase <- factor(data_imputed_jc$moon_phase)

# 2. Korrelationen bzw. Multikollinearität:
# Berechne die Korrelationen
cor_matrix <- cor(data_imputed_jc[c("Temperature.Deviation...C.", "Respiratory.Rate", "Rest.Time", "Low.Activity.Time", "Average.MET", 
                                    "Readiness.Score", "Previous.Night.Score", "Previous.Day.Activity.Score")], use = "complete.obs")

# Multikollinearität (VIF) überprüfen
library(car)
model <- lm(Average.Resting.Heart.Rate ~ Temperature.Deviation...C. + Respiratory.Rate + Rest.Time + Low.Activity.Time + Average.MET +
              Readiness.Score + Previous.Night.Score + Previous.Day.Activity.Score + moon_phase, data = data_imputed_jc)
# Variance Inflation Factor (VIF)-Werte
vif(model) # -> KEINE multikollinearität vorhanden, da 1 -> keine Korrelation mit anderen Prädiktoren, 1 bis 5 -> unproblematisch
# ( = keine Mulitkollinearität, bei mir sind alle < 3) und ab 10 hohe korrelation

# 3. bayesianische Modelle mit ZG Average Resting Heart Rate

library(brms)
# Bayesianisches Modell für Average HRV als Zielgröße
model_resting_heartrate <- brm(
  formula = Average.Resting.Heart.Rate ~ Temperature.Deviation...C. + Respiratory.Rate + Rest.Time + Low.Activity.Time + 
    Average.MET + Readiness.Score + Previous.Night.Score + Previous.Day.Activity.Score, 
  data = data_imputed_jc, 
  family = gaussian(),
  prior = c(set_prior("normal(0, 5)", class = "b")),
  iter = 2000, warmup = 500, chains = 4, cores = 4
)

# Zusammenfassung des Modells anzeigen
summary(model_resting_heartrate)

# Posterior Plot
plot(model_resting_heartrate)
# Ergebnisse: siehe notizen

# wochenend effekt:
# Datumsspalte in ein Date-Format umwandeln
data_imputed_jc$date <- as.Date(data_imputed_jc$date)

# Erstellen einer neuen Spalte, die angibt, ob es sich um einen Wochenendtag handelt
data_imputed_jc$is_weekend <- ifelse(weekdays(data_imputed_jc$date) %in% c("Samstag", "Sonntag"), 1, 0)

# Das Modell mit dem Wochenend-Effekt
model_resting_heartrate_weekend <- brm(
  Average.Resting.Heart.Rate ~ Temperature.Deviation...C. + Respiratory.Rate + Rest.Time + Low.Activity.Time + 
    Average.MET + Readiness.Score + Previous.Night.Score + Previous.Day.Activity.Score + is_weekend,
  data = data_imputed_jc,
  family = gaussian(),
  prior = c(set_prior("normal(0, 10)", class = "b"))
)

# Modell zusammenfassen
summary(model_resting_heartrate_weekend)

