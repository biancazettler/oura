source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

# Standardisierung der Prädiktoren
standardized_data_jc <- data_no_nas_jc
numerical_columns <- sapply(standardized_data_jc, is.numeric)  # Identifiziere nur numerische Spalten

standardized_data_jc[, numerical_columns] <- scale(standardized_data_jc[, numerical_columns])

# Überprüfung der Skalierung
summary(standardized_data_jc)

# Histogramm von Sleep.Score um Zielvariable auf Normalverteilung zu überprüfen
hist(standardized_data_jc$Sleep.Score, main = "Histogram of Sleep Score", xlab = "Sleep Score")

# Q-Q-Plot
qqnorm(standardized_data_jc$Sleep.Score)
qqline(standardized_data_jc$Sleep.Score)

# logarithmierung geht nicht, da viele Werte nicht positiv sind

# Shapiro-Wilk Test auf Normalverteilung
shapiro.test(standardized_data_jc$Sleep.Score)
# -> keine Normalverteilung!

################ TO DO: keine Normalverteilung der Zielgröße Sleep Score -> Transformierung!

# Prüfen der Zielgröße auf normalverteilung
install.packages("ggplot2")
install.packages("ggpubr")  # Für den Q-Q-Plot
install.packages("stats")

library(ggplot2)
library(ggpubr)

# Histogramm
ggplot(main_scores, aes(x = Sleep.Score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogramm des Sleep Scores")

# Q-Q-Plot
ggqqplot(main_scores$Sleep.Score)

# Shapiro-Wilk-Test
shapiro_test <- shapiro.test(main_scores$Sleep.Score)
print(shapiro_test)

# Logarithmische Transformation
main_scores$Log.Sleep.Score <- log(main_scores$Sleep.Score)

# Überprüfe erneut die Normalverteilung des transformierten Scores
shapiro_test_log <- shapiro.test(main_scores$Log.Sleep.Score)
print(shapiro_test_log)

# Histogramm des transformierten Scores
ggplot(main_scores, aes(x = Log.Sleep.Score)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogramm des logarithmisch transformierten Sleep Scores")

# Q-Q-Plot
qqnorm(main_scores$Log.Sleep.Score)
qqline(main_scores$Log.Sleep.Score, col = "red")

# PROBLEM: sleep score immer noch nicht normalverteilt

# dasselbe für imputierte nas
# Histogramm der Zielvariable (Sleep.Score)
ggplot(data_imputed_jc, aes(x = Sleep.Score)) + 
  geom_histogram(binwidth = 5, fill = "lightblue") + 
  labs(title = "Verteilung des Sleep Scores", x = "Sleep Score")

# QQ-Plot für den Sleep Score
qqnorm(data_imputed_jc$Sleep.Score)
qqline(data_imputed_jc$Sleep.Score, col = "red")

# log transformation für sleep score weil keine normalverteilung gegeben ist
data_imputed_jc$log_Sleep_Score <- log(data_imputed_jc$Sleep.Score)

# Histogramm der log-transformierten Zielvariable
ggplot(data_imputed_jc, aes(x = log_Sleep_Score)) + 
  geom_histogram(binwidth = 0.1, fill = "lightblue") + 
  labs(title = "Verteilung des log-transformierten Sleep Scores", x = "Log(Sleep Score)")

# QQ-Plot der log-transformierten Zielvariable
qqnorm(data_imputed_jc$log_Sleep_Score)
qqline(data_imputed_jc$log_Sleep_Score, col = "red")

# Quadratwurzel transformation 
data_imputed_jc$sqrt_Sleep_Score <- sqrt(data_imputed_jc$Sleep.Score)

# Histogramm der log-transformierten Zielvariable
ggplot(data_imputed_jc, aes(x = sqrt_Sleep_Score)) + 
  geom_histogram(binwidth = 0.1, fill = "lightblue") + 
  labs(title = "Verteilung des Quadratwurzel-transformierten Sleep Scores", x = "Quadratwurzel(Sleep Score)")

# QQ-Plot der log-transformierten Zielvariable
qqnorm(data_imputed_jc$sqrt_Sleep_Score)
qqline(data_imputed_jc$sqrt_Sleep_Score, col = "red")

# Box-Cox Transformation mit dem Paket MASS
library(MASS)

# Berechne das beste Lambda für die Box-Cox Transformation
boxcox_result <- boxcox(lm(Sleep.Score ~ 1, data = data_imputed_jc), lambda = seq(-2, 2, by = 0.1))

# Bestes Lambda wählen
best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Box-Cox Transformation anwenden
data_imputed_jc$boxcox_Sleep_Score <- (data_imputed_jc$Sleep.Score^best_lambda - 1) / best_lambda

# Histogramm der log-transformierten Zielvariable
ggplot(data_imputed_jc, aes(x = boxcox_Sleep_Score)) + 
  geom_histogram(binwidth = 0.1, fill = "lightblue") + 
  labs(title = "Verteilung des Box Cox-transformierten Sleep Scores", x = "Box-Cox(Sleep Score)")

# QQ-Plot der log-transformierten Zielvariable
qqnorm(data_imputed_jc$boxcox_Sleep_Score)
qqline(data_imputed_jc$boxcox_Sleep_Score, col = "red")


#### lineare Beziehung zwischen Prädiktoren und Zielvariable überprüfen:

# Scatterplot der transformierten Zielvariable vs. Average.Resting.Heart.Rate
ggplot(data_imputed_jc, aes(x = Average.Resting.Heart.Rate, y = sqrt_Sleep_Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Beziehung zwischen Average Resting Heart Rate und transformiertem Sleep Score", 
       x = "Average Resting Heart Rate", 
       y = "Quadratwurzel(Sleep Score)")

# Scatterplot der transformierten Zielvariable vs. Average.HRV
ggplot(data_imputed_jc, aes(x = Average.HRV, y = sqrt_Sleep_Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Beziehung zwischen Average HRV und transformiertem Sleep Score", 
       x = "Average HRV", 
       y = "Quadratwurzel(Sleep Score)")

# Scatterplot der transformierten Zielvariable vs. Temperature.Deviation...C.
ggplot(data_imputed_jc, aes(x = Temperature.Deviation...C., y = sqrt_Sleep_Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Beziehung zwischen Temperaturabweichung und transformiertem Sleep Score", 
       x = "Temperaturabweichung in Celsius", 
       y = "Quadratwurzel(Sleep Score)")
# problem: fast waagrecht -> kein wirklicher einfluss => im Modell weglassen

# Scatterplot der transformierten Zielvariable vs. Activity Score
ggplot(data_imputed_jc, aes(x = Activity.Score, y = sqrt_Sleep_Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Beziehung zwischen Activity Score und transformiertem Sleep Score", 
       x = "Activity Score", 
       y = "Quadratwurzel(Sleep Score)")
# problem: fast waagrecht -> kein wirklicher einfluss => im Modell weglassen

# Scatterplot der transformierten Zielvariable vs. Datum
ggplot(data_imputed_jc, aes(x = date, y = sqrt_Sleep_Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Beziehung zwischen Datum und transformiertem Sleep Score", 
       x = "Datum", 
       y = "Quadratwurzel(Sleep Score)")
# 