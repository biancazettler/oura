source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")
library(ggplot2)

# descriptive analysis 
summary(data_no_nas_bz)
summary(data_no_nas_bz[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")])
summary(data_no_nas_jc)
summary(data_no_nas_jc[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")])

# Histogram for sleep score, activity score and resting heart rate
hist(data_no_nas_bz$Sleep.Score, main="Verteilung der Schlafqualität", xlab="Schlafqualität")
hist(data_no_nas_bz$Activity.Score, main="Verteilung der Aktivitätsscores", xlab="Aktivitätsscore")
hist(data_no_nas_bz$Average.Resting.Heart.Rate, main="Verteilung der Ruheherzfrequenz", xlab="Ruheherzfrequenz")

hist(data_no_nas_jc$Sleep.Score, main="Verteilung der Schlafqualität", xlab="Schlafqualität")
hist(data_no_nas_jc$Activity.Score, main="Verteilung der Aktivitätsscores", xlab="Aktivitätsscore")
hist(data_no_nas_jc$Average.Resting.Heart.Rate, main="Verteilung der Ruheherzfrequenz", xlab="Ruheherzfrequenz")

# Dichtediagramme
plot(density(data_no_nas_bz$Sleep.Score), main="Dichtediagramm der Schlafqualität")
plot(density(data_no_nas_bz$Activity.Score), main="Dichtediagramm der Aktivitätsscores")
plot(density(data_no_nas_bz$Average.Resting.Heart.Rate), main="Dichtediagramm der Ruheherzfrequenz")

plot(density(data_no_nas_jc$Sleep.Score), main="Dichtediagramm der Schlafqualität")
plot(density(data_no_nas_jc$Activity.Score, na.rm = TRUE), main="Dichtediagramm der Aktivitätsscores")
plot(density(data_no_nas_jc$Average.Resting.Heart.Rate), main="Dichtediagramm der Ruheherzfrequenz")

# Korrelationsmatrix
cor(data_no_nas_jc[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")], use="complete.obs")
cor(data_no_nas_bz[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")], use="complete.obs")

# Scatterplots
pairs(~ Sleep.Score + Activity.Score + Average.Resting.Heart.Rate, data=data_no_nas_jc, main="Scatterplot-Matrix")
pairs(~ Sleep.Score + Activity.Score + Average.Resting.Heart.Rate, data=data_no_nas_bz, main="Scatterplot-Matrix")

pairs(~ Sleep.Score + Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score + Temperature.Deviation...C., data = data_no_nas_bz)
pairs(~ Sleep.Score + Total.Sleep.Duration + Average.Resting.Heart.Rate + Activity.Score + Temperature.Deviation...C., data = data_no_nas_jc)

# search for outliers
# Boxplot für eine kontinuierliche Variable
boxplot(data_no_nas_jc$Sleep.Score, main = "Boxplot - Sleep Score")
boxplot(data_no_nas_bz$Sleep.Score, main = "Boxplot - Sleep Score")

# Berechnung von Z-Scores für 'Sleep.Score'
z_scores_jc <- scale(data_no_nas_jc$Sleep.Score)
data_no_nas_jc$z_score <- z_scores_jc
z_scores_bz <- scale(data_no_nas_bz$Sleep.Score)
data_no_nas_bz$z_score <- z_scores_bz

# Filtern von Ausreißern
outliers_jc <- data_no_nas_jc[abs(data_no_nas_jc$z_score) > 3,]
print(outliers_jc)
nrow(outliers_jc) # 3 days
outliers_bz <- data_no_nas_bz[abs(data_no_nas_bz$z_score) > 3,]
print(outliers_bz)
nrow(outliers_bz) # 3 days

# Entscheidung über Umgang mit Ausreißern: Entfernen oder Behalten
data_no_outliers_jc <- data_no_nas_jc[abs(data_no_nas_jc$z_score) <= 3,]
data_no_outliers_bz <- data_no_nas_bz[abs(data_no_nas_bz$z_score) <= 3,]



#### deskriptive analyse zielgröße

# sleep score
# Verteilung des Sleep Scores
ggplot(data_imputed_jc, aes(x = Sleep.Score)) + 
  geom_histogram(binwidth = 5, fill = "lightblue") + 
  labs(title = "Verteilung des Sleep Scores", x = "Sleep Score", y = "Häufigkeit")

# Boxplot zur Visualisierung von Ausreißern
ggplot(data_imputed_jc, aes(x = "", y = Sleep.Score)) + 
  geom_boxplot() + 
  labs(title = "Boxplot des Sleep Scores", x = "", y = "Sleep Score")

# gesamtschlafzeit
ggplot(data_jc, aes(x = Total.Sleep.Duration)) + 
  geom_histogram(binwidth = 500, fill = "green") + 
  labs(title = "Verteilung der Gesamtschlafzeit", x = "Total Sleep Duration (sec)")

# rem schlafzeit
ggplot(data_jc, aes(x = REM.Sleep.Score)) + 
  geom_histogram(binwidth = 5, fill = "purple") + 
  labs(title = "Verteilung des REM Sleep Scores", x = "REM Sleep Score")


#### deskriptive analyse einflussfaktoren 

# Aktivitätsbewertung
ggplot(data_imputed_jc, aes(x = Activity.Score)) + 
  geom_histogram(binwidth = 5, fill = "lightblue") + 
  labs(title = "Verteilung des Activity Scores", x = "Activity Score")

# Ruheherzfrequenz
ggplot(data_imputed_jc, aes(x = Average.Resting.Heart.Rate)) + 
  geom_histogram(binwidth = 2, fill = "lightgreen") + 
  labs(title = "Verteilung der Ruheherzfrequenz", x = "Average Resting Heart Rate")

# HRV
ggplot(data_imputed_jc, aes(x = Average.HRV)) + 
  geom_histogram(binwidth = 5, fill = "purple") + 
  labs(title = "Verteilung der Herzfrequenzvariabilität", x = "Average HRV")

# Körpertemperaturabweichung
ggplot(data_imputed_jc, aes(x = Temperature.Deviation...C.)) + 
  geom_histogram(binwidth = 0.1, fill = "orange") + 
  labs(title = "Verteilung der Körpertemperaturabweichung", x = "Temperature Deviation (Celsius)")



# Erstelle Scatterplots für die Zielvariable gegen jede kontinuierliche Kovariable
library(ggplot2)
ggplot(data_no_nas_jc_clean, aes(x = Respiratory.Rate, y = Sleep.Score)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sleep.Score vs Respiratory Rate")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = tavg, y = Sleep.Score)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sleep.Score vs Tavg")

ggplot(data_no_nas_jc_clean, aes(x = tsun, y = Sleep.Score)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sleep.Score vs Tsun")

ggplot(data_no_nas_jc_clean, aes(x = date, y = Sleep.Score)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sleep.Score vs Date")


### zv respiratory rate:
ggplot(data_no_nas_jc_clean, aes(x = pres, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs pres")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = tavg, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs Tavg")
# überwiegend linearer zusammenhang -> eher lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = tsun, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs Tsun")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = date, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs Date")
# kein linearer zusammenhang -> eher kein lineares modell

ggplot(data_no_nas_jc_clean, aes(x = Resting.Heart.Rate.Score, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs Resting heart rate score")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = HRV.Balance.Score, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs hrv balance score")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = Temperature.Score, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs temperature score")
# kein linearer zusammenhang -> eher kein lineares modell 

ggplot(data_no_nas_jc_clean, aes(x = Recovery.Index.Score, y = Respiratory.Rate)) + 
  geom_point() + geom_smooth(method = "loess", color = "blue") +
  labs(title = "Respiratory Rate vs recovery index score")
# kein linearer zusammenhang -> eher kein lineares modell

