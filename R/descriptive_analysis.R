source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

# descriptive analysis 
summary(data_no_nas_bz)
summary(data_no_nas_bz[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")])
summary(data_no_nas_jc)
summary(data_no_nas_jc[c("Sleep.Score", "Activity.Score", "Average.Resting.Heart.Rate")])

# Histogramme für Schlafqualität, Aktivitätsscore und Ruheherzfrequenz
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
