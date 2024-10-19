source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

# correlation analysis
# 1. data with scores and no na's

cor_scores <- cor(scores_data_no_nas_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores <- scores_data_no_nas_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main <- cor(main_scores, use = "complete.obs")
print(cor_scores_main)

corrplot(cor_scores_main, method = "circle")

# 2. data with scores and imputed nas

cor_scores <- cor(scores_imputed_nas_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores_imputed <- scores_imputed_nas_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main_im <- cor(main_scores_imputed, use = "complete.obs")
print(cor_scores_main_im)

corrplot(cor_scores_main_im, method = "circle")

# Korrelation zwischen Sleep.Score und anderen Variablen mit daten mit imputierten nas
cor_matrix_sleep <- cor(data_imputed_jc %>% select(Sleep.Score, Total.Sleep.Duration, REM.Sleep.Score,
                                           Deep.Sleep.Score, Light.Sleep.Duration,
                                           Sleep.Latency.Score, Total.Sleep.Score, Sleep.Latency,
                                           REM.Sleep.Duration), use = "complete.obs")
print(cor_matrix_sleep)

# (relativ) hohe korrelation von sleep score mit total sleep duration, rem sleep score,
# deep sleep score, total sleep score, rem sleep duration und rem sleep score -> 
# sleep score aussagekräftig für die gesamt schlafqualität als zielgröße

# 3. with data with na's

# Korrelationen berechnen und anzeigen
cor_matrix <- cor(data_jc %>% select(Sleep.Score, Activity.Score, Average.Resting.Heart.Rate, Average.HRV, Temperature.Deviation...C.), use = "complete.obs")
print(cor_matrix)

# Korrelationen visualisieren (mit ggplot2 oder corrplot)
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Berechnung der Korrelationen zwischen Sleep.Score und potenziellen Prädiktoren
cor_matrix_prädiktoren <- cor(data_imputed_jc %>% 
                                select(Sleep.Score, Activity.Score, Total.Burn, Steps, 
                                       Average.Resting.Heart.Rate, Lowest.Resting.Heart.Rate, 
                                       Average.HRV, Temperature.Deviation...C.), 
                              use = "complete.obs")
print(cor_matrix_prädiktoren)

# Optional: Visualisierung der Korrelationen
library(corrplot)
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Lowest.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
library(car)
vif_model <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Lowest.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model)

# zu hohe Korrelation zwischen lowest resting heart rate und average resting heart rate 
# -> nur eins aufnehmen:
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
vif_model_easy <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model_easy)
# korrelation ist ok
