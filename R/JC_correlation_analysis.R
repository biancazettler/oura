# correlation analysis
# 1. data with scores 

library(dplyr)
scores_data <- data_no_nas_jc %>%
  select(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
         Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
         Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
         HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

# convert to numeric datatypes
scores_data_numeric <- scores_data %>% mutate_at(vars(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
                                                      Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
                                                      Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
                                                      HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
                                                      Resting.Heart.Rate.Score), as.numeric)

sum(is.na(scores_data_numeric)) # 7
scores_data_no_nas_jc <- na.omit(scores_data_numeric)
sum(is.na(scores_data_no_nas_jc)) # 7
str(scores_data_no_nas_jc)

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
