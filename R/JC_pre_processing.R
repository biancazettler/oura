############################## TO DO: USE CURRENT NEWEST DATE ############################

# install packages 
library(tidyverse)
library(dplyr)

# get data 
#getwd()
data_jc <- read.csv("data/oura_2020-08-01_2024-10-13_trends.csv")
head(data_jc)
summary(data_jc)
min(data_jc$date) # starting date: 2021-02-09
max(data_jc$date) # end date: current date -> data of more than 3,5 years

# data cleansing / pre processing
# NA's (missing values)
sum(is.na(data_jc)) # sum of missing values: 2614

missing_data_cols <- data_jc %>% # get columns with missing values
  summarise_all(~any(is.na(.)))

print(missing_data_cols) # get columns with missing values

sum(missing_data_cols == TRUE) # 25 columns with missing values

missing_data_rows <- data_jc %>% # get rows with missing values
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique(missing_data_rows$row_id) # a lot of missing rows
length(unique(missing_data_rows$row_id)) # 105 days missing 
data_jc[unique(missing_data_rows$row_id), ]$date

# -> delete these rows (days), so there are no NA's in further analysis
data_no_nas_jc <- data_jc[-unique(missing_data_rows$row_id),]
sum(is.na(data_no_nas_jc)) # 0 NA'S 

# datatypes
data_no_nas_jc$Activity.Score <- as.integer(data_no_nas_jc$Activity.Score)
data_no_nas_jc$Sleep.Score <- as.integer(data_no_nas_jc$Sleep.Score)
data_no_nas_jc$Readiness.Score  <- as.integer(data_no_nas_jc$Readiness.Score)
data_no_nas_jc$HRV.Balance.Score  <- as.integer(data_no_nas_jc$HRV.Balance.Score)
# new NA's in activity score and balance score (None -> NA)
data_no_nas_jc <- na.omit(data_no_nas_jc)
summary(data_no_nas_jc)
nrow(data_no_nas_jc) # 839 rows = 839 days ~ 2,3 Years

# Another way of handling NA's: Imputation (median or mean)
library(mice)
# Kopiere den ursprünglichen Datensatz
data_imputed_jc <- data_jc
# Wandelt alle "None" Werte in NA um
data_imputed_jc[data_imputed_jc == "None"] <- NA
# Umwandlung der Datumsfelder in das Datumsformat (falls notwendig)
data_imputed_jc$date <- as.Date(data_imputed_jc$date, format="%Y-%m-%d")
data_imputed_jc$Bedtime.Start <- as.POSIXct(data_imputed_jc$Bedtime.Start, format="%Y-%m-%dT%H:%M:%OS")
data_imputed_jc$Bedtime.End <- as.POSIXct(data_imputed_jc$Bedtime.End, format="%Y-%m-%dT%H:%M:%OS")
# Konvertiere nicht-numerische Variablen, falls nötig, in numerische Variablen
data_imputed_jc <- data_imputed_jc %>%
  mutate_if(is.character, as.numeric)
# easy Imputation with mean
for(i in 1:ncol(data_imputed_jc)) {
  data_imputed_jc[is.na(data_imputed_jc[, i]), i] <- mean(data_imputed_jc[, i], na.rm = TRUE)
}
nrow(data_imputed_jc)

# 1016 rows (days) -> 2,8 years, so ~ 5 months more data than when NA's would be removed. 
################ TODO: try models with this data and check for differences! #####################

#### select scores data from data without nas and set numeric datatypes
scores_no_nas <- data_no_nas_jc %>%
  select(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
         Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
         Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
         HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

# convert to numeric datatypes
scores_data_no_nas <- scores_no_nas %>% mutate_at(vars(Sleep.Score, Total.Sleep.Score, REM.Sleep.Score, Deep.Sleep.Score, 
                                                      Sleep.Efficiency.Score, Restfulness.Score, Sleep.Latency.Score,
                                                      Sleep.Timin.Score, Activity.Score, Stay.Active.Score, Move.Every.Hour.Score,
                                                      HRV.Balance.Score, Activity.Balance.Score, Readiness.Score, Temperature.Score,
                                                      Resting.Heart.Rate.Score), as.numeric)

# check for nas and remove them
sum(is.na(scores_data_no_nas)) # 7
scores_data_no_nas_jc <- na.omit(scores_data_no_nas)
sum(is.na(scores_data_no_nas_jc)) # 0
str(scores_data_no_nas_jc)
nrow(scores_data_no_nas_jc)

#### do the same with data with imputed nas 

scores_data <- data_imputed_jc %>%
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

sum(is.na(scores_data_numeric)) # 0
str(scores_data_numeric)
nrow(scores_data_numeric)