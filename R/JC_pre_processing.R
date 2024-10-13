############################## TO DO: USE CURRENT NEWEST DATE ############################

# install packages 
library(tidyverse)
library(dplyr)

# data of another person
# get data 
#getwd()
data_jc <- read.csv("data/oura_2020-08-01_2024-10-13_trends.csv")
head(data_jc)
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
# easy Imputation with mean
data_imputed_jc <- data_jc
for(i in 1:ncol(data_jc)) {
  data_imputed_jc[is.na(data_imputed_jc[, i]), i] <- mean(data_jc[, i], na.rm = TRUE)
}
nrow(data_imputed_jc)
# 1016 rows (days) -> 2,8 years, so ~ 5 months more data than when NA's would be removed. 
################ TODO: try models with this data and check for differences! #####################


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




