# install packages 
library(tidyverse)
library(dplyr)

# own data
# get data 
#getwd()
data_bz <- read.csv("data/oura_2024-02-01_2024-09-17_trends.csv")
head(data_bz)
min(data_bz$date) # starting date: 2024-02-23
max(data_bz$date) # end date: current date -> ~ 5 months 

# data cleansing / pre processing
# NA's (missing values)
sum(is.na(data_bz)) # sum of missing values: 58

missing_data_cols <- data_bz %>% # get columns with missing values
  summarise_all(~any(is.na(.)))

print(missing_data_cols) # get columns with missing values

sum(missing_data_cols == TRUE) # 29 columns with missing values

missing_data_rows <- data_bz %>% # get rows with missing values
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique(missing_data_rows$row_id) # only two rows  
data_bz[unique(missing_data_rows$row_id), ]$date
# -> missing values only on two days: "2024-05-04" and "2024-02-23"
# 29 columns on these two days have missing values
# -> delete this row (day), so there are no NA's in further analysis

data_no_nas_bz <- data_bz[-unique(missing_data_rows$row_id),]
sum(is.na(data_no_nas_bz)) # 0 NA'S 

# datatypes
data_no_nas_bz$Activity.Score <- as.integer(data_no_nas_bz$Activity.Score)
data_no_nas_bz$Sleep.Score <- as.integer(data_no_nas_bz$Sleep.Score)
data_no_nas_bz$Readiness.Score  <- as.integer(data_no_nas_bz$Readiness.Score)
data_no_nas_bz$HRV.Balance.Score  <- as.integer(data_no_nas_bz$HRV.Balance.Score)
# new NA's in activity score and balance score (None -> NA)
data_no_nas_bz <- na.omit(data_no_nas_bz)
summary(data_no_nas_bz)
nrow(data_no_nas_bz) # 138 rows = 138 days ~ 5 months

# Another way of handling NA's: Imputation (median or mean)
library(mice)
# easy Imputation with mean
data_imputed_bz <- data_bz
for(i in 1:ncol(data_bz)) {
  data_imputed_bz[is.na(data_imputed_bz[, i]), i] <- mean(data_bz[, i], na.rm = TRUE)
}
nrow(data_imputed_bz)
# 159 rows (days), so 3 weeks more data than when NA's would be removed. 
################ TODO: try models with this data and check for differences! #####################

# Standardisierung der Prädiktoren
standardized_data_bz <- data_no_nas_bz
numerical_columns <- sapply(standardized_data_bz, is.numeric)  # Identifiziere nur numerische Spalten

standardized_data_bz[, numerical_columns] <- scale(standardized_data_bz[, numerical_columns])

# Überprüfung der Skalierung
summary(standardized_data_bz)

# Histogramm von Sleep.Score um Zielvariable auf Normalverteilung zu überprüfen
hist(standardized_data_bz$Sleep.Score, main = "Histogram of Sleep Score", xlab = "Sleep Score")

# Q-Q-Plot
qqnorm(standardized_data_bz$Sleep.Score)
qqline(standardized_data_bz$Sleep.Score)

# logarithmierung geht nicht, da viele Werte nicht positiv sind

# Shapiro-Wilk Test auf Normalverteilung
shapiro.test(standardized_data_bz$Sleep.Score)
# -> keine Normalverteilung!

################ TO DO: keine Normalverteilung der Zielgröße Sleep Score -> Transformierung!




