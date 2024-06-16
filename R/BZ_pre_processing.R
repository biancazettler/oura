# install packages 
library(tidyverse)
library(dplyr)

# own data
# get data 
#getwd()
data_bz <- read.csv("data/oura_2024-02-24_2024-06-15_trends.csv")
head(data_bz)
min(data_bz$date) # starting date: 2024-02-23
max(data_bz$date) # end date: current date -> ~ 4-5 months 

# data cleansing / pre processing
# NA's (missing values)
sum(is.na(data_bz)) # sum of missing values: 29

missing_data_cols <- data_bz %>% # get columns with missing values
  summarise_all(~any(is.na(.)))

print(missing_data_cols) # get columns with missing values

sum(missing_data_cols == TRUE) # 29 columns with missing values

missing_data_rows <- data_bz %>% # get rows with missing values
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique(missing_data_rows$row_id) # only one row 
data_bz[unique(missing_data_rows$row_id), ]$date

# -> missing values only on day 2024-05-04
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

