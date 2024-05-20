# install packages 
library(tidyverse)
library(dplyr)

# data of another person
# get data 
#getwd()
data_jc <- read.csv("data/jc_oura.csv")
head(data_jc)
min(data_jc$date) # starting date: 2021-02-09
max(data_jc$date) # end date: 2024-05-18 -> data of more than 3 years

# data cleansing / pre processing
# NA's (missing values)
sum(is.na(data_jc)) # sum of missing values: 2314

missing_data_cols <- data_jc %>% # get columns with missing values
  summarise_all(~any(is.na(.)))

print(missing_data_cols) # get columns with missing values

sum(missing_data_cols == TRUE) # 26 columns with missing values

missing_data_rows <- data_jc %>% # get rows with missing values
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique(missing_data_rows$row_id) # a lot of missing rows
length(unique(missing_data_rows$row_id)) # 89 days missing 
data_jc[unique(missing_data_rows$row_id), ]$date

# -> delete these rows (days), so there are no NA's in further analysis
data_no_nas_jc <- data_jc[-unique(missing_data_rows$row_id),]
sum(is.na(data_no_nas_jc)) # 0 NA'S 

# datatypes
data_no_nas_jc$Activity.Score <- as.integer(data_no_nas_jc$Activity.Score)
data_no_nas_jc$Sleep.Score <- as.integer(data_no_nas_jc$Sleep.Score)
data_no_nas_jc$Readiness.Score  <- as.integer(data_no_nas_jc$Readiness.Score)
data_no_nas_jc$HRV.Balance.Score  <- as.integer(data_no_nas_jc$HRV.Balance.Score)




