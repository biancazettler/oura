# install packages 
#install.packages("xlsx")
#install.packages("tidyverse")
library(tidyverse)
library(xlsx)

# get data 
#getwd()
all_data <- read.csv("BZ_oura_2024-02-08_2024-05-14_trends.csv")
head(all_data)

# data cleansing / pre processing
# NA's (missing values)
sum(is.na(all_data)) # sum of missing values: 58

missing_data_cols <- all_data %>% # get columns with missing values
  summarise_all(~any(is.na(.)))

print(missing_data_cols) # get columns with missing values

sum(missing_data_cols == TRUE) # 29 columns with missing values

missing_data_rows <- all_data %>% # get rows with missing values
  mutate(row_id = row_number()) %>%
  gather(key = "variable", value = "value", -row_id) %>%
  filter(is.na(value))

unique(missing_data_rows$row_id) # only row 81 and 82
all_data[unique(missing_data_rows$row_id), ]$date

# -> missing values on only two days: "2024-02-23" and "2024-05-04" 
# 29 columns on these two days have missing values
# -> delete these two rows (days), so there are no NA's in further analysis

data_no_nas <- all_data[-unique(missing_data_rows$row_id),]
sum(is.na(data_no_nas)) # 0 NA'S 
