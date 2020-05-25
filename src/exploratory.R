library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggforce)

train <- read_csv("./data/train.csv") 
test <- read_csv("./data/test.csv")

cols_train <- names(train)
cols_test <- names(test)

# Selecting variables in both dataset
cols_both_id <- cols_train %in% cols_test

selected_cols <- cols_train[cols_both_id]

# Selecting 
train %>%
  select(all_of(selected_cols))
