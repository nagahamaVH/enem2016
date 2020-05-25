library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggforce)
library(DataExplorer)
library(caret)

train <- read_csv("./data/train.csv")
test <- read_csv("./data/test.csv")

col_train <- names(train)
col_test <- names(test)

# Selecting variables in both dataset
intersect_vars <- intersect(col_train, col_test) %>%
  c("NU_NOTA_MT", "TP_PRESENCA_MT")

train <- train %>%
  select(all_of(intersect_vars))

plot_missing(train, missing_only = T)

# Removing redundancy of variables
redundant_vars <- c("SG_UF_RESIDENCIA")

train <- train %>%
  select(-all_of(redundant_vars))

# Distribution of NU_NOTA_MT ----
ggplot(aes(x = NU_NOTA_MT), data = train) +
  geom_histogram()

bc <- BoxCoxTrans(train$NU_NOTA_MT)

train_clean <- train %>%
  filter(NU_NOTA_MT > 0) %>%
  mutate(
    NU_NOTA_MT_BC = predict(bc, NU_NOTA_MT)
  )

ggplot(aes(x = NU_NOTA_MT), data = train_clean) +
  geom_histogram()

ggplot(aes(x = NU_NOTA_MT_BC), data = train_clean) +
  geom_histogram()

# Understanding score NA, 0 ----
# TP_0 == 0 related with NU_NOTA_MT = NA
train %>% 
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    tp2 = TP_PRESENCA_CH == 2 | TP_PRESENCA_CN == 2 | TP_PRESENCA_LC == 2
  ) %>%
  select(NU_NOTA_MT, tp0, tp2, TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC) %>%
  View()

train %>%
  select(NU_NOTA_MT, TP_PRESENCA_MT) %>%
  View()
