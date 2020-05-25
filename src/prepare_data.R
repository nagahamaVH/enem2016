library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(mice)

SEED <- 5062

train <- read_csv("./data/train.csv", col_types = cols())

train_col_types <- spec(train)

test <- read_csv("./data/test.csv", col_types = train_col_types)

col_train <- names(train)
col_test <- names(test)

intersect_vars <- intersect(col_train, col_test) %>%
  c("NU_NOTA_MT")

useless_vars <- c("SG_UF_RESIDENCIA", "NU_INSCRICAO", "Q026", "TP_PRESENCA_LC", 
                  "IN_CEGUEIRA")

char_var <- c("Q001", "Q002", "Q006", "Q024", "Q025", "Q026", "Q027", "Q047")

bc <- BoxCoxTrans(train$NU_NOTA_MT)

train <- train %>%
  filter(NU_NOTA_MT > 0 & !is.na(NU_NOTA_MT)) %>%
  filter(NU_NOTA_CH > 0 | NU_NOTA_CN > 0) %>%
  mutate(NU_NOTA_MT_BC = predict(bc, NU_NOTA_MT)) %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(all_of(intersect_vars)) %>%
  select(-all_of(useless_vars))

test <- test %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(-all_of(useless_vars))

all_data <- bind_rows(train, test)

n_train <- nrow(train)
n_test <- nrow(test)

# Data imputation ----
inputation_train <- mice(all_data, seed = SEED)
all_data <- complete(inputation_train) %>%
  as_tibble()

# Transforming data for predictive model ----
quantitative <- all_data %>%
  select_if(is.numeric) %>%
  select(-c(Id, SalePrice))

categorical <- all_data %>%
  select_if(is.character) %>%
  mutate_all(as.factor)

pre_quantitative <- preProcess(quantitative, method = c("center", "scale"))

#pre_categorical <- dummyVars(~., data = categorical)

quantitative_norm <- predict(pre_quantitative, quantitative)
# categorical_train_norm <- predict(pre_categorical, categorical) %>%
#   as_tibble()

train_quantitative <- head(quantitative_norm, n_train)
train_categorical <- head(categorical, n_train)

test_quantitative <- tail(quantitative_norm, n_test)
test_categorical <- tail(categorical, n_test)

x_train <- bind_cols(train_quantitative, train_categorical)
#x_train_dummy <- bind_cols(quantitative_train_norm, categorical_train_norm)

x_test <- bind_cols(test_quantitative, test_categorical)
#x_test_dummy <- bind_cols(quantitative_test_norm, categorical_test_norm)

