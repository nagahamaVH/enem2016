library(dplyr)
library(tidyr)
library(readr)
library(caret)
source("./src/utils.R", encoding = "UTF-8")

raw_train <- read_csv("./data/train.csv", col_types = cols())

train_col_types <- spec(raw_train)

raw_test <- read_csv("./data/test.csv", col_types = train_col_types)

col_train <- names(raw_train)
col_test <- names(raw_test)

intersect_vars <- intersect(col_train, col_test) %>%
  c("NU_NOTA_MT")

useless_vars <- c("SG_UF_RESIDENCIA", "NU_INSCRICAO", "Q026", "TP_PRESENCA_LC", 
                  "IN_CEGUEIRA")

char_var <- c("Q001", "Q002", "Q006", "Q024", "Q025", "Q026", "Q027", "Q047")

train <- raw_train %>%
  filter(NU_NOTA_MT > 0 & !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN)) %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(all_of(intersect_vars)) %>%
  select(-all_of(useless_vars))

test <- raw_test %>%
  filter(!is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_REDACAO) &
           !is.na(NU_NOTA_REDACAO)) %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(-all_of(useless_vars))

test_id <- raw_test %>%
  mutate(
    TO_PREDICT = ifelse(
      !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_REDACAO) &
        !is.na(NU_NOTA_REDACAO),
      T,
      F
    )
  ) %>%
  select(NU_INSCRICAO, TO_PREDICT)

n_train <- nrow(train)
n_test <- nrow(test)

all_data <- bind_rows(train, test)

# Data imputation for missing vars ----
inpute_vars <- c("Q027", "TP_DEPENDENCIA_ADM_ESC", "TP_ENSINO")

all_data <- all_data %>%
  mutate_at(inpute_vars, create_na_level)

# Transforming data for predictive model ----
quantitative <- all_data %>%
  select_if(is.numeric) %>%
  select(-NU_NOTA_MT)

categorical <- all_data %>%
  select_if(is.character) %>%
  mutate_all(as.factor) %>%
  select(select_vars_multiple_levels(.))

pre_quantitative <- preProcess(quantitative, method = c("center", "scale"))
pre_categorical <- dummyVars(~., data = categorical)

quantitative_norm <- predict(pre_quantitative, quantitative)

# Splitting data in train and test ----
train_quantitative <- head(quantitative_norm, n_train)
test_quantitative <- tail(quantitative_norm, n_test)

categorical_dummy <- predict(pre_categorical, categorical) %>%
  as_tibble()

train_categorical <- head(categorical, n_train)
test_categorical <- tail(categorical, n_test)

train_categorical_dummy <- head(categorical_dummy, n_train)
test_categorical_dummy <- tail(categorical_dummy, n_test)

x_train <- bind_cols(train_quantitative, train_categorical)
x_test <- bind_cols(test_quantitative, test_categorical)

x_train_dummy <- bind_cols(train_quantitative, train_categorical_dummy)
x_test_dummy <- bind_cols(test_quantitative, test_categorical_dummy)

y_train <- train$NU_NOTA_MT

bc <- BoxCoxTrans(train$NU_NOTA_MT)
y_train_bc <- predict(bc, train$NU_NOTA_MT)
