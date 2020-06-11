library(tidyr)
library(caret)
library(xgboost)

source("./src/prepare_data.R", encoding = "UTF-8")

SEED <- 294

xgb_train <- xgb.DMatrix(as.matrix(x_train_dummy))
xgb_test <- xgb.DMatrix(as.matrix(x_test_dummy))

fit_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

set.seed(SEED)
xgb_model <- train(
  x = xgb_train, 
  y = y_train, 
  method = "xgbTree", 
  trControl = fit_control
)

vars <- colnames(x_train_dummy)
all_vars <- tibble(id = 1:length(vars), var = vars)

var_imp <- varImp(xgb_model, scale = F)
imp_data <- tibble(
  id = 1:nrow(var_imp$importance), 
  value = var_imp$importance$Overall
)

imp_data <- left_join(imp_data, all_vars, by = "id")



fitted_xgb <- predict(xgb_model, xgb_train)

pred_xgb <- predict(xgb_model, xgb_test)

pred_xgb <- test_id %>%
  filter(TO_PREDICT) %>%
  mutate(
    NU_NOTA_MT = pred_xgb
  ) %>%
  select(-TO_PREDICT)

filled_data <- test_id %>%
  filter(!TO_PREDICT) %>%
  mutate(
    NU_NOTA_MT = NA
  ) %>%
  select(-TO_PREDICT)

answer <- bind_rows(pred_xgb, filled_data)

write_csv(answer, path = "./data/xgb.csv")
