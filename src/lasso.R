library(caret)
library(glmnet)

source("./src/prepare_data.R", encoding = "UTF-8")

SEED <- 294

x_train_mm <- model.matrix(~., x_train)
x_test_mm <- model.matrix(~., x_test)

fit_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

set.seed(SEED)
lasso_model <- train(
  x = x_train_mm, 
  y = y_train_bc, 
  method = "glmnet",
  trControl = fit_control
)

fitted_lasso <- predict(lasso_model, x_train_mm) %>%
  reverse_bc(., bc$lambda)

pred_lasso <- predict(lasso_model, x_test_mm) %>%
  reverse_bc(., bc$lambda)

pred_lasso <- test_id %>%
  filter(TO_PREDICT) %>%
  mutate(
    NU_NOTA_MT = pred_lasso
  ) %>%
  select(-TO_PREDICT)

filled_data <- test_id %>%
  filter(!TO_PREDICT) %>%
  mutate(
    NU_NOTA_MT = NA
  ) %>%
  select(-TO_PREDICT)

answer <- bind_rows(pred_lasso, filled_data)

write_csv(answer, path = "./data/lasso.csv")
