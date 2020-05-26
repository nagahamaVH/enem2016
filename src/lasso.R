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

answer <- raw_test %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    NU_NOTA_MT = ifelse(tp0, NA, pred_lasso)
  ) %>%
  select(NU_INSCRICAO, NU_NOTA_MT)

write_csv(answer, path = "./data/lasso.csv")
