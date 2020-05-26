library(caret)
library(xgboost)

source("./src/prepare_data.R", encoding = "UTF-8")

SEED <- 294

xgb_train <- xgb.DMatrix(as.matrix(x_train_dummy))
xgb_test <- xgb.DMatrix(as.matrix(x_test_dummy))

set.seed(SEED)
xgb_model <- train(x = xgb_train, y = y_train, method = "xgbLinear")

y_hat <- predict(xgb_model, xgb_test)

answer <- raw_test %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    NU_NOTA_MT = ifelse(tp0, NA, y_hat)
  ) %>%
  select(NU_INSCRICAO, NU_NOTA_MT)

write_csv(answer, path = "./data/xgb.csv")
