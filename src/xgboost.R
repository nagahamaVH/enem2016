library(xgboost)
library(readr)

source("./src/prepare_data.R", encoding = "UTF-8")

xgb_train <- xgb.DMatrix(as.matrix(x_train_dummy), label = y_train)
xgb_test <- xgb.DMatrix(as.matrix(x_test_dummy))

default_param <- list(
  objective = "reg:linear",
  booster = "gbtree"
)

xgb_model <- xgb.train(data = xgb_train, params = default_param, nrounds = 180)

y_hat <- predict(xgb_model, xgb_test)

answer <- raw_test %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    NU_NOTA_MT = ifelse(tp0, NA, y_hat)
  ) %>%
  select(NU_INSCRICAO, NU_NOTA_MT)

write_csv(answer, path = "./data/xgb.csv")
