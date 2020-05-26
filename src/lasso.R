library(caret)
library(glmnet)

source("./src/prepare_data.R", encoding = "UTF-8")

SEED <- 294

x_train_mm <- model.matrix(~., x_train)
x_test_mm <- model.matrix(~., x_test

set.seed(SEED)
lasso_model <- train(x = x_train_mm, y = y_train, method = "glmnet") 

y_hat <- predict(lasso_model, x_test_mm)

data_model <- tibble(
  i = 1:length(y_train),
  y_train, 
  y_hat = predict(lasso_model, x_train_mm), 
  error = y_hat - y_train,
  error_std = error / sd(error)
)

ggplot(aes(x = i, y = error_std), data = data_model) +
  geom_point()

ggplot(aes(sample = error), data = data_model) +
  geom_qq() +
  geom_qq_line()

answer <- raw_test %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    NU_NOTA_MT = ifelse(tp0, NA, reverse_bc(y_hat, bc$lambda))
  ) %>%
  select(NU_INSCRICAO, NU_NOTA_MT)

write_csv(answer, path = "./data/lasso.csv")
