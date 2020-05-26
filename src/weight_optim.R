fitted_matrix <- tibble(fitted_lasso, fitted_xgb, y_train)

rss <- function(data, weights){
  sum(weights[1] * data[,1] + weights[2] * data[,2] - data[,3])^2
}

result <- optim(par = c(0, 0), fn = rss, data = fitted_matrix)
weights <- result$par

pred_ensemble <- weights[1] * pred_lasso + weights[2] * pred_xgb

answer <- raw_test %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    NU_NOTA_MT = ifelse(tp0, NA, pred_ensemble)
  ) %>%
  select(NU_INSCRICAO, NU_NOTA_MT)

write_csv(answer, path = "./data/ensemble.csv")
