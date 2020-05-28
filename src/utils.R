library(dplyr)

create_na_level <- function(x){
  ifelse(is.na(x), "NA", x)
}

as_factor_numeric_level <- function(x){
  n_levels <- unique(x) %>%
    length()
  
  factor(x, labels = 1:n_levels)
}

reverse_bc <- function(z, lambda){
  if (lambda == 0) {
    exp(z)
  } else{
    (lambda * z + 1) ^ (1 / lambda) 
  }
}

fill_missing_value <- function(x, value){
  ifelse(is.na(x), value, x)
}

# library(arules)
# 
# x <- all_data$NU_NOTA_CH
# y <- all_data$NU_NOTA_MT
# 
# parms <- expand.grid(
#   method = c("interval", "frequency", "cluster"),
#   breaks = c(3, 5, 7, 9, 11, )
# )
# 
# xgb_discretize <- function(x, y, parms, iter){
#   discretize(x, method = parms)
# 
# }
