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

select_vars_multiple_levels <- function(data){
  data_levels <- sapply(data, levels)
  
  selected_vars <- NULL
  
  for (i in 1:length(data_levels)) {
    add_i <- ifelse(length(data_levels[[i]]) > 1, names(data_levels[i]), NA)
    
    selected_vars <- c(selected_vars, add_i)
  }
  
  selected_vars <- selected_vars[!is.na(selected_vars)]
  
  return(selected_vars)
}

decode_tp_presenca <- function(x){
  case_when(
    x == 0 ~ "Faltou",
    x == 1 ~ "Presente",
    x == 2 ~ "Eliminado"
  )
}

