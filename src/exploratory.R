library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggforce)
library(DataExplorer)
library(caret)

train <- read_csv("./data/train.csv", col_types = cols())

train_col_types <- spec(train)
test <- read_csv("./data/test.csv", col_types = train_col_types)

col_train <- names(train)
col_test <- names(test)

# Selecting variables in both dataset
intersect_vars <- intersect(col_train, col_test) %>%
  c("NU_NOTA_MT", "TP_PRESENCA_MT")

train <- train %>%
  select(all_of(intersect_vars)) %>%
  mutate(id = 1:n())

plot_missing(train, missing_only = T)
plot_missing(test, missing_only = T)

# Removing useless variables
useless_vars <- c("SG_UF_RESIDENCIA", "NU_INSCRICAO")

train <- train %>%
  select(-all_of(useless_vars))

# Distribution of NU_NOTA_MT ----
ggplot(aes(x = NU_NOTA_MT), data = train) +
  geom_histogram()

names(train) %>%
  sort()

char_var <- c("Q001", "Q002", "Q006", "Q024", "Q025", "Q026", "Q027", "Q047")

train_clean <- train %>%
  filter(NU_NOTA_MT > 0 & !is.na(NU_NOTA_MT)) %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(-TP_PRESENCA_MT)

ggplot(aes(x = NU_NOTA_MT), data = train_clean) +
  geom_histogram()

# Understanding NU_NOTA_MT NA, 0 ----
# TP_0 == 0 related with NU_NOTA_MT = NA. The student didn't make the test or
# no show
train %>%
  mutate(
    tp0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    tp2 = TP_PRESENCA_CH == 2 | TP_PRESENCA_CN == 2 | TP_PRESENCA_LC == 2
  ) %>%
  select(NU_NOTA_MT, tp0, tp2, TP_PRESENCA_CH, TP_PRESENCA_CN, 
         TP_PRESENCA_LC) %>%
  View()

train %>%
  select(NU_NOTA_MT, TP_PRESENCA_MT) %>%
  View()

# Exploring relation between covariables and response variable ----
quantitative <- train_clean %>%
  select_if(is.numeric) %>%
  select(-id)

categorical <- train_clean %>%
  select_if(is.character) %>%
  bind_cols(select(train_clean, NU_NOTA_MT), .)

n_row_quant <- 3
n_col_quant <- 3

n_row_cat <- 4
n_col_cat <- 4

# Tidy data to make multiple plots
quantitative_pivot <- quantitative %>%
  pivot_longer(-NU_NOTA_MT, names_to = "var", values_to = "value") %>%
  arrange(var)

categorical_pivot <- categorical %>%
  pivot_longer(-NU_NOTA_MT, names_to = "var", values_to = "value") %>%
  arrange(var)

n_vars_quantitative <- unique(quantitative_pivot$var) %>%
  length()

n_pages_quantitative <- ceiling(
  n_vars_quantitative / (n_row_quant * n_col_quant)
)

n_vars_categorical <- unique(categorical_pivot$var) %>%
  length()

n_pages_categorical <- ceiling(n_vars_categorical / (n_col_cat * n_row_cat))

for (i in 1:n_pages_quantitative) {
  ggplot(aes(x = value, y = NU_NOTA_MT), data = quantitative_pivot) +
    geom_point() +
    geom_smooth(se = F) +
    labs(x = "", y = "Nota da prova de matemática") +
    facet_wrap_paginate(~var, scales = "free", ncol = n_col_quant, 
                        nrow = n_row_quant, page = i) +
    theme_bw()
  ggsave(paste0("./images/quant-grid-", i, ".png"), width = 10, height = 10)
}

for (i in 1:n_pages_categorical) {
  ggplot(
    aes(x = reorder(value, NU_NOTA_MT, .fun = median), y = NU_NOTA_MT),
    data = categorical_pivot
  ) +
    geom_boxplot() +
    labs(x = "", y = "Nota da prova de matemática") +
    scale_x_discrete(labels = NULL) +
    facet_wrap_paginate(~var, scales = "free", ncol = n_col_cat, 
                        nrow = n_row_cat, page = i) +
    theme_bw()
  ggsave(paste0("./images/cat-grid-", i, ".png"), width = 10, height = 10)
}

# Removing outliers and single level variables
train_clean <- train_clean %>%
  filter(NU_NOTA_CH > 0 & NU_NOTA_CN > 0) %>%
  select(-c(Q026, TP_PRESENCA_LC, IN_CEGUEIRA))

# Transforming response variable ----
bc <- BoxCoxTrans(train_clean$NU_NOTA_MT)
train_bc <- train_clean %>%
  mutate(NU_NOTA_MT = predict(bc, NU_NOTA_MT))

# Exploring relation between covariables and response variable ----
quantitative <- train_bc %>%
  select_if(is.numeric) %>%
  select(-id)

categorical <- train_bc %>%
  select_if(is.character) %>%
  bind_cols(select(train_bc, NU_NOTA_MT), .)

n_row_quant <- 3
n_col_quant <- 3

n_row_cat <- 4
n_col_cat <- 4

# Tidy data to make multiple plots
quantitative_pivot <- quantitative %>%
  pivot_longer(-NU_NOTA_MT, names_to = "var", values_to = "value") %>%
  arrange(var)

categorical_pivot <- categorical %>%
  pivot_longer(-NU_NOTA_MT, names_to = "var", values_to = "value") %>%
  arrange(var)

n_vars_quantitative <- unique(quantitative_pivot$var) %>%
  length()

n_pages_quantitative <- ceiling(
  n_vars_quantitative / (n_row_quant * n_col_quant)
)

n_vars_categorical <- unique(categorical_pivot$var) %>%
  length()

n_pages_categorical <- ceiling(n_vars_categorical / (n_col_cat * n_row_cat))

for (i in 1:n_pages_quantitative) {
  ggplot(aes(x = value, y = NU_NOTA_MT), data = quantitative_pivot) +
    geom_point() +
    geom_smooth(se = F) +
    labs(x = "", y = "Nota transformada (Box Cox)") +
    facet_wrap_paginate(~var, scales = "free", ncol = n_col_quant, 
                        nrow = n_row_quant, page = i) +
    theme_bw()
  ggsave(paste0("./images/quant-grid-bc-", i, ".png"), width = 10, height = 10)
}

for (i in 1:n_pages_categorical) {
  ggplot(
    aes(x = reorder(value, NU_NOTA_MT, .fun = median), y = NU_NOTA_MT),
    data = categorical_pivot
  ) +
    geom_boxplot() +
    labs(x = "", y = "Nota transformada (Box Cox)") +
    scale_x_discrete(labels = NULL) +
    facet_wrap_paginate(~var, scales = "free", ncol = n_col_cat, 
                        nrow = n_row_cat, page = i) +
    theme_bw()
  ggsave(paste0("./images/cat-grid-bc-", i, ".png"), width = 10, height = 10)
}
