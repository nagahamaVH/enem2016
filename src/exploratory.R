library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggforce)
library(gridExtra)
library(DataExplorer)
library(caret)

train_raw <- read_csv("./data/train.csv", col_types = cols())

train_col_types <- spec(train_raw)
test <- read_csv("./data/test.csv", col_types = train_col_types)

col_train <- names(train_raw)
col_test <- names(test)

# Selecting variables in both dataset
intersect_vars <- intersect(col_train, col_test) %>%
  c("NU_NOTA_MT", "TP_PRESENCA_MT")

train_raw <- train_raw %>%
  select(all_of(intersect_vars))

missing_train <- plot_missing(train_raw, missing_only = T) +
  labs(y = "Linhas faltantes", x = "Variáveis", title = "Treino") +
  theme_gray() + 
  theme(legend.position = "none")

missing_test <- plot_missing(test, missing_only = T) +
  labs(y = "Linhas faltantes", x = "Variáveis", title = "Teste") +
  theme_gray() +
  theme(legend.position = "none")

plot_missing <- grid.arrange(missing_train, missing_test, nrow = 2)
ggsave("./images/missing-data.png", plot = plot_missing, units = 'cm', 
       width = 26, height = 18)

# Removing useless variables
useless_vars <- c("SG_UF_RESIDENCIA", "NU_INSCRICAO")

train_raw <- train_raw %>%
  select(-all_of(useless_vars))

# Distribution of NU_NOTA_MT ----
ggplot(aes(x = NU_NOTA_MT), data = train_raw) +
  geom_histogram()

train_raw %>%
  count(zero_score = NU_NOTA_MT == 0)

names(train_raw) %>%
  sort()

char_var <- c("Q001", "Q002", "Q006", "Q024", "Q025", "Q026", "Q027", "Q047")

# Removing outliers and transforming data types
train <- train_raw %>%
  filter(NU_NOTA_MT > 0) %>%
  mutate_at(vars(starts_with("IN")), as.character) %>%
  mutate_at(vars(starts_with("TP")), as.character) %>%
  mutate_at(vars(starts_with("CO")), as.character) %>%
  mutate_at(char_var, as.character) %>%
  select(-TP_PRESENCA_MT)

ggplot(aes(x = NU_NOTA_MT), data = train) +
  geom_histogram()

# Understanding NU_NOTA_MT NA, 0 and score NA for all tests ----
mt_status <- train_raw %>%
  mutate(
    MT_STATUS = case_when(
      is.na(NU_NOTA_MT) ~ "NA",
      NU_NOTA_MT == 0 ~ "0",
      NU_NOTA_MT > 0 ~ ">0"
    ),
    CH_STATUS = case_when(
      is.na(NU_NOTA_CH) ~ "NA",
      NU_NOTA_CH == 0 ~ "0",
      NU_NOTA_CH > 0 ~ ">0"
    ),
    CN_STATUS = case_when(
      is.na(NU_NOTA_CN) ~ "NA",
      NU_NOTA_CN == 0 ~ "0",
      NU_NOTA_CN > 0 ~ ">0"
    ),
    TP0 = TP_PRESENCA_CH == 0 | TP_PRESENCA_CN == 0 | TP_PRESENCA_LC == 0,
    TP2 = TP_PRESENCA_CH == 2 | TP_PRESENCA_CN == 2 | TP_PRESENCA_LC == 2
  ) %>%
  select(MT_STATUS, TP0, TP2, TP_PRESENCA_CH, TP_PRESENCA_CN, 
         TP_PRESENCA_LC, TP_PRESENCA_MT, CH_STATUS, CN_STATUS) %>%
  mutate_all(as.factor)

ggplot(aes(x = MT_STATUS, fill = TP_PRESENCA_CN), data = mt_status) +
  geom_bar(position = "fill")
# no score in MT are related with no show in CN test

ggplot(aes(x = MT_STATUS, fill = CN_STATUS), data = mt_status) +
  geom_bar(position = "fill")
# no score in MT are related with no score CN test

mt_status %>%
  filter(CN_STATUS == "NA") %>%
  count(MT_STATUS) %>%
  mutate(
    percent = n / sum(n) * 100
  )

ggplot(aes(x = MT_STATUS, fill = TP_PRESENCA_CH), data = mt_status) +
  geom_bar(position = "fill")
# no score in MT are related with no show in CH test

ggplot(aes(x = MT_STATUS, fill = CH_STATUS), data = mt_status) +
  geom_bar(position = "fill")
# no score in MT are related with no score CH test

mt_status %>%
  filter(CH_STATUS == "NA") %>%
  count(MT_STATUS) %>%
  mutate(
    percent = n / sum(n) * 100
  )

mt_status %>%
  count(CN_STATUS) %>%
  mutate(
    percent = n / sum(n) * 100
  )

ggplot(aes(x = MT_STATUS, fill = CH_STATUS), data = mt_status) +
  geom_bar(position = "fill")
# students desqualifed are those who has 0 score in test 

ggplot(aes(x = MT_STATUS, fill = TP0), data = mt_status) +
  geom_bar(position = "fill")
# no score in MT are related with no show in any of main tests (CH, CN, LC)

mt_status %>%
  filter(TP0 == T) %>%
  count(MT_STATUS) %>%
  mutate(
    percent = n / sum(n) * 100
  )

ggplot(aes(x = MT_STATUS, fill = TP2), data = mt_status) +
  geom_bar(position = "fill")

# Exploring relation between covariables and response variable ----
quantitative <- train %>%
  select_if(is.numeric)

categorical <- train %>%
  select_if(is.character) %>%
  bind_cols(select(train, NU_NOTA_MT), .)

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
  ggsave(paste0("./images/quant-grid-raw", i, ".png"), width = 10, height = 10)
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
  ggsave(paste0("./images/cat-grid-raw", i, ".png"), width = 10, height = 10)
}

# Removing outliers and single level variables
# Outliers: NU_NOTA_CH, NU_NOTA_CN, TP_PRESENCA_CH, TP_PRESENCA_CN
train_clean <- train %>%
  filter(!is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & 
           NU_NOTA_CH > 0 & NU_NOTA_CN > 0) %>%
  select(-c("IN_CEGUEIRA", "TP_PRESENCA_CH", "TP_PRESENCA_CN", 
            "TP_PRESENCA_LC"))

quantitative <- train_clean %>%
  select_if(is.numeric)

categorical <- train_clean %>%
  select_if(is.character) %>%
  bind_cols(select(train_clean, NU_NOTA_MT), .)

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
  ggsave(paste0("./images/quant-grid", i, ".png"), width = 10, height = 10)
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
  ggsave(paste0("./images/cat-grid", i, ".png"), width = 10, height = 10)
}
