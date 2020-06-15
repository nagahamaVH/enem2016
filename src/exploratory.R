library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggforce)
library(gridExtra)
library(cowplot)
library(DataExplorer)
library(caret)

source("./src/utils.R", encoding = "UTF-8")

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
  labs(y = "", x = "", title = "Treino") +
  theme_gray() +
  theme(legend.position = "none")

missing_test <- plot_missing(test, missing_only = T) +
  labs(y = "", x = "", title = "Teste") +
  theme_gray() +
  theme(legend.position = "none")

plot_missing <- plot_grid(missing_train, missing_test, nrow = 2) +
  draw_label("Variáveis", x = 0, y = 0.5, vjust = 1.2, angle = 90, size = 12) +
  draw_label("Número de observações faltantes", x = 0.5, y = 0, 
             vjust = -0.5, size = 12)
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
      is.na(NU_NOTA_MT) ~ "Missing",
      NU_NOTA_MT == 0 ~ "0",
      NU_NOTA_MT > 0 ~ ">0"
    ),
    CH_STATUS = case_when(
      is.na(NU_NOTA_CH) ~ "Missing",
      NU_NOTA_CH == 0 ~ "0",
      NU_NOTA_CH > 0 ~ ">0"
    ),
    CN_STATUS = case_when(
      is.na(NU_NOTA_CN) ~ "Missing",
      NU_NOTA_CN == 0 ~ "0",
      NU_NOTA_CN > 0 ~ ">0"
    ),
    LC_STATUS = case_when(
      is.na(NU_NOTA_LC) ~ "Missing",
      NU_NOTA_LC == 0 ~ "0",
      NU_NOTA_LC > 0 ~ ">0"
    )
  ) %>%
  mutate_at(c("TP_PRESENCA_CH", "TP_PRESENCA_CN", "TP_PRESENCA_LC"), 
            decode_tp_presenca) %>%
  select(MT_STATUS, TP_PRESENCA_CH, TP_PRESENCA_CN, 
         TP_PRESENCA_LC, TP_PRESENCA_MT, CH_STATUS, CN_STATUS, LC_STATUS) %>%
  mutate_all(as.factor)

ch_score <- ggplot(aes(x = TP_PRESENCA_CH, fill = CH_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", fill = "Nota", title = "Ciências Humanas") +
  theme_minimal_hgrid() +
  theme(legend.position = "bottom")

legend_score <- get_legend(
  ch_score #+ theme(legend.box.margin = margin(0, 0, 0, 12))
)

ch_table <- mt_status %>%
  count(TP_PRESENCA_CH) %>%
  rename(
    "Presença" = TP_PRESENCA_CH,
    "Frequência" = n
  )

cn_score <- ggplot(aes(x = TP_PRESENCA_CN, fill = CN_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", fill = "", title = "Ciências Naturais") +
  theme_minimal_hgrid()

cn_table <- mt_status %>%
  count(TP_PRESENCA_CN) %>%
  rename(
    "Presença" = TP_PRESENCA_CN,
    "Frequência" = n
  )

lc_score <- ggplot(aes(x = TP_PRESENCA_LC, fill = LC_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", title = "Linguagens e Códigos") +
  theme_minimal_hgrid()

lc_table <- mt_status %>%
  count(TP_PRESENCA_LC) %>%
  rename(
    "Presença" = TP_PRESENCA_LC,
    "Frequência" = n
  )

cn_grid <- plot_grid(
  cn_score + theme(legend.position = "none"),
  tableGrob(cn_table, rows = NULL),
  rel_widths = c(1, .3)
)

ch_grid <- plot_grid(
  ch_score + theme(legend.position = "none"),
  tableGrob(ch_table, rows = NULL),
  rel_widths = c(1, .3)
)

lc_grid <- plot_grid(
  lc_score + theme(legend.position = "none"),
  tableGrob(lc_table, rows = NULL),
  rel_widths = c(1, .3)
)

score_grid_raw <- plot_grid(
  ch_grid,
  cn_grid,
  lc_grid,
  align = "vh",
  nrow = 3
)

score_grid <- plot_grid(
  score_grid_raw,
  legend_score,
  nrow = 2,
  rel_heights = c(1, .1)
) +
  draw_label("Proporção", x = 0, y = 0.5, vjust = 1.2, angle = 90, size = 12) +
  draw_label("Presença", x = 0.5, y = 0, vjust = -0.5, size = 12)
ggsave("./images/score-grid.png", plot = score_grid, units = 'cm', 
       width = 26, height = 18)

# Crossing score between MT and predictor score
ch_cross_score <- ggplot(aes(x = CH_STATUS, fill = MT_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", fill = "Nota Matemática", title = "Ciências Humanas") +
  theme_minimal_hgrid() +
  theme(legend.position = "bottom")

legend_cross_score <- get_legend(
  ch_cross_score #+ theme(legend.box.margin = margin(0, 0, 0, 12))
)

cn_cross_score <- ggplot(aes(x = CN_STATUS, fill = MT_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", title = "Ciências Naturais") +
  theme_minimal_hgrid()

lc_cross_score <- ggplot(aes(x = LC_STATUS, fill = MT_STATUS), data = mt_status) +
  geom_bar(position = "fill") +
  labs(x = "", y = "", title = "Linguagens e Códigos") +
  theme_minimal_hgrid() +
  theme(legend.position = "none")

cross_score_grid_raw <- plot_grid(
  ch_cross_score + theme(legend.position = "none"),
  cn_cross_score + theme(legend.position = "none"),
  lc_cross_score + theme(legend.position = "none"),
  align = "vh",
  nrow = 3
)

cross_score_grid <- plot_grid(
  cross_score_grid_raw,
  legend_cross_score,
  nrow = 2,
  rel_heights = c(1, .1)
) +
  draw_label("Proporção", x = 0, y = 0.5, vjust = 1.2, angle = 90, size = 12) +
  draw_label("Nota", x = 0.5, y = 0, vjust = -0.5, size = 12)
ggsave("./images/cross-score-grid.png", plot = cross_score_grid, units = 'cm', 
       width = 26, height = 18)

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
