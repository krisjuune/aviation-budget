library(lme4)
library(emmeans)
library(ggeffects)
library(performance)
library(dplyr)
library(ggplot2)
library(ggridges)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
source(here("scripts", "lmm_simple_models.R"))

main_text_size <- 14

data_controls <- read_csv(
  here("data", "wtc_wtp_controls_tidy.csv"), show_col_types = FALSE
) |>
  filter(!is.na(wtc)) |>
  mutate(
    income_group = case_when(
      income_decile %in% 1:3 ~ "low",
      income_decile %in% 4:7 ~ "mid",
      income_decile %in% 8:10 ~ "high",
      TRUE ~ NA_character_ 
    ),
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high")
    ),
    flying_group = case_when(
      flying_recent == "no" ~ "non-flyer",
      flying_recent_number <= 6 ~ "average flyer",
      flying_recent_number > 6 ~ "frequent flyer",
      TRUE ~ NA_character_
    ),
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer")
    ),
    treatment = factor(treatment) |> fct_relevel("control"),
    time = factor(time, levels = c("pre", "post"))
  )


# adding additional covariates

model_wtc <- lmer(
  wtc ~ treatment * red_amt +
    clim_concern_score +
    income_decile +
    flying_recent_number +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * red_amt +
    clim_concern_score +
    income_decile +
    flying_recent_number +
    (1 | country),
  data = data_controls
)

summary(model_wtc)
summary(model_wtp)

covariates <- c("clim_concern_score", "income_decile", "flying_recent_number")

plot_covariate_effects <- function(
  model,
  covariates,
  response_label = "Predicted response",
  title = "Marginal effects of covariates",
  main_text_size = 14
) {
  preds_all <- covariates |>
    lapply(\(var) ggpredict(model, terms = var) |> 
             as.data.frame() |> 
             mutate(variable = var)) |>
    bind_rows()

  ggplot(preds_all, aes(x = x, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    facet_wrap(~ variable, scales = "free_x") +
    theme_classic() +
    labs(
      x = "Value of covariate",
      y = response_label,
      title = title
    ) +
    # scale_y_continuous(limits = c(-0.8, 5.4)) +
    theme(text = element_text(size = main_text_size))
}

plot_wtc_covariates <- plot_covariate_effects(
  model_wtc,
  covariates,
  response_label = "Predicted willingness to reduce flying"
)
plot_wtp_covariates <- plot_covariate_effects(
  model_wtp,
  covariates,
  response_label = "Predicted willingness to pay for SAFs"
)

emm <- emmeans(model_wtc, ~ red_amt * treatment)
plot_emmeans(emm, plot_30 = FALSE)
emm <- emmeans(model_wtp, ~ red_amt * treatment)
plot_emmeans(emm, plot_30 = FALSE)

ggsave(
  plot = plot_wtc_covariates,
  here("output", "plot_wtc_covariates.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_wtp_covariates,
  here("output", "plot_wtp_covariates.png"),
  height = 6, width = 10
)

# income and flying behaviour correlation

# filter missing
data_controls_num <- data_controls |>
  filter(!is.na(income_decile)) |>
  mutate(
    flying_recent_number = replace_na(flying_recent_number, 0),
    country = recode(
      country,
      "ch" = "Switzerland",
      "cn" = "China",
      "us" = "The United States"
    )
  )

corr_income_flying <- data_controls_num |>
  group_by(country) |>
  summarise(
    spearman_rho = cor(income_decile, flying_recent_number, method = "spearman")
  )

country_income_table <- data_controls_num |>
  count(country, income_decile)

plot_income_flying <- ggplot(data_controls_num, aes(
  x = flying_recent_number,
  y = factor(income_decile),
  fill = factor(income_decile)
)) +
  geom_density_ridges(alpha = 0.6, scale = 0.9) +
  facet_wrap(~country, nrow = 1) +
  coord_cartesian(xlim = c(0, 25)) +
  labs(
    x = "Number of flights (last 12 months)",
    y = "Income decile"
  ) +
  theme_classic() +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  theme(
    text = element_text(size = main_text_size),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

write_csv(corr_income_flying, here("data", "corr_income_flying.csv"))
ggsave(
  plot = plot_income_flying,
  here("output", "plot_income_flying.png"),
  height = 10, width = 10
)
