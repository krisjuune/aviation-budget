library(lme4)
library(emmeans)
library(ggeffects)
library(performance)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
source(here("scripts", "lmm_simple_models.R"))

data_controls <- read_csv(here("data", "wtc_wtp_controls_tidy.csv")) |>
  filter(!is.na(wtc))

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

# subgroup analysis

data_controls <- data_controls |>
  mutate(
    income_group = case_when(
      income_decile %in% 1:3 ~ "low",
      income_decile %in% 4:7 ~ "mid",
      income_decile %in% 8:10 ~ "high",
      TRUE ~ NA_character_ 
    ),
    income_group = factor(income_group, levels = c("low", "mid", "high"))
  )

model_wtc <- lmer(
  wtc ~ treatment * income_group +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * income_group +
    (1 | country),
  data = data_controls
)

emm_wtc_income <- emmeans(model_wtc, ~ income_group * treatment)
emm_wtp_income <- emmeans(model_wtp, ~ income_group * treatment)

model_wtc <- lmer(
  wtc ~ treatment * flying_recent +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * flying_recent +
    (1 | country),
  data = data_controls
)

emm_wtc_flyer <- emmeans(model_wtc, ~ flying_recent * treatment)
emm_wtp_flyer <- emmeans(model_wtp, ~ flying_recent * treatment)

plot_emmeans_subgroup <- function(
  emm,
  by,
  legend_title = "Income group",
  main_text_size = 14,
  alpha = 1
) {
  data <- as.data.frame(emm)
  by_sym <- ensym(by)

  ymin_col <- if ("asymp.LCL" %in% names(data)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(data)) "asymp.UCL" else "upper.CL"

  ggplot(data, aes(
    x = treatment,
    y = emmean,
    color = !!by_sym,
    group = !!by_sym
  )) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = .data[[ymin_col]],
        ymax = .data[[ymax_col]]
      ),
      width = 0.2,
      position = position_dodge(0.3),
      alpha = alpha
    ) +
    labs(
      y = NULL,
      x = "Treatment",
      color = legend_title
    ) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    geom_hline(
      yintercept = 2.5,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}

plot_wtc_income <- plot_emmeans_subgroup(emm_wtc_income, by = income_group) +
  labs(title = "A. Willingness to reduce flying")
plot_wtp_income <- plot_emmeans_subgroup(emm_wtp_income, by = income_group) +
  labs(title = "B. Willingness to pay for SAFs")

plot_income_subgroups <- (plot_wtc_income / plot_wtp_income) +
  plot_layout(guides = "collect", axis_titles = "collect")

plot_wtc_flyer <- plot_emmeans_subgroup(
  emm_wtc_flyer,
  by = flying_recent,
  legend_title = "Flown in the past year"
) +
  labs(title = "A. Willingness to reduce flying")

plot_wtp_flyer <- plot_emmeans_subgroup(
  emm_wtp_flyer,
  by = flying_recent,
  legend_title = "Flown in the past year"
) +
  labs(title = "B. Willingness to pay for SAFs")

plot_flyer_subgroups <- (plot_wtc_flyer / plot_wtp_flyer) +
  plot_layout(guides = "collect", axis_titles = "collect")

#TODO add also three-level category for flying, no flights plannes, some, many

ggsave(
  plot = plot_income_subgroups,
  here("output", "plot_income_subgroups.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_flyer_subgroups,
  here("output", "plot_flyer_subgroups.png"),
  height = 6, width = 10
)
