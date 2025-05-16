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

################## other variables ####################

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
