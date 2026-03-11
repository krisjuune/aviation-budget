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
library(knitr)
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
    time = factor(time, levels = c("pre", "post")),
    income_decile = as.integer(income_decile)
  )

data_fair <- read_csv(
  here("data", "wtc_wtp_fair_tidy.csv"),
  show_col_types = FALSE
)

fair_vars <- data_fair |>
  select(
    id,
    fair_group_wtp,
    fair_self_wtp,
    fair_group_wtc,
    fair_self_wtc
  ) |>
  distinct()

data_controls <- data_controls |>
  left_join(fair_vars, by = "id")

################ model covariates ##################

model_wtc <- lmer(
  wtc ~ treatment + red_amt +
    fair_group_wtc + fair_self_wtc +
    clim_concern_score +
    income_decile +
    flying_recent_number +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment + red_amt +
    fair_group_wtp + fair_self_wtp +
    clim_concern_score +
    income_decile +
    flying_recent_number +
    relative_added_cost +
    (1 | country),
  data = data_controls
)

summary(model_wtc)
summary(model_wtp, correlation = TRUE)

covariates_wtp <- c(
  "fair_self_wtp",
  "fair_group_wtp",
  "clim_concern_score",
  "income_decile",
  "flying_recent_number"
)

covariate_labels_wtp <- c(
  fair_self_wtp = "Personal fairness score",
  fair_group_wtp = "Group fairness score",
  clim_concern_score = "Climate concern sum score",
  flying_recent_number = "Nr of flights per year",
  income_decile = "Income decile"
)

covariates_wtc <- c(
  "fair_self_wtc",
  "fair_group_wtc",
  "clim_concern_score",
  "income_decile",
  "flying_recent_number"
)

covariate_labels_wtc <- c(
  fair_self_wtc = "Personal fairness score",
  fair_group_wtc = "Group fairness score",
  clim_concern_score = "Climate concern sum score",
  flying_recent_number = "Nr of flights per year",
  income_decile = "Income decile"
)

plot_covariate_effects <- function(
  model,
  covariates,
  x_labels = NULL,
  response_label = "Predicted response",
  main_text_size = 14,
  title = NULL
) {

    plots <- lapply(seq_along(covariates), function(i) {

    var <- covariates[i]

    preds <- ggpredict(model, terms = var) |>
      as.data.frame()

    x_lab <- if (!is.null(x_labels) && var %in% names(x_labels)) {
      x_labels[[var]]
    } else {
      var
    }

    y_lab <- if (i == 1) response_label else NULL

    p <- ggplot(preds, aes(x = x, y = predicted)) +
      geom_line(colour = "#3B4CC0") +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#AAB0FF") +
      geom_hline(yintercept = 2.5, linetype = "dashed") +
      theme_classic() +
      labs(
        x = x_lab,
        y = y_lab
      ) +
      scale_x_continuous(
        breaks = function(x) {
          brks <- pretty(x, n = 5)
          brks[brks %% 1 == 0]
        }
      ) +
      ylim(.25, 4.5) +
      theme(text = element_text(size = main_text_size))

    if (i != 1) {
      p <- p + theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }

    return(p)
  })
  combined_plot <- wrap_plots(plots, nrow = 1) +
    plot_annotation(title = title)

  return(combined_plot)
}





plot_covariate_effects_bubbles <- function(
  model,
  covariates,
  raw_data = NULL,
  response_var = "wtc",
  x_labels = NULL,
  response_label = "Predicted response",
  main_text_size = 14,
  title = NULL
) {

  plots <- lapply(seq_along(covariates), function(i) {

    var <- covariates[i]

    preds <- ggpredict(model, terms = var) |>
      as.data.frame()

    x_lab <- if (!is.null(x_labels) && var %in% names(x_labels)) {
      x_labels[[var]]
    } else {
      var
    }

    y_lab <- if (i == 1) response_label else NULL

    p <- ggplot(preds, aes(x = x, y = predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
      geom_hline(yintercept = 2.5, linetype = "dashed") +
      theme_classic() +
      labs(
        x = x_lab,
        y = y_lab
      ) +
      scale_x_continuous(
        breaks = function(x) {
          brks <- pretty(x, n = 5)
          brks[brks %% 1 == 0]
        }
      ) +
      ylim(0.25, 4.5) +
      theme(text = element_text(size = main_text_size))


    # Determine if the covariate is discrete
    if (is.integer(raw_data[[var]]) || is.factor(raw_data[[var]]) || length(unique(raw_data[[var]])) <= 6) {

      # Bubble plot for discrete covariates
      df_bubbles <- raw_data %>%
        count(x = .data[[var]], y = wtc) # change wtc to wtp if plotting WTP

      p <- p + 
        geom_point(
          data = df_bubbles,
          aes(x = x, y = y, size = n),
          color = "grey40",
          alpha = 0.3
        ) +
        scale_size_area(max_size = 5)

    } else {

      # Optional: add quasirandom jitter for continuous covariates
      p <- p + geom_jitter(data = raw_data, aes_string(x = var, y = "wtc"),
                           width = 0.2, height = 0, alpha = 0.1, color = "grey50")
    }

    if (i != 1) {
      p <- p + theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }

    return(p)
  })

  combined_plot <- wrap_plots(plots, nrow = 1) +
    plot_annotation(title = title)

  return(combined_plot)
}

# plot_wtc_covariates <- plot_covariate_effects(
#   model_wtc,
#   covariates_wtc,
#   raw_data = data_controls,
#   outcome = "wtc",
#   x_labels = covariate_labels_wtc,
#   response_label = "Predicted score",
#   title = "B. Predicted willingness to change"
# )


plot_wtc_covariates <- plot_covariate_effects(
  model_wtc,
  covariates_wtc,
  x_labels = covariate_labels_wtc,
  response_label = "Predicted WTP score",
  title = "B. Predicted willingness to change"
)

plot_wtp_covariates <- plot_covariate_effects(
  model_wtp,
  covariates_wtp,
  x_labels = covariate_labels_wtp,
  response_label = "Predicted WTC score",
  title = "A. Predicted willingness to pay"
)

title_wtp <- wrap_elements(
  grid::textGrob(
    "A. Predicted willingness to pay for SAFs",
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)

title_wtc <- wrap_elements(
  grid::textGrob(
    "B. Predicted willingness to constrain own flying",
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)

plot_covariates <-
  title_wtp /
  plot_wtp_covariates /
  title_wtc /
  plot_wtc_covariates +
  plot_layout(heights = c(0.1, 1, 0.1, 1))


################### save stuff ######################
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

ggsave(
  plot = plot_covariates,
  here("output", "plot_covariates.png"),
  height = 7, width = 14
)
