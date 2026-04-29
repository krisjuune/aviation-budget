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

if (exists("snakemake")) {
  controls_file <- snakemake@input[["controls"]]
  fair_file     <- snakemake@input[["fair"]]
  plot_out        <- snakemake@output[["covariates_plot"]]
  main_text_size  <- snakemake@config[["main_text_size"]]
  hline_linewidth <- snakemake@config[["hline_linewidth"]]
  line_linewidth  <- snakemake@config[["line_linewidth"]]
} else {
  controls_file   <- here("data", "wtc_wtp_controls_tidy.csv")
  fair_file       <- here("data", "wtc_wtp_fair_tidy.csv")
  plot_out        <- here("output", "plot_covariates.png")
  main_text_size  <- 14
  hline_linewidth <- 0.3
  line_linewidth  <- 1
}

data_controls <- read_csv(controls_file, show_col_types = FALSE) |>
  filter(!is.na(wtc)) |>
  mutate(
    income_group = case_when(
      income_decile %in% 1:3 ~ "low",
      income_decile %in% 4:7 ~ "mid",
      income_decile %in% 8:10 ~ "high",
      TRUE ~ NA_character_
    ),
    income_group = factor(income_group, levels = c("low", "mid", "high")),
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

data_fair <- read_csv(fair_file, show_col_types = FALSE)

fair_vars <- data_fair |>
  select(id, fair_group_wtp, fair_self_wtp, fair_group_wtc, fair_self_wtc) |>
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
  title = NULL
) {
  plots <- lapply(seq_along(covariates), function(i) {
    var <- covariates[i]
    preds <- ggpredict(model, terms = var) |> as.data.frame()
    x_lab <- if (!is.null(x_labels) && var %in% names(x_labels)) {
      x_labels[[var]]
    } else {
      var
    }
    y_lab <- if (i == 1) response_label else NULL
    p <- ggplot(preds, aes(x = x, y = predicted)) +
      geom_line(colour = "#3B4CC0", linewidth = line_linewidth) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#AAB0FF") +
      geom_hline(yintercept = 2.5, linetype = "dashed", linewidth = hline_linewidth) +
      theme_classic() +
      labs(x = x_lab, y = y_lab) +
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
  wrap_plots(plots, nrow = 1) + plot_annotation(title = title)
}

plot_wtp_covariates <- plot_covariate_effects(
  model_wtp,
  covariates_wtp,
  x_labels = covariate_labels_wtp,
  response_label = "Predicted WTP",
  title = "A. Predicted willingness to pay"
)

plot_wtc_covariates <- plot_covariate_effects(
  model_wtc,
  covariates_wtc,
  x_labels = covariate_labels_wtc,
  response_label = "Predicted WTC",
  title = "B. Predicted willingness to change"
)

title_wtp <- wrap_elements(
  grid::textGrob(
    "A. Predicted willingness to pay",
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)

title_wtc <- wrap_elements(
  grid::textGrob(
    "B. Predicted willingness to change",
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

ggsave(plot = plot_covariates, plot_out, height = 7, width = 14)