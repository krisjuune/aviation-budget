library(dplyr)
library(ggplot2)
library(ggtext)
library(rlang)
library(viridis)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
library(emmeans)


standardize_emm_columns <- function(emm_df) {
  emm_df <- emm_df |>
    rename(
      asymp.LCL = case_when(
        "asymp.LCL" %in% colnames(emm_df) ~ "asymp.LCL",
        "lower.CL" %in% colnames(emm_df) ~ "lower.CL",
        TRUE ~ NA_character_
      ),
      asymp.UCL = case_when(
        "asymp.UCL" %in% colnames(emm_df) ~ "asymp.UCL",
        "upper.CL" %in% colnames(emm_df) ~ "upper.CL",
        TRUE ~ NA_character_
      )
    )
  if ("lower.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.LCL = lower.CL)
  }
  if ("upper.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.UCL = upper.CL)
  }
  return(emm_df)
}

theme_main <- function(main_text_size = 14) {
  theme_classic() +
    theme(
      text = element_text(size = main_text_size),
      plot.title = element_text(face = "bold")
    )
}

emm_wtc <- read_csv(
  here("data", "emm_wtc.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent-flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp <- read_csv(
  here("data", "emm_wtp.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee" = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee" = "Prioritarianism",
        "Flying fee" = "Proportionalism"
      )
  )

contr_wtc <- read_csv(
  here("data", "contr_wtc.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent-flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp <- read_csv(
  here("data", "contr_wtp.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee" = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee" = "Prioritarianism",
        "Flying fee" = "Proportionalism"
      )
  )

############# overall plots #################

plot_emmeans_overall <- function(emm, title, main_text_size = 14) {
  ymin_col <- if ("asymp.LCL" %in% names(emm)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(emm)) "asymp.UCL" else "upper.CL"

  ggplot(emm, aes(x = treatment, y = emmean, group = 1)) +
    geom_point(size = 1.5) +
    geom_errorbar(aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
                  width = 0.05) +
    geom_hline(yintercept = 2.5, linetype = 2, colour = "gray40", linewidth = 0.3) +
    coord_cartesian(ylim = c(0.5, 4.5)) +
    labs(title = title, y = "Marginal means", x = NULL) +
    theme_main(main_text_size)
}

plot_contrasts_overall <- function(contr, title, main_text_size = 14) {
  ggplot(contr, aes(x = contrast, y = estimate, group = 1)) +
    geom_point(size = 1.5) +
    geom_errorbar(aes(ymin = estimate - 1.96 * SE, ymax = estimate + 1.96 * SE),
                  width = 0.05) +
    geom_hline(yintercept = 0, linetype = 2, colour = "gray40", linewidth = 0.3) +
    ylim(-1.65, 1.5) +
    labs(title = title, y = "Contrast with control", x = NULL) +
    theme_main(main_text_size)
}

plot_overall <- (
  plot_emmeans_overall(emm_wtp, "A. Effect of surcharge designs on willingness to pay") |
  plot_contrasts_overall(contr_wtp, title = NULL)
) / (
  plot_emmeans_overall(emm_wtc, "B. Effect of budget designs on willingness to change") |
  plot_contrasts_overall(contr_wtc, title = NULL)
) +
  plot_layout(axis_titles = "collect")

################### save stuff #####################

ggsave(
  plot = plot_overall,
  here("output", "plot_overall_results.png"),
  height = 8, width = 15
)