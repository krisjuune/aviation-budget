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

if (exists("snakemake")) {
  emm_wtc_file  <- snakemake@input[["emm_wtc"]]
  emm_wtp_file  <- snakemake@input[["emm_wtp"]]
  contr_wtc_file  <- snakemake@input[["contr_wtc"]]
  contr_wtp_file  <- snakemake@input[["contr_wtp"]]
  ranef_wtc_file  <- snakemake@input[["ranef_wtc"]]
  ranef_wtp_file  <- snakemake@input[["ranef_wtp"]]
  plot_out           <- snakemake@output[["overall_plot"]]
  country_plot_out   <- snakemake@output[["country_plot"]]
  main_text_size     <- snakemake@config[["main_text_size"]]
  point_size         <- snakemake@config[["point_size"]]
  errorbar_linewidth <- snakemake@config[["errorbar_linewidth"]]
  hline_linewidth    <- snakemake@config[["hline_linewidth"]]
  main_colour        <- snakemake@config[["main_colour"]]
} else {
  emm_wtc_file       <- here("data", "emm_wtc.csv")
  emm_wtp_file       <- here("data", "emm_wtp.csv")
  contr_wtc_file     <- here("data", "contr_wtc.csv")
  contr_wtp_file     <- here("data", "contr_wtp.csv")
  ranef_wtc_file     <- here("data", "ranef_wtc.csv")
  ranef_wtp_file     <- here("data", "ranef_wtp.csv")
  plot_out           <- here("output", "plot_overall_results.png")
  country_plot_out   <- here("output", "plot_country_intercepts.png")
  main_text_size     <- 14
  point_size         <- 3
  errorbar_linewidth <- 0.2
  hline_linewidth    <- 0.3
  main_colour        <- "#3B4CC0"
}

standardize_emm_columns <- function(emm_df) {
  if ("lower.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.LCL = lower.CL)
  }
  if ("upper.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.UCL = upper.CL)
  }
  return(emm_df)
}

theme_main <- function() {
  theme_classic() +
    theme(
      text = element_text(size = main_text_size),
      plot.title = element_text(face = "bold")
    )
}

emm_wtc <- read_csv(emm_wtc_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp <- read_csv(emm_wtp_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee"   = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"        = "Prioritarianism",
        "Flying fee"         = "Proportionalism"
      )
  )

contr_wtc <- read_csv(contr_wtc_file, show_col_types = FALSE) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp <- read_csv(contr_wtp_file, show_col_types = FALSE) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

ranef_wtc <- read_csv(ranef_wtc_file, show_col_types = FALSE)
ranef_wtp <- read_csv(ranef_wtp_file, show_col_types = FALSE)

############# overall plots #################

plot_emmeans_overall <- function(emm, title, shape = 16) {
  ymin_col <- if ("asymp.LCL" %in% names(emm)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(emm)) "asymp.UCL" else "upper.CL"
  ggplot(emm, aes(x = treatment, y = emmean, group = 1)) +
    geom_hline(yintercept = 2.5, linetype = 2, colour = "gray40", linewidth = hline_linewidth) +
    geom_errorbar(
      aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
      width = 0.08, linewidth = errorbar_linewidth, colour = main_colour
    ) +
    geom_point(size = point_size, shape = shape, colour = main_colour) +
    coord_cartesian(ylim = c(1.4, 3.6)) +
    labs(title = title, y = "Marginal means", x = NULL) +
    theme_main()
}

plot_contrasts_overall <- function(contr, title, shape = 16) {
  ggplot(contr, aes(x = contrast, y = estimate, group = 1)) +
    geom_hline(yintercept = 0, linetype = 2, colour = "gray40", linewidth = hline_linewidth) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * SE, ymax = estimate + 1.96 * SE),
      width = 0.08, linewidth = errorbar_linewidth, colour = main_colour
    ) +
    geom_point(size = point_size, shape = shape, colour = main_colour) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(title = title, y = "Contrast with control", x = NULL) +
    theme_main()
}

title_wtp <- "A. Effect of surcharge designs on willingness to pay"
title_wtc <- "B. Effect of budget designs on willingness to change"

plot_overall <- (
  plot_emmeans_overall(emm_wtp, title_wtp, shape = 16) |
  plot_contrasts_overall(contr_wtp, title = NULL, shape = 16)
) / (
  plot_emmeans_overall(emm_wtc, title_wtc, shape = 17) |
  plot_contrasts_overall(contr_wtc, title = NULL, shape = 17)
) +
  plot_layout(axis_titles = "collect")

ggsave(plot = plot_overall, plot_out, height = 8, width = 15)

############# country intercepts plot #################

ranef_combined <- bind_rows(
  ranef_wtc |> mutate(response = "Willingness to change (WTC)"),
  ranef_wtp |> mutate(response = "Willingness to pay (WTP)")
) |>
  mutate(
    country = case_match(
      country,
      "ch" ~ "Switzerland",
      "cn" ~ "China",
      "us" ~ "United States"
    ),
    response = factor(
      response,
      levels = c("Willingness to pay (WTP)", "Willingness to change (WTC)")
    )
  )

plot_country_intercepts <- ggplot(
  ranef_combined,
  aes(x = country, y = country_mean, shape = response)
) +
  geom_hline(
    yintercept = 2.5, linetype = 2, colour = "gray40",
    linewidth = hline_linewidth
  ) +
  geom_errorbar(
    aes(
      ymin = country_mean - 1.96 * condsd,
      ymax = country_mean + 1.96 * condsd
    ),
    width = 0.08, linewidth = errorbar_linewidth, colour = main_colour
  ) +
  geom_point(size = point_size, colour = main_colour) +
  scale_shape_manual(
    values = c(
      "Willingness to pay (WTP)"    = 16,
      "Willingness to change (WTC)" = 17
    ),
    guide = "none"
  ) +
  coord_cartesian(ylim = c(1.5, 3.5)) +
  facet_wrap(~response) +
  labs(
    x = NULL,
    y = "Country mean for control group"
  ) +
  theme_main() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  plot = plot_country_intercepts,
  country_plot_out,
  height = 5, width = 9
)