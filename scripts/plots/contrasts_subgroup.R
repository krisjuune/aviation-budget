library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)

contr_wtp_income <- read_csv(
  here("data", "contr_wtp_income.csv"), show_col_types = FALSE
) |>
  mutate(income_group = factor(income_group, levels = c("low", "mid", "high")))

contr_wtc_income <- read_csv(
  here("data", "contr_wtc_income.csv"), show_col_types = FALSE
) |>
  mutate(income_group = factor(income_group, levels = c("low", "mid", "high")))

contr_delta_income <- read_csv(
  here("data", "contr_flights_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"))
  )


contr_wtp_flyer <- read_csv(
  here("data", "contr_wtp_flyer.csv"), show_col_types = FALSE
) |>
  mutate(flying_group = factor(flying_group, levels = c("non-flyer", "average flyer", "frequent flyer")))

contr_wtc_flyer <- read_csv(
  here("data", "contr_wtc_flyer.csv"), show_col_types = FALSE
) |>
  mutate(flying_group = factor(flying_group, levels = c("non-flyer", "average flyer", "frequent flyer")))

contr_delta_flyer <- read_csv(
  here("data", "contr_flights_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(flying_group, levels = c("non-flyer", "average flyer", "frequent flyer"))
  )


contr_wtp_clim <- read_csv(
  here("data", "contr_wtp_clim.csv"), show_col_types = FALSE
) |>
  mutate(clim_concern = factor(clim_concern, levels = c("low", "mid", "high")))

contr_wtc_clim <- read_csv(
  here("data", "contr_wtc_clim.csv"), show_col_types = FALSE
) |>
  mutate(clim_concern = factor(clim_concern, levels = c("low", "mid", "high")))

contr_delta_clim <- read_csv(
  here("data", "contr_flights_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"))
  )


plot_contrasts_subgroup <- function(
  contr,
  by,
  legend_title = "Income group",
  main_text_size = 14,
  alpha = 1
) {
  data <- as.data.frame(contr)
  by_sym <- ensym(by)

  ggplot(data, aes(
    x = contrast,
    y = estimate,
    color = !!by_sym,
    group = !!by_sym
  )) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * SE,
        ymax = estimate + 1.96 * SE
      ),
      width = 0.2,
      position = position_dodge(0.3),
      alpha = alpha
    ) +
    labs(
      y = "Difference vs control",
      x = "Treatment",
      color = legend_title
    ) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}



plot_contr_wtp_income <- plot_contrasts_subgroup(contr_wtp_income, by = income_group) +
  labs(title = "A. Willingness to pay for SAFs")
plot_contr_wtc_income <- plot_contrasts_subgroup(contr_wtc_income, by = income_group) +
  labs(title = "B. Willingness to reduce flying")
plot_contr_delta_income <- plot_contrasts_subgroup(contr_delta_income, by = income_group) +
  labs(title = "C. Change in planned flights")

plot_contr_income <- (
  plot_contr_wtp_income / plot_contr_wtc_income / plot_contr_delta_income
) +
  plot_layout(guides = "collect", axis_titles = "collect")


plot_contr_wtp_flyer <- plot_contrasts_subgroup(contr_wtp_flyer, by = flying_group, legend_title = "Flying behaviour") +
  labs(title = "A. Willingness to pay for SAFs")
plot_contr_wtc_flyer <- plot_contrasts_subgroup(contr_wtc_flyer, by = flying_group, legend_title = "Flying behaviour") +
  labs(title = "B. Willingness to reduce flying")
plot_contr_delta_flyer <- plot_contrasts_subgroup(contr_delta_flyer, by = flying_group, legend_title = "Flying behaviour") +
  labs(title = "C. Change in planned flights")

plot_contr_flyer <- (
  plot_contr_wtp_flyer / plot_contr_wtc_flyer / plot_contr_delta_flyer
) +
  plot_layout(guides = "collect", axis_titles = "collect")


plot_contr_wtp_clim <- plot_contrasts_subgroup(contr_wtp_clim, by = clim_concern, legend_title = "Climate concern") +
  labs(title = "A. Willingness to pay for SAFs")
plot_contr_wtc_clim <- plot_contrasts_subgroup(contr_wtc_clim, by = clim_concern, legend_title = "Climate concern") +
  labs(title = "B. Willingness to reduce flying")
plot_contr_delta_clim <- plot_contrasts_subgroup(contr_delta_clim, by = clim_concern, legend_title = "Climate concern") +
  labs(title = "C. Change in planned flights")

plot_contr_clim <- (
  plot_contr_wtp_clim / plot_contr_wtc_clim / plot_contr_delta_clim
) +
  plot_layout(guides = "collect", axis_titles = "collect")


ggsave(
  plot = plot_contr_income,
  here("output", "plot_contr_income.png"),
  height = 10, width = 10
)

ggsave(
  plot = plot_contr_flyer,
  here("output", "plot_contr_flyer.png"),
  height = 10, width = 10
)

ggsave(
  plot = plot_contr_clim,
  here("output", "plot_contr_clim.png"),
  height = 10, width = 10
)
