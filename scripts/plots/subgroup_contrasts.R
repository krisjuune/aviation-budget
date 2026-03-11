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
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtc_income <- read_csv(
  here("data", "contr_wtc_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtp_flyer <- read_csv(
  here("data", "contr_wtp_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtc_flyer <- read_csv(
  here("data", "contr_wtc_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtp_clim <- read_csv(
  here("data", "contr_wtp_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtc_clim <- read_csv(
  here("data", "contr_wtc_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$")
  )


contr_delta_income <- read_csv(
  here("data", "contr_flights_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"))
  )

contr_delta_flyer <- read_csv(
  here("data", "contr_flights_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(flying_group, levels = c("non-flyer", "average flyer", "frequent flyer"))
  )

contr_delta_clim <- read_csv(
  here("data", "contr_flights_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"))
  )

contr_wtc <- read_csv(
  here("data", "contr_wtc.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$")
  )

contr_wtp <- read_csv(
  here("data", "contr_wtp.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$")
  )


################ plot contrasts ####################

plot_contrasts_subgroup <- function(
  contr_subgroup,
  contr_overall = NULL,
  by,
  legend_title = "Group",
  main_text_size = 14,
  alpha = 1
) {
  by_sym <- ensym(by)
  
  if (!is.null(contr_overall)) {
    contr_overall <- contr_overall %>%
      mutate(!!by_sym := "Overall")
    contr_plot <- bind_rows(contr_subgroup, contr_overall)
  } else {
    contr_plot <- contr_subgroup
  }
  
  if ("n" %in% colnames(contr_plot)) {
    contr_plot <- contr_plot %>%
      mutate(
        !!by_sym := ifelse(
          .data[[by]] == "Overall",
          "Overall",
          paste0(.data[[by]], " (n=", n, ")")
        )
      )
  }
  
  subgroups <- unique(contr_plot[[by]])
  subgroups <- setdiff(subgroups, "Overall")
  contr_plot[[by]] <- factor(
    contr_plot[[by]],
    levels = c(subgroups, "Overall")
  )
  
  subgroup_colors <- viridis::viridis(length(subgroups), option = "plasma", end = 0.8)
  color_vector <- c(subgroup_colors, "Overall" = "grey40")
  names(color_vector) <- c(subgroups, "Overall")
  
  ggplot(contr_plot, aes(
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
    scale_color_manual(values = color_vector) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "gray40",
      linewidth = 0.3
    ) +
    labs(
      y = "Difference vs control",
      x = "Treatment",
      color = legend_title
    ) +
    ylim(-1.65, 1.5) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}



plot_contr_wtp_income <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_income,
  contr_overall  = contr_wtp,
  by = "income_group",
  legend_title = "Income group"
) + labs(title = "A. Willingness to pay")

plot_contr_wtc_income <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_income,
  contr_overall  = contr_wtc,
  by = "income_group",
  legend_title = "Income group"
) + labs(title = "B. Willingness to change")

plot_contr_income <- (plot_contr_wtp_income | plot_contr_wtc_income) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")



plot_contr_wtp_flyer <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_flyer,
  contr_overall  = contr_wtp,
  by = "flying_group",
  legend_title = "Flying behaviour"
) + labs(title = "A. Willingness to pay")

plot_contr_wtc_flyer <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_flyer,
  contr_overall  = contr_wtc,
  by = "flying_group",
  legend_title = "Flying behaviour"
) + labs(title = "B. Willingness to change")

plot_contr_flyer <- (plot_contr_wtp_flyer | plot_contr_wtc_flyer) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")



plot_contr_wtp_clim <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_clim,
  contr_overall  = contr_wtp,
  by = "clim_concern",
  legend_title = "Climate concern"
) + labs(title = "A. Willingness to pay")

plot_contr_wtc_clim <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_clim,
  contr_overall  = contr_wtc,
  by = "clim_concern",
  legend_title = "Climate concern"
) + labs(title = "B. Willingness to change")

plot_contr_clim <- (plot_contr_wtp_clim | plot_contr_wtc_clim) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")


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
