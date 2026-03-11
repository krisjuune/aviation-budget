library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)

emm_wtc <- read_csv(
  here("data", "emm_wtc.csv"), show_col_types = FALSE
)

emm_wtp <- read_csv(
  here("data", "emm_wtp.csv"), show_col_types = FALSE
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



################## plot emm #####################

plot_emmeans <- function(
  emm,
  main_text_size = 14,
  plot_30 = TRUE,
  alpha = 1,
  title = NULL
) {
  data <- as.data.frame(emm)

  if (!plot_30) {
    data <- subset(data, red_amt != "30%")
  }

  ymin_col <- if ("asymp.LCL" %in% names(data)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(data)) "asymp.UCL" else "upper.CL"

  ggplot(data, aes(
    x = treatment,
    y = emmean
  )) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = .data[[ymin_col]],
        ymax = .data[[ymax_col]]
      ),
      width = 0.2,
      alpha = alpha
    ) +
    labs(
      y = NULL,
      x = NULL,
      title = title
    ) +
    geom_hline(
      yintercept = 2.5,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    ylim(1, 4) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}

plot_wtp <- plot_emmeans(
  emm_wtp,
  plot_30 = TRUE,
  title = "A. Willingness to pay"
)

plot_wtc <- plot_emmeans(
  emm_wtc,
  plot_30 = TRUE,
  title = "B. Willingness to change"
)

plot_redamt_combined <- (
  plot_wtp | plot_wtc
) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")


################# plot contrasts ################

plot_contrasts <- function(
  contr,
  by,
  legend_title = "Emissions reductions",
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
    ylim(-1.1, 1.1) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}

plot_contrasts <- function(
  contr,
  main_text_size = 14,
  alpha = 1
) {
  data <- as.data.frame(contr)

  ggplot(data, aes(
    x = contrast,
    y = estimate
  )) +
    geom_point(size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * SE,
        ymax = estimate + 1.96 * SE
      ),
      width = 0.2,
      alpha = alpha
    ) +
    labs(
      y = "Difference vs control",
      x = "Treatment"
    ) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    ylim(-1.1, 1.1) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}

plot_contr_wtp <- plot_contrasts(contr_wtp) +
  labs(title = "A. Willingness to pay")

plot_contr_wtc <- plot_contrasts(contr_wtc) +
  labs(title = "B. Willingness to change")

plot_contr_combined <- (
  plot_contr_wtp | plot_contr_wtc
) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")


#################### save stuff #################

ggsave(
  plot = plot_redamt_combined,
  here("output", "plot_wtc_wtp_emm.png"),
  height = 6, width = 14
)

ggsave(
  plot = plot_contr_combined,
  here("output", "plot_wtc_wtp_contr.png"),
  height = 6, width = 14
)