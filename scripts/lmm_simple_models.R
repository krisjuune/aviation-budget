library(lme4)
library(emmeans)
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

data <- read_csv(here("data", "wtc_wtp_tidy.csv"))

data_flights <- data |>
  filter(!is.na(wtc)) |>
  mutate(time = factor(
    time,
    levels = c("post", "pre")
  )
  )

################## plot functions #####################

plot_emmeans <- function(
  emm,
  main_text_size = 14,
  plot_30 = TRUE,
  alpha = 1
) {
  data <- as.data.frame(emm)

  if (!plot_30) {
    data <- subset(data, red_amt != "30%")
  }

  ymin_col <- if ("asymp.LCL" %in% names(data)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(data)) "asymp.UCL" else "upper.CL"

  ggplot(data, aes(
    x = treatment,
    y = emmean,
    color = red_amt,
    group = red_amt
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
      y = "Estimated marginal means",
      x = "Treatment",
      color = "Emissions reductions"
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

plot_pairwise_contrasts <- function(
  emm,
  plot_30 = TRUE,
  control_only = FALSE,
  no_control = FALSE,
  main_text_size = 14,
  alpha = 1
) {
  contrasts <- contrast(emm, method = "pairwise", by = "red_amt") |>
    as.data.frame() |>
    mutate(
      lower.CL = estimate - 1.96 * SE,
      upper.CL = estimate + 1.96 * SE,
      sig = p.value < 0.05
    )

  if (!plot_30) {
    contrasts <- subset(contrasts, red_amt != "30%")
  }

  if (control_only) {
    contrasts <- contrasts[grepl("control", contrasts$contrast), ]
  }

  if (no_control) {
    contrasts <- contrasts[!grepl("control", contrasts$contrast), ]
  }

  ggplot(contrasts, aes(x = estimate, y = contrast, color = red_amt)) +
    geom_point(alpha = alpha) +
    geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.1, alpha = alpha) +
    geom_vline(
      xintercept = 0,
      linetype = 2,
      color = "gray40",
      linewidth = .3
    ) +
    facet_wrap(~ red_amt) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    theme_classic() +
    theme(text = element_text(size = main_text_size)) +
    labs(
      x = "Estimated difference",
      y = "Contrast",
      color = "Emissions reductions"
    )
}

plot_flight_change <- function(
  emm,
  plot_30 = TRUE,
  main_text_size = 14
) {
  diffs_df <- contrast(emm, method = "pairwise") |>
    as.data.frame()

  if (!plot_30) {
    diffs_df <- diffs_df |> 
      filter(red_amt != "30%")
  }

  ggplot(diffs_df, aes(x = treatment, y = estimate, color = red_amt)) +
    geom_point(size = 3, position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * SE, ymax = estimate + 1.96 * SE),
      width = 0.2,
      position = position_dodge(0.3)
    ) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    # geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
    theme_classic() +
    labs(
      y = "Estimated change after treatment",
      x = "Treatment",
      color = "Emissions reductions"
    ) +
    theme(text = element_text(size = main_text_size))
}

################## basic analysis #####################
# willingness to reduce flying
model <- lmer(
  wtc ~ treatment * red_amt + (1 | country),
  data = data
)
summary(model)
emm <- emmeans(model, ~ red_amt * treatment)
contrast(emm, method = "pairwise", by = "red_amt")
pwpp(emm, by = "red_amt") + theme_classic()

plot_wtc <- plot_emmeans(emm, plot_30 = FALSE) +
  scale_y_continuous(limits = c(1.7, 3.3))
plot_wtc_contrasts <- plot_pairwise_contrasts(emm, plot_30 = FALSE) +
  scale_x_continuous(limits = c(-1.2, 1.2))

# willingness to pay for SAFs
model <- lmer(
  wtp ~ treatment * red_amt + (1 | country),
  data = data
)
summary(model)
emm <- emmeans(model, ~ treatment * red_amt)
pwpp(emm, by = "red_amt") + theme_classic()

plot_wtp <- plot_emmeans(emm, plot_30 = FALSE) +
  scale_y_continuous(limits = c(1.7, 3.3))
plot_wtp_contrasts <- plot_pairwise_contrasts(
  emm,
  plot_30 = FALSE,
  control_only = TRUE
) +
  scale_x_continuous(limits = c(-1.2, 1.2))

# change in planned flights
model <- lmer(
  planned_flights ~ time * treatment * red_amt + (1 | id) + (1 | country),
  data = data_flights
)
summary(model)
emm <- emmeans(model, ~ time | treatment * red_amt)
plot_del_flights <- plot_flight_change(emm, plot_30 = FALSE)

plot_wtc_combined <- (plot_wtc / plot_del_flights) +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "A")

# save stuff
ggsave(
  plot = plot_wtc_contrasts,
  here("output", "plot_wtc_contrasts.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_wtp,
  here("output", "plot_wtp.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_wtp_contrasts,
  here("output", "plot_wtp_contrasts.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_wtc_combined,
  here("output", "plot_wtc_combined.png"),
  height = 8, width = 10
)

#################### assumptions #######################

# check whether assumptions of normally distirbuted
# residuals and random effects hold
check_model(model)


################### test Bayesian ######################

# library(brms)
# brm(wtc ~ treatment * red_amt + (1 | country), data = data_flights)
