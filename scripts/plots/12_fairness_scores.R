library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)
library(forcats)
library(here)
library(tidyverse)
library(patchwork)

if (exists("snakemake")) {
  fair_file    <- snakemake@input[["fair"]]
  plot_out       <- snakemake@output[["fairness_plot"]]
  main_text_size <- snakemake@config[["main_text_size"]]
  main_colour    <- snakemake@config[["main_colour"]]
  light_colour   <- snakemake@config[["light_colour"]]
} else {
  fair_file      <- here("data", "wtc_wtp_fair_tidy.csv")
  plot_out       <- here("output", "fairness_scores.png")
  main_text_size <- 14
  main_colour    <- "#3B4CC0"
  light_colour   <- "#AAB0FF"
}

data_fair <- read_csv(fair_file, show_col_types = FALSE)

likert_labels <- c(
  "0" = "Very unfair",
  "1" = "Unfair",
  "2" = "Somewhat unfair",
  "3" = "Somewhat fair",
  "4" = "Fair",
  "5" = "Very fair"
)

# 6-colour gradient: main_colour at position 2, light_colour at position 5.
# Extreme endpoints derived algebraically: a = (4*main - light) / 3,
# d = (4*light - main) / 3, so that a 2-anchor colorRampPalette(a, d)(6)
# places main and light exactly at positions 2 and 5.
lik_colours <- local({
  m    <- col2rgb(main_colour)[, 1]
  l    <- col2rgb(light_colour)[, 1]
  clip <- function(x) pmax(0L, pmin(255L, as.integer(round(x))))
  a    <- clip((4 * m - l) / 3)
  d    <- clip((4 * l - m) / 3)
  colorRampPalette(c(
    rgb(a[1], a[2], a[3], maxColorValue = 255),
    rgb(d[1], d[2], d[3], maxColorValue = 255)
  ))(6)
})

data_fair_wtc <- data_fair |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Control"                = "control",
        "Equal budget"           = "egal",
        "Frequent-flying cap"    = "limit",
        "Tourism cap"            = "prior",
        "Proportional reduction" = "prop"
      )
  ) |>
  drop_na(fair_self_wtc, fair_group_wtc)

data_fair_wtp <- data_fair |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Control"             = "control",
        "Income-based fee"    = "egal",
        "Frequent-flying fee" = "limit",
        "Tourism fee"         = "prior",
        "Flying fee"          = "prop"
      )
  ) |>
  drop_na(fair_self_wtp, fair_group_wtp)

########## plot fairness scores ###########

plot_fairness <- function(data, self_var, group_var) {

  self_name  <- deparse(substitute(self_var))
  group_name <- deparse(substitute(group_var))

  fairness_long <- data |>
    filter(time == "post", treatment != "Control") |>
    select(id, treatment, {{self_var}}, {{group_var}}) |>
    pivot_longer(
      cols = c({{self_var}}, {{group_var}}),
      names_to = "fairness_type",
      values_to = "score"
    ) |>
    mutate(
      fairness_type = case_when(
        fairness_type == self_name  ~ "Personal fairness",
        fairness_type == group_name ~ "Group fairness"
      )
    )

  treatment_order <- fairness_long |>
    filter(fairness_type == "Personal fairness") |>
    group_by(treatment) |>
    summarise(mean_fair = mean(score, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(mean_fair)) |>
    pull(treatment)

  fairness_prop <- fairness_long |>
    group_by(treatment, fairness_type, score) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(treatment, fairness_type) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(
      treatment    = factor(treatment, levels = rev(treatment_order)),
      score        = factor(score, levels = 0:5),
      fairness_type = factor(
        fairness_type,
        levels = c("Personal fairness", "Group fairness")
      )
    )

  fairness_means <- fairness_long |>
    group_by(treatment, fairness_type) |>
    summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |>
    mutate(
      fairness_type = factor(
        fairness_type,
        levels = c("Personal fairness", "Group fairness")
      )
    )

  p <- ggplot(fairness_prop, aes(x = treatment, y = prop, fill = score)) +
    geom_col(alpha = 0.75) +
    geom_text(
      aes(label = ifelse(prop > 0.05, percent(prop, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    facet_wrap(~fairness_type, nrow = 1) +
    coord_flip() +
    scale_fill_manual(
      values = lik_colours,
      labels = likert_labels,
      name   = "Fairness score"
    ) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = NULL, y = "Share of respondents") +
    theme_classic(base_size = main_text_size) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    ) +
    geom_text(
      data = fairness_means,
      aes(
        x     = treatment,
        y     = 1.06,
        label = paste0("μ = ", sprintf("%.2f", mean_score))
      ),
      inherit.aes = FALSE,
      size = 3.5
    ) +
    expand_limits(y = 1.1)

  return(p)
}

fairness_wtc <- plot_fairness(data_fair_wtc, fair_self_wtc, fair_group_wtc) +
  labs(title = "B. Fairness scores for flying budget designs")

fairness_wtp <- plot_fairness(data_fair_wtp, fair_self_wtp, fair_group_wtp) +
  labs(title = "A. Fairness scores for flying surcharge designs")

fairness_scores <- fairness_wtp / fairness_wtc +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

ggsave(plot = fairness_scores, plot_out, height = 6, width = 14)
