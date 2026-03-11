library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)

data_fair <- read_csv(
  here("data", "wtc_wtp_fair_tidy.csv"),
  show_col_types = FALSE
)

data_fair_wtc <- data_fair |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Control" = "control",
        "Equal budget" = "egal",
        "Frequent flying cap" = "limit",
        "Tourism cap" = "prior",
        "Proportional reduction" = "prop"
      )
  ) |>
  drop_na(fair_self_wtc, fair_group_wtc)


data_fair_wtp <- data_fair |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Control" = "control",
        "Income-based tax" = "egal",
        "Frequent flying tax" = "limit",
        "Tourism tax" = "prior",
        "Flying tax" = "prop"
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
      treatment = factor(treatment, levels = rev(treatment_order)),
      score = factor(score, levels = 0:5),
      fairness_type = factor(
        fairness_type,
        levels = c("Personal fairness", "Group fairness")
      )
    )

  p <- ggplot(
    fairness_prop,
    aes(x = treatment, y = prop, fill = score)
  ) +
    geom_col(alpha = 0.6) +
    geom_text(
      aes(label = ifelse(prop > 0.05, percent(prop, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    facet_wrap(~fairness_type, nrow = 1) +
    coord_flip() +
    scale_fill_viridis_d(
      option = "plasma",
      begin = 0.1,
      end = 1,
      labels = likert_labels,
      name = "Fairness score"
    ) +
    guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = NULL,
      y = "Share of respondents"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text.x = element_text(size = 14, face = "bold")
    )

  return(p)
}

fairness_wtc <- plot_fairness(
  data_fair_wtc,
  fair_self_wtc,
  fair_group_wtc
) +
  labs(title = "B. Fairness scores for different aviation budget designs")

fairness_wtp <- plot_fairness(
  data_fair_wtp,
  fair_self_wtp,
  fair_group_wtp
) +
  labs(title = "A. Fairness scores for different aviation tax designs")

fairness_scores <- fairness_wtp / fairness_wtc +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

ggsave(
  plot = fairness_scores,
  here("output", "fairness_scores.png"),
  height = 7,
  width = 14
)