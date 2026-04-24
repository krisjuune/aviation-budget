library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)
library(tidyr)
library(scales)
library(stringr)
library(tidyverse)
library(here)
library(patchwork)
library(see)

if (exists("snakemake")) {
  controls_file   <- snakemake@input[["controls"]]
  fair_file       <- snakemake@input[["fair"]]
  us_file         <- snakemake@input[["us"]]
  ch_file         <- snakemake@input[["ch"]]
  cn_file         <- snakemake@input[["cn"]]
  out_dists       <- snakemake@output[["distributions"]]
  out_purpose     <- snakemake@output[["flying_purpose"]]
  out_purpose_cov <- snakemake@output[["purpose_covariates"]]
  out_fair_assoc  <- snakemake@output[["fairness_assoc"]]
  out_corr        <- snakemake@output[["correlations"]]
} else {
  controls_file   <- here("data", "wtc_wtp_controls_tidy.csv")
  fair_file       <- here("data", "wtc_wtp_fair_tidy.csv")
  us_file         <- here("data", "data_clean_us.csv")
  ch_file         <- here("data", "data_clean_ch.csv")
  cn_file         <- here("data", "data_clean_cn.csv")
  out_dists       <- here("output", "plot_sample_distributions.png")
  out_purpose     <- here("output", "plot_flying_purpose.png")
  out_purpose_cov <- here("output", "plot_flying_purpose_covariates.png")
  out_fair_assoc  <- here("output", "plot_fairness_associations.png")
  out_corr        <- here("output", "plot_variable_correlations.png")
}

###################### load and prepare data ######################

country_names <- c(
  ch = "Switzerland", cn = "China", us = "United States"
)

edu_levels <- c(
  "Below high school", "High school / Matura",
  "Bachelor's", "Postgraduate"
)

purpose_levels <- c("tourism", "family", "business", "studies", "other")
purpose_labels <- c(
  tourism  = "Tourism",  family   = "Family",
  business = "Business", studies  = "Studies", other = "Other"
)

demo_raw <- bind_rows(
  read_csv(us_file, show_col_types = FALSE) |>
    select(id, education),
  read_csv(ch_file, show_col_types = FALSE) |>
    select(id, education),
  read_csv(cn_file, show_col_types = FALSE) |>
    select(id, education)
)

data_person <- read_csv(controls_file, show_col_types = FALSE) |>
  filter(time == "post") |>
  left_join(demo_raw, by = "id") |>
  mutate(
    country       = recode(country, !!!country_names),
    income_decile = as.integer(income_decile),
    edu_unified   = case_when(
      education %in% c("no_high_school", "no_matura") ~
        "Below high school",
      education %in% c("high_school", "matura") ~
        "High school / Matura",
      education == "bachelor"  ~ "Bachelor's",
      education == "postgrad"  ~ "Postgraduate",
      TRUE ~ NA_character_
    ),
    edu_unified  = factor(edu_unified, levels = edu_levels),
    edu_num      = as.integer(edu_unified),
    flying_group = case_when(
      flying_recent == "no"           ~ "Non-flier",
      flying_recent_number <= 6       ~ "Average flier",
      flying_recent_number > 6        ~ "Frequent flier",
      TRUE                            ~ NA_character_
    ),
    flying_group = factor(
      flying_group,
      levels = c("Non-flier", "Average flier", "Frequent flier")
    ),
    income_group = case_when(
      income_decile %in% 1:3  ~ "Low",
      income_decile %in% 4:7  ~ "Mid",
      income_decile %in% 8:10 ~ "High",
      TRUE ~ NA_character_
    ),
    income_group = factor(income_group, levels = c("Low", "Mid", "High"))
  )

data_fair_joined <- read_csv(fair_file, show_col_types = FALSE) |>
  filter(time == "post") |>
  mutate(country = recode(country, !!!country_names)) |>
  select(
    id, country,
    fair_group_wtc, fair_self_wtc,
    fair_group_wtp, fair_self_wtp
  ) |>
  left_join(
    data_person |>
      select(id, flying_group, income_group, flying_recent, flying_purpose),
    by = "id"
  ) |>
  mutate(
    purpose_group = case_when(
      flying_recent == "no" ~ "Non-flier",
      str_detect(coalesce(flying_purpose, ""), "business") ~ "Business flier",
      TRUE ~ "Leisure flier"
    ),
    purpose_group = factor(
      purpose_group,
      levels = c("Non-flier", "Leisure flier", "Business flier")
    )
  )

###################### theme and colours ######################

main_colour <- "#AAB0FF"

theme_explore <- function(text_size = 14) {
  theme_classic() +
    theme(
      text             = element_text(size = text_size),
      plot.title       = element_text(face = "bold"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

# Half-violin (density on right) + jittered raw data + boxplot
geom_raincloud <- function(
  fill_col     = main_colour,
  jitter_alpha = 0.06,
  jitter_width = 0.05,
  jitter_size  = 0.5,
  box_width    = 0.08
) {
  list(
    see::geom_violinhalf(
      fill     = fill_col,
      colour   = NA,
      alpha    = 0.9,
      position = position_nudge(x = 0.15)
    ),
    geom_jitter(
      width  = jitter_width,
      alpha  = jitter_alpha,
      size   = jitter_size,
      colour = "grey40"
    ),
    geom_boxplot(
      width         = box_width,
      fill          = "white",
      alpha         = 0.85,
      outlier.shape = NA,
      position      = position_nudge(x = 0.07)
    )
  )
}

###################### distributions ######################

plot_income <- data_person |>
  filter(!is.na(income_decile)) |>
  ggplot(aes(x = income_decile)) +
  geom_bar(fill = main_colour) +
  facet_wrap(~country, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "A. Income decile",
    x = "Income decile", y = "Count"
  ) +
  theme_explore()

plot_clim <- data_person |>
  filter(!is.na(clim_concern_score)) |>
  ggplot(aes(x = clim_concern_score)) +
  geom_histogram(fill = main_colour, binwidth = 1, colour = "white") +
  facet_wrap(~country, nrow = 1) +
  labs(
    title = "B. Climate concern sum score",
    x = "Score (0–15)", y = "Count"
  ) +
  theme_explore()

plot_flights <- data_person |>
  filter(flying_recent == "yes", !is.na(flying_recent_number)) |>
  mutate(flights_capped = pmin(flying_recent_number, 25)) |>
  ggplot(aes(x = flights_capped)) +
  geom_histogram(fill = main_colour, binwidth = 2, colour = "white") +
  facet_wrap(~country, nrow = 1) +
  labs(
    title = "C. Flights per year among recent fliers",
    x = "Number of flights", y = "Count"
  ) +
  theme_explore()

plot_edu <- data_person |>
  filter(!is.na(edu_unified)) |>
  count(country, edu_unified) |>
  group_by(country) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = edu_unified, y = prop)) +
  geom_col(fill = main_colour) +
  facet_wrap(~country, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "D. Education level",
    x = NULL, y = "Share of respondents"
  ) +
  theme_explore() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_dists <- (plot_income | plot_clim) / (plot_flights | plot_edu)
ggsave(out_dists, plot_dists, height = 10, width = 15)

###################### flying purpose ######################

n_fliers_by_country <- data_person |>
  filter(flying_recent == "yes") |>
  count(country, name = "n_fliers")

data_purpose_long <- data_person |>
  filter(
    flying_recent == "yes",
    !is.na(flying_purpose),
    flying_purpose != ""
  ) |>
  separate_rows(flying_purpose, sep = ",") |>
  mutate(flying_purpose = str_trim(flying_purpose)) |>
  filter(flying_purpose %in% purpose_levels) |>
  distinct(id, flying_purpose, .keep_all = TRUE) |>
  mutate(flying_purpose = factor(flying_purpose, levels = purpose_levels))

plot_purpose_prop <- data_purpose_long |>
  count(country, flying_purpose) |>
  left_join(n_fliers_by_country, by = "country") |>
  mutate(prop = n / n_fliers) |>
  ggplot(aes(x = flying_purpose, y = prop)) +
  geom_col(fill = main_colour) +
  facet_wrap(~country, nrow = 1) +
  scale_x_discrete(labels = purpose_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Flying purpose among recent fliers",
    subtitle = "Multiple purposes allowed; proportions sum to more than 100%",
    x = NULL, y = "Share of recent fliers"
  ) +
  theme_explore()

ggsave(out_purpose, plot_purpose_prop, height = 5, width = 15)

plot_purpose_income <- data_purpose_long |>
  filter(!is.na(income_decile)) |>
  ggplot(aes(x = flying_purpose, y = income_decile)) +
  geom_raincloud(jitter_alpha = 0.07) +
  facet_wrap(~country, nrow = 1) +
  scale_x_discrete(labels = purpose_labels) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 10)) +
  coord_flip() +
  labs(
    title = "A. Income decile by flying purpose",
    x = NULL, y = "Income decile"
  ) +
  theme_explore()

plot_purpose_flights <- data_purpose_long |>
  filter(!is.na(flying_recent_number)) |>
  mutate(flights_capped = pmin(flying_recent_number, 25)) |>
  ggplot(aes(x = flying_purpose, y = flights_capped)) +
  geom_raincloud(jitter_alpha = 0.07) +
  facet_wrap(~country, nrow = 1) +
  scale_x_discrete(labels = purpose_labels) +
  coord_flip() +
  labs(
    title = "B. Flights per year by flying purpose",
    x = NULL, y = "Number of flights"
  ) +
  theme_explore()

plot_purpose_cov <- plot_purpose_income / plot_purpose_flights
ggsave(out_purpose_cov, plot_purpose_cov, height = 10, width = 15)

###################### fairness associations ######################

fair_var_labels <- c(
  fair_group_wtc = "Group\n(WTC)",
  fair_self_wtc  = "Personal\n(WTC)",
  fair_group_wtp = "Group\n(WTP)",
  fair_self_wtp  = "Personal\n(WTP)"
)

fairness_long <- data_fair_joined |>
  pivot_longer(
    cols      = c(
      fair_group_wtc, fair_self_wtc,
      fair_group_wtp, fair_self_wtp
    ),
    names_to  = "variable",
    values_to = "score"
  ) |>
  filter(!is.na(score)) |>
  mutate(variable = recode(variable, !!!fair_var_labels))

fair_profile <- function(data, group_var, title) {
  data |>
    filter(!is.na(.data[[group_var]])) |>
    group_by(country, .data[[group_var]], variable) |>
    summarise(
      mean = mean(score, na.rm = TRUE),
      se   = sd(score, na.rm = TRUE) / sqrt(sum(!is.na(score))),
      .groups = "drop"
    ) |>
    ggplot(aes(
      x      = variable,
      y      = mean,
      colour = .data[[group_var]],
      group  = .data[[group_var]]
    )) +
    geom_point(size = 2.5, position = position_dodge(0.35)) +
    geom_errorbar(
      aes(ymin = .data$mean - 1.96 * .data$se,
          ymax = .data$mean + 1.96 * .data$se),
      width    = 0.15,
      position = position_dodge(0.35)
    ) +
    geom_line(position = position_dodge(0.35), alpha = 0.45) +
    facet_wrap(~country, nrow = 1) +
    scale_colour_viridis_d(option = "plasma", end = 0.85) +
    coord_cartesian(ylim = c(1.5, 4.5)) +
    labs(
      title  = title,
      x      = NULL,
      y      = "Mean fairness score (0–5)",
      colour = NULL
    ) +
    theme_explore() +
    theme(legend.position = "right")
}

plot_fair_flying <- fair_profile(
  fairness_long, "flying_group",
  "A. Fairness perceptions by flying frequency"
)
plot_fair_purpose <- fair_profile(
  fairness_long, "purpose_group",
  "B. Fairness perceptions by flying purpose"
)
plot_fair_income <- fair_profile(
  fairness_long, "income_group",
  "C. Fairness perceptions by income group"
)

plot_fair_assoc <- plot_fair_flying / plot_fair_purpose / plot_fair_income
ggsave(out_fair_assoc, plot_fair_assoc, height = 14, width = 15)

###################### correlations ######################

corr_var_names <- c(
  income_decile        = "Income decile",
  edu_num              = "Education level",
  clim_concern_score   = "Climate concern",
  flying_recent_number = "Flights/year",
  fair_group_wtc       = "Group fairness (WTC)",
  fair_self_wtc        = "Personal fairness (WTC)",
  wtc                  = "WTC",
  wtp                  = "WTP"
)

data_corr <- data_person |>
  filter(!is.na(wtc)) |>
  mutate(flying_recent_number = replace_na(flying_recent_number, 0)) |>
  left_join(
    data_fair_joined |> select(id, fair_group_wtc, fair_self_wtc),
    by = "id"
  )

compute_corr <- function(df, ...) {
  mat <- df |>
    select(all_of(names(corr_var_names))) |>
    cor(method = "spearman", use = "pairwise.complete.obs")
  as.data.frame(mat) |>
    rownames_to_column("var1") |>
    pivot_longer(!var1, names_to = "var2", values_to = "rho")
}

corr_long <- data_corr |>
  group_by(country) |>
  group_modify(compute_corr) |>
  ungroup() |>
  mutate(
    var1 = factor(corr_var_names[var1], levels = corr_var_names),
    var2 = factor(corr_var_names[var2], levels = rev(corr_var_names))
  )

plot_corr <- ggplot(corr_long, aes(x = var1, y = var2, fill = rho)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3) +
  facet_wrap(~country, nrow = 1) +
  scale_fill_gradient2(
    low      = "#3B4CC0",
    mid      = "white",
    high     = "#B40426",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Spearman ρ"
  ) +
  coord_equal() +
  labs(
    title = "Spearman correlations between key variables",
    x = NULL, y = NULL
  ) +
  theme_explore() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(out_corr, plot_corr, height = 8, width = 15)
