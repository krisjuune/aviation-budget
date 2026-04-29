library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)
library(stringr)
library(tidyverse)
library(here)
library(ggridges)

if (exists("snakemake")) {
  controls_file          <- snakemake@input[["controls"]]
  fair_file              <- snakemake@input[["fair"]]
  us_file                <- snakemake@input[["us"]]
  ch_file                <- snakemake@input[["ch"]]
  cn_file                <- snakemake@input[["cn"]]
  out_dist_income        <- snakemake@output[["dist_income"]]
  out_dist_flights       <- snakemake@output[["dist_flights"]]
  out_dist_clim          <- snakemake@output[["dist_clim"]]
  out_dist_education     <- snakemake@output[["dist_education"]]
  out_dist_age           <- snakemake@output[["dist_age"]]
  out_bivar_income_clim  <- snakemake@output[["bivar_income_clim"]]
  out_bivar_flights_clim <- snakemake@output[["bivar_flights_clim"]]
  out_bivar_edu_income   <- snakemake@output[["bivar_edu_income"]]
  out_bivar_age_clim     <- snakemake@output[["bivar_age_clim"]]
  out_flying_purpose     <- snakemake@output[["flying_purpose"]]
  out_corr_pooled        <- snakemake@output[["corr_pooled"]]
  out_corr_by_country    <- snakemake@output[["corr_by_country"]]
  main_text_size         <- snakemake@config[["main_text_size"]]
  point_size             <- snakemake@config[["point_size"]]
  line_linewidth         <- snakemake@config[["line_linewidth"]]
} else {
  controls_file          <- here("data", "wtc_wtp_controls_tidy.csv")
  fair_file              <- here("data", "wtc_wtp_fair_tidy.csv")
  us_file                <- here("data", "data_clean_us.csv")
  ch_file                <- here("data", "data_clean_ch.csv")
  cn_file                <- here("data", "data_clean_cn.csv")
  out_dist_income        <- here("output", "sample", "plot_dist_income.png")
  out_dist_flights       <- here("output", "sample", "plot_dist_flights.png")
  out_dist_clim          <- here("output", "sample", "plot_dist_clim.png")
  out_dist_education     <- here("output", "sample", "plot_dist_education.png")
  out_dist_age           <- here("output", "sample", "plot_dist_age.png")
  out_bivar_income_clim  <- here("output", "sample", "plot_bivar_income_clim.png")
  out_bivar_flights_clim <- here("output", "sample", "plot_bivar_flights_clim.png")
  out_bivar_edu_income   <- here("output", "sample", "plot_bivar_edu_income.png")
  out_bivar_age_clim     <- here("output", "sample", "plot_bivar_age_clim.png")
  out_flying_purpose     <- here("output", "sample", "plot_flying_purpose.png")
  out_corr_pooled        <- here("output", "sample", "plot_corr_pooled.png")
  out_corr_by_country    <- here("output", "sample", "plot_corr_by_country.png")
  main_text_size         <- 14
  point_size             <- 3
  line_linewidth         <- 1
}

dir.create(dirname(out_dist_income), showWarnings = FALSE, recursive = TRUE)

######################### load data #########################

edu_num_map <- c(
  no_high_school = 1L, no_matura = 1L,
  high_school    = 2L, matura    = 2L,
  bachelor       = 3L, postgrad  = 4L
)

edu_label_map <- c(
  no_high_school = "Below high school", no_matura = "Below high school",
  high_school    = "High school",       matura    = "High school",
  bachelor       = "Bachelor",          postgrad  = "Postgraduate"
)

edu_levels <- c("Below high school", "High school", "Bachelor", "Postgraduate")

age_levels <- c("18y_24y", "25y_34y", "35y_44y", "45y_54y", "55y_64y", "65y_above")
age_labels <- c(
  "18y_24y"    = "18–24", "25y_34y" = "25–34",
  "35y_44y"    = "35–44", "45y_54y" = "45–54",
  "55y_64y"    = "55–64", "65y_above" = "65+"
)

country_names <- c(ch = "Switzerland", cn = "China", us = "United States")

df_demo <- bind_rows(
  read_csv(us_file, show_col_types = FALSE) |> select(id, age, education),
  read_csv(ch_file, show_col_types = FALSE) |> select(id, age, education),
  read_csv(cn_file, show_col_types = FALSE) |> select(id, age, education)
) |>
  mutate(
    education_num   = edu_num_map[education],
    education_label = factor(edu_label_map[education], levels = edu_levels),
    age_num         = as.integer(factor(age, levels = age_levels)),
    age             = factor(age, levels = age_levels)
  )

data_controls <- read_csv(controls_file, show_col_types = FALSE) |>
  filter(time == "pre") |>
  mutate(
    income_decile        = as.integer(income_decile),
    flying_recent_number = replace_na(flying_recent_number, 0),
    country_label        = recode(country, !!!country_names)
  )

data_fair <- read_csv(fair_file, show_col_types = FALSE) |>
  filter(time == "pre") |>
  select(id, fair_self_wtc, fair_group_wtc, fair_self_wtp, fair_group_wtp)

df <- data_controls |>
  left_join(df_demo, by = "id") |>
  left_join(data_fair, by = "id")

p95_flights <- quantile(df$flying_recent_number, 0.95, na.rm = TRUE)

######################### theme #########################

theme_explore <- function() {
  theme_classic() +
    theme(
      text             = element_text(size = main_text_size),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

######################### distributions #########################

plot_dist_income <- df |>
  filter(!is.na(income_decile)) |>
  ggplot(aes(x = income_decile)) +
  geom_bar(fill = "#4393C3", alpha = 0.85) +
  facet_wrap(~country_label) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Income decile", y = "Count",
    title = "Income decile distribution"
  ) +
  theme_explore()

plot_dist_flights <- df |>
  mutate(flights_display = pmin(flying_recent_number, p95_flights)) |>
  ggplot(aes(x = flights_display)) +
  geom_histogram(binwidth = 1, fill = "#74ADD1", alpha = 0.85) +
  facet_wrap(~country_label, scales = "free_y") +
  labs(
    x       = "Number of flights (last 12 months)",
    y       = "Count",
    title   = "Number of flights distribution",
    caption = paste0(
      "Capped at 95th percentile (", p95_flights, " flights) for display"
    )
  ) +
  theme_explore()

plot_dist_clim <- df |>
  filter(!is.na(clim_concern_score)) |>
  ggplot(aes(x = clim_concern_score)) +
  geom_histogram(binwidth = 1, fill = "#66C2A5", alpha = 0.85) +
  facet_wrap(~country_label) +
  scale_x_continuous(breaks = seq(0, 15, by = 3)) +
  labs(
    x     = "Climate concern sum score (0–15)",
    y     = "Count",
    title = "Climate concern sum score distribution"
  ) +
  theme_explore()

plot_dist_education <- df |>
  filter(!is.na(education_label)) |>
  count(country_label, education_label) |>
  group_by(country_label) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = education_label, y = pct)) +
  geom_col(fill = "#FC8D59", alpha = 0.85) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)), vjust = -0.3, size = 3.5
  ) +
  facet_wrap(~country_label) +
  scale_y_continuous(
    labels = percent_format(), expand = expansion(mult = c(0, 0.12))
  ) +
  labs(x = "Education level", y = "Share", title = "Education distribution") +
  theme_explore() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

plot_dist_age <- df |>
  filter(!is.na(age)) |>
  count(country_label, age) |>
  group_by(country_label) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = fct_recode(age, !!!age_labels), y = pct)) +
  geom_col(fill = "#FDAE61", alpha = 0.85) +
  geom_text(
    aes(label = percent(pct, accuracy = 1)), vjust = -0.3, size = 3.5
  ) +
  facet_wrap(~country_label) +
  scale_y_continuous(
    labels = percent_format(), expand = expansion(mult = c(0, 0.12))
  ) +
  labs(x = "Age group", y = "Share", title = "Age distribution") +
  theme_explore()

######################### bivariate #########################

plot_bivar_income_clim <- df |>
  filter(!is.na(income_decile), !is.na(clim_concern_score)) |>
  ggplot(aes(x = factor(income_decile), y = clim_concern_score)) +
  geom_violin(
    fill = "#66C2A5", alpha = 0.65,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = point_size) +
  facet_wrap(~country_label, nrow = 1) +
  labs(
    x       = "Income decile",
    y       = "Climate concern sum score",
    title   = "Climate concern by income decile",
    caption = "Diamond marks the mean; lines inside violins are quartiles."
  ) +
  theme_explore()

plot_bivar_flights_clim <- df |>
  filter(!is.na(clim_concern_score)) |>
  mutate(flights_display = pmin(flying_recent_number, p95_flights)) |>
  ggplot(aes(x = flights_display, y = clim_concern_score)) +
  geom_point(alpha = 0.12, size = 1) +
  geom_smooth(
    method = "lm", colour = "#D6604D", fill = "#F4A582", linewidth = line_linewidth
  ) +
  facet_wrap(~country_label, nrow = 1) +
  labs(
    x     = "Number of flights (last 12 months)",
    y     = "Climate concern sum score",
    title = "Climate concern by number of flights"
  ) +
  theme_explore()

plot_bivar_edu_income <- df |>
  filter(!is.na(education_label), !is.na(income_decile)) |>
  ggplot(aes(x = education_label, y = income_decile)) +
  geom_violin(
    fill = "#FC8D59", alpha = 0.65,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = point_size) +
  facet_wrap(~country_label, nrow = 1) +
  scale_y_continuous(breaks = 1:10) +
  labs(
    x     = "Education level",
    y     = "Income decile",
    title = "Income by education level"
  ) +
  theme_explore() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

plot_bivar_age_clim <- df |>
  filter(!is.na(age), !is.na(clim_concern_score)) |>
  ggplot(aes(x = fct_recode(age, !!!age_labels), y = clim_concern_score)) +
  geom_violin(
    fill = "#FDAE61", alpha = 0.65,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = point_size) +
  facet_wrap(~country_label, nrow = 1) +
  labs(
    x       = "Age group",
    y       = "Climate concern sum score",
    title   = "Climate concern by age group",
    caption = "Diamond marks the mean; lines inside violins are quartiles."
  ) +
  theme_explore()

######################### flying purpose #########################

purpose_levels <- c("tourism", "family", "business", "studies", "other")
purpose_labels <- c(
  tourism  = "Tourism",  family   = "Family",
  business = "Business", studies  = "Studies", other = "Other"
)

n_fliers_by_country <- df |>
  filter(flying_recent == "yes") |>
  count(country_label, name = "n_fliers")

plot_flying_purpose <- df |>
  filter(
    flying_recent == "yes",
    !is.na(flying_purpose),
    flying_purpose != ""
  ) |>
  separate_rows(flying_purpose, sep = ",") |>
  mutate(flying_purpose = str_trim(flying_purpose)) |>
  filter(flying_purpose %in% purpose_levels) |>
  distinct(id, flying_purpose, .keep_all = TRUE) |>
  mutate(flying_purpose = factor(flying_purpose, levels = purpose_levels)) |>
  count(country_label, flying_purpose) |>
  left_join(n_fliers_by_country, by = "country_label") |>
  mutate(prop = n / n_fliers) |>
  ggplot(aes(
    x = fct_recode(flying_purpose, !!!purpose_labels), y = prop
  )) +
  geom_col(fill = "#74ADD1", alpha = 0.85) +
  geom_text(
    aes(label = percent(prop, accuracy = 1)), vjust = -0.3, size = 3.5
  ) +
  facet_wrap(~country_label) +
  scale_y_continuous(
    labels = percent_format(), expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x        = NULL,
    y        = "Share of recent fliers",
    title    = "Flying purpose among recent fliers",
    subtitle = "Multiple purposes allowed; proportions sum to more than 100%"
  ) +
  theme_explore()

######################### correlations #########################

vars_corr <- c(
  "income_decile",
  "flying_recent_number",
  "clim_concern_score",
  "fair_self_wtc",
  "fair_group_wtc",
  "fair_self_wtp",
  "fair_group_wtp",
  "education_num",
  "age_num"
)

var_labels_corr <- c(
  income_decile        = "Income decile",
  flying_recent_number = "Nr of flights",
  clim_concern_score   = "Climate concern",
  fair_self_wtc        = "Fairness self (WTC)",
  fair_group_wtc       = "Fairness group (WTC)",
  fair_self_wtp        = "Fairness self (WTP)",
  fair_group_wtp       = "Fairness group (WTP)",
  education_num        = "Education",
  age_num              = "Age"
)

make_corr_heatmap <- function(data, title, label_size = 3.5) {
  mat <- cor(
    data[, vars_corr], method = "spearman", use = "pairwise.complete.obs"
  )
  as.data.frame(mat) |>
    rownames_to_column("var1") |>
    pivot_longer(-var1, names_to = "var2", values_to = "r") |>
    mutate(
      label1 = factor(var_labels_corr[var1], levels = var_labels_corr),
      label2 = factor(var_labels_corr[var2], levels = rev(var_labels_corr))
    ) |>
    ggplot(aes(x = label1, y = label2, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(
      aes(label = sprintf("%.2f", r), colour = abs(r) > 0.45),
      size = label_size
    ) +
    scale_fill_gradient2(
      low = "#4393C3", mid = "white", high = "#D6604D",
      midpoint = 0, limits = c(-1, 1), name = "Spearman ρ"
    ) +
    scale_colour_manual(
      values = c("TRUE" = "white", "FALSE" = "grey20"), guide = "none"
    ) +
    labs(x = NULL, y = NULL, title = title) +
    theme_classic() +
    theme(
      text          = element_text(size = main_text_size),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      panel.grid    = element_blank()
    )
}

plot_corr_pooled <- make_corr_heatmap(
  df, "Spearman correlation matrix (pooled)"
)

plot_corr_by_country <- df |>
  group_by(country_label) |>
  group_modify(~ {
    mat <- cor(
      .x[, vars_corr], method = "spearman", use = "pairwise.complete.obs"
    )
    as.data.frame(mat) |>
      rownames_to_column("var1") |>
      pivot_longer(-var1, names_to = "var2", values_to = "r")
  }) |>
  ungroup() |>
  mutate(
    label1 = factor(var_labels_corr[var1], levels = var_labels_corr),
    label2 = factor(var_labels_corr[var2], levels = rev(var_labels_corr))
  ) |>
  ggplot(aes(x = label1, y = label2, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = sprintf("%.2f", r), colour = abs(r) > 0.45),
    size = 2.8
  ) +
  scale_fill_gradient2(
    low = "#4393C3", mid = "white", high = "#D6604D",
    midpoint = 0, limits = c(-1, 1), name = "Spearman ρ"
  ) +
  scale_colour_manual(
    values = c("TRUE" = "white", "FALSE" = "grey20"), guide = "none"
  ) +
  facet_wrap(~country_label) +
  labs(x = NULL, y = NULL, title = "Spearman correlation matrix by country") +
  theme_classic() +
  theme(
    text             = element_text(size = main_text_size),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid       = element_blank(),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

######################### save #########################

ggsave(plot = plot_dist_income,        out_dist_income,        height = 5, width = 12)
ggsave(plot = plot_dist_flights,       out_dist_flights,       height = 5, width = 12)
ggsave(plot = plot_dist_clim,          out_dist_clim,          height = 5, width = 12)
ggsave(plot = plot_dist_education,     out_dist_education,     height = 5, width = 12)
ggsave(plot = plot_dist_age,           out_dist_age,           height = 5, width = 12)
ggsave(plot = plot_bivar_income_clim,  out_bivar_income_clim,  height = 5, width = 12)
ggsave(plot = plot_bivar_flights_clim, out_bivar_flights_clim, height = 5, width = 12)
ggsave(plot = plot_bivar_edu_income,   out_bivar_edu_income,   height = 5, width = 12)
ggsave(plot = plot_bivar_age_clim,     out_bivar_age_clim,     height = 5, width = 12)
ggsave(plot = plot_flying_purpose,     out_flying_purpose,     height = 5, width = 12)
ggsave(plot = plot_corr_pooled,        out_corr_pooled,        height = 7, width = 9)
ggsave(plot = plot_corr_by_country,    out_corr_by_country,    height = 7, width = 18)
