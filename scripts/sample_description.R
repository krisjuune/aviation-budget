library(tidyverse)
library(here)
library(stringr)
library(dplyr)
library(purrr)
library(janitor)

df_us <- read_csv(
  here("data", "data_clean_us.csv"),
  show_col_types = FALSE
) |>
  mutate(
    country = "us"
  )

df_ch <- read_csv(
  here("data", "data_clean_ch.csv"),
  show_col_types = FALSE
) |>
  mutate(
    country = "ch"
  )

df_cn <- read_csv(
  here("data", "data_clean_cn.csv"),
  show_col_types = FALSE
) |>
  mutate(
    country = "cn"
  )

df_income <- read_csv(
  here("data", "wtc_wtp_controls_tidy.csv"),
  show_col_types = FALSE
) |>
  mutate(income_decile = as.integer(income_decile))

###################### remove outliers ####################

cutoff <- 200

df_cn_raw <- df_cn

df_cn <- df_cn |>
  filter(
    is.na(flying_recent_number) | flying_recent_number <= cutoff
  )


###################### sample description #################

quota_gender <- list(
  us = c(male = 0.49, female = 0.51),
  ch = c(male = 0.50, female = 0.50),
  cn = c(male = 0.51, female = 0.49)
)

quota_age <- list(
  us = c(
    "18y_24y" = 0.12,
    "25y_34y" = 0.18,
    "35y_44y" = 0.18,
    "45y_54y" = 0.16,
    "55y_64y" = 0.17,
    "65y_above" = 0.19
  ),
  ch = c(
    "18y_24y" = 0.09,
    "25y_34y" = 0.18,
    "35y_44y" = 0.19,
    "45y_54y" = 0.18,
    "55y_64y" = 0.18,
    "65y_above" = 0.18
  ),
  cn = c(
    "18y_24y" = 0.12,
    "25y_34y" = 0.20,
    "35y_44y" = 0.18,
    "45y_54y" = 0.16,
    "55y_64y" = 0.34,
    "65y_above" = 0.00
  )
)

quota_language_ch <- c(
  german = 0.73,
  french = 0.27,
  italian = 0.00,
  romansh = 0.00
)

df_all <- bind_rows(df_ch, df_us, df_cn)

summarize_age <- function(df, country) {

  quota_tbl <- tibble(
    age = names(quota_age[[country]]),
    target = unname(quota_age[[country]])
  )

  df |>
    count(age) |>
    left_join(quota_tbl, by = "age") |>
    mutate(
      share = n / sum(n),
      line = paste0(
        age, ": ",
        round(share * 100, 1), "% (target: ",
        round(target * 100, 1), "%)"
      )
    ) |>
    pull(line)
}

summarize_gender <- function(df, country) {

  quota_tbl <- tibble(
    gender = names(quota_gender[[country]]),
    target = unname(quota_gender[[country]])
  )

  df |>
    count(gender) |>
    left_join(quota_tbl, by = "gender") |>
    mutate(
      share = n / sum(n),
      line = paste0(
        gender, ": ",
        round(share * 100, 1), "% (target: ",
        round(target * 100, 1), "%)"
      )
    ) |>
    pull(line)
}

summarize_language_ch <- function(df) {

  quota_tbl <- tibble(
    ch_region = names(quota_language_ch),
    target = unname(quota_language_ch)
  )

  df |>
    count(ch_region) |>
    left_join(quota_tbl, by = "ch_region") |>
    mutate(
      share = n / sum(n),
      line = paste0(
        ch_region, ": ",
        round(share * 100, 1), "% (target: ",
        round(target * 100, 1), "%)"
      )
    ) |>
    pull(line)
}

summarize_flying <- function(df) {

  flights <- case_when(
    df$flying_ever == "no" ~ 0,
    df$flying_recent == "no" ~ 0,
    TRUE ~ df$flying_recent_number
  )

  median_val <- median(flights, na.rm = TRUE)
  q25 <- quantile(flights, 0.25, na.rm = TRUE)
  q75 <- quantile(flights, 0.75, na.rm = TRUE)
  q90 <- quantile(flights, 0.90, na.rm = TRUE)

  non_fliers <- mean(flights == 0, na.rm = TRUE)

  c(
    paste("Median flights:", round(median_val, 2)),
    paste("25th percentile:", round(q25, 2)),
    paste("75th percentile:", round(q75, 2)),
    paste("90th percentile:", round(q90, 2)),
    paste("Share non-fliers:", round(non_fliers * 100, 1), "%")
  )
}

summarize_income <- function(df_income, country_code) {

  df_income |>
    filter(country == country_code) |>
    count(income_decile) |>
    arrange(income_decile) |>
    mutate(
      share = n / sum(n),
      line = paste0(
        "Decile ", income_decile, ": ",
        round(share * 100, 1), "%"
      )
    ) |>
    pull(line)
}

country_summary <- function(df, country_name, country_code) {

  lines <- c(
    paste0("----- ", country_name, " -----"),
    "",
    "Age distribution:",
    summarize_age(df, country_code),
    "",
    "Gender distribution:",
    summarize_gender(df, country_code)
  )

  if (country_code == "ch") {
    lines <- c(
      lines,
      "",
      "Language region:",
      summarize_language_ch(df)
    )
  }

  lines <- c(
    lines,
    "",
    "Flying distribution:",
    summarize_flying(df),
    "",
    "Income distribution:",
    summarize_income(df_income, country_code),
    ""
  )

  return(lines)
}

total_n <- nrow(df_all)

n_ch <- nrow(df_ch)
n_us <- nrow(df_us)
n_cn <- nrow(df_cn)

max_removed <- df_cn_raw |>
  filter(flying_recent_number > cutoff) |>
  summarise(max_val = max(flying_recent_number, na.rm = TRUE)) |>
  pull(max_val)

summary_text <- c(

  "Sample summary",
  "================",

  paste("Total sample size:", total_n),
  paste("CH sample:", n_ch),
  paste("US sample:", n_us),
  paste("CN sample:", n_cn),

  "",
  paste("Outlier cutoff for flying_recent_number:", cutoff),
  paste("Maximum removed value:", 235),

  "",

  country_summary(df_ch, "Switzerland", "ch"),
  country_summary(df_us, "United States", "us"),
  country_summary(df_cn, "China", "cn")
)

writeLines(
  summary_text,
  here("output", "sample_summary.txt")
)