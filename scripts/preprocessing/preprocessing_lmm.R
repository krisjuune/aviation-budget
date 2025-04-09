library(tidyverse)
library(here)

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

var_list <- c(
  "id", "country",
  "c_wtc_fly", "t_wtc_fly",
  "c_wtp_buy", "t_wtp_buy",
  "treatment", "red_amt"
)

df <- map_dfr(
  list(df_us, df_ch, df_cn),
  select, all_of(var_list)
)

df_tidy <- df |>
  mutate(
  wtc = case_when(
    treatment == "control" ~ c_wtc_fly,
    treatment != "control" ~ t_wtc_fly),
  wtp = case_when(
    treatment == "control" ~ c_wtp_buy,
    treatment != "control" ~ t_wtp_buy)
  ) |>
  select(id, country, wtc, wtp, treatment, red_amt)

df_tidy |>
  group_by(treatment) |>
  count()

df_tidy |>
  group_by(red_amt) |>
  count()

df_tidy |>
  group_by(country) |>
  count()

df_tidy <- df_tidy |>
  pivot_longer(
    c(wtc, wtp),
    names_to = "outcome"
  ) |>
  mutate(value = case_when(
    value %in% c("very_unwilling", "very_unlikely") ~ 0,
    value %in% c("unwilling", "unlikely") ~ 1,
    value %in% c("somewhat_unwilling", "somewhat_unlikely") ~ 2,
    value %in% c("somewhat_willing", "somewhat_likely") ~ 3,
    value %in% c("willing", "likely") ~ 4,
    value %in% c("very_willing", "very_likely") ~ 5,
  ))

df_tidy <- df_tidy |>
  filter(!is.na(treatment) & !is.na(red_amt))

write_csv(df_tidy, here("data", "wtc_wtp_tidy.csv"))
