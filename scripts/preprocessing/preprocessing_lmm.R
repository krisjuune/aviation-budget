library(tidyverse)
library(here)
library(stringr)
library(dplyr)
library(purrr)

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


###################### wtc, wtp, flights ##################
var_list <- c(
  "id", "country",
  "c_wtc_fly", "t_wtc_fly",
  "c_wtp_buy", "t_wtp_buy",
  "planned_flights", "c_wtc_fly_number", "t_wtc_fly_number",
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
      treatment != "control" ~ t_wtc_fly
    ),
    wtp = case_when(
      treatment == "control" ~ c_wtp_buy,
      treatment != "control" ~ t_wtp_buy
    ),
    pre_flights = planned_flights,
    post_flights = case_when(
      treatment == "control" ~ c_wtc_fly_number,
      treatment != "control" ~ t_wtc_fly_number
    ),
  ) |>
  select(
    id, country,
    wtc, wtp,
    pre_flights, post_flights,
    treatment, red_amt
  )

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
  )) |>
  pivot_wider(
    names_from = "outcome",
    values_from = "value"
  ) |>
  pivot_longer(
    cols = c(pre_flights, post_flights),
    names_to = "time",
    values_to = "planned_flights"
  ) |>
  mutate(
    time = ifelse(time == "pre_flights", "pre", "post")
  ) |>
  filter(!is.na(treatment) & !is.na(red_amt))

write_csv(df_tidy, here("data", "wtc_wtp_tidy.csv"))



###################### fairness #########################
fair_self_group_regex <- "(?=.*fair)(?=.*(self|group))"
df_fair <- map_dfr(
  list(df_us, df_ch, df_cn),
  ~ select(.x, all_of(var_list), matches(fair_self_group_regex, perl = TRUE))
)

colnames(df_fair)

df_fair <- df_fair |>
  mutate(
    wtc = case_when(
      treatment == "control" ~ c_wtc_fly,
      treatment != "control" ~ t_wtc_fly
    ),
    wtp = case_when(
      treatment == "control" ~ c_wtp_buy,
      treatment != "control" ~ t_wtp_buy
    ),
    pre_flights = planned_flights,
    post_flights = case_when(
      treatment == "control" ~ c_wtc_fly_number,
      treatment != "control" ~ t_wtc_fly_number
    ),
    fair_group_wtc = case_when(
      treatment == "control" ~ c_wtc_fair_group,
      treatment == "egal" ~ t1_wtc_group_fair,
      treatment == "limit" ~ t2_wtc_group_fair,
      treatment == "prior" ~ t3_wtc_group_fair,
      treatment == "prop" ~ t4_wtc_group_fair
    ),
    fair_self_wtc = case_when(
      treatment == "control" ~ c_wtc_fair_self,
      treatment != "control" ~ t_wtc_fair_self
    ),
    fair_group_wtp = case_when(
      treatment == "control" ~ c_wtp_fair_group,
      treatment != "control" ~ t_wtp_fair_group
    ),
    fair_self_wtp = case_when(
      treatment == "control" ~ c_wtp_fair_self,
      treatment != "control" ~ t_wtp_fair_self
    )
  ) |>
  select(
    id, country,
    wtc, wtp,
    pre_flights, post_flights,
    treatment, red_amt,
    fair_group_wtc, fair_self_wtc,
    fair_group_wtp, fair_self_wtp
  ) |>
  mutate(fair_group_wtc = str_to_lower(fair_group_wtc),
         fair_group_wtc = str_replace_all(fair_group_wtc, " ", "_"),
         fair_self_wtc = str_to_lower(fair_self_wtc),
         fair_self_wtc = str_replace_all(fair_self_wtc, " ", "_"),
         fair_group_wtp = str_to_lower(fair_group_wtp),
         fair_group_wtp = str_replace_all(fair_group_wtp, " ", "_"),
         fair_self_wtp = str_to_lower(fair_self_wtp),
         fair_self_wtp = str_replace_all(fair_self_wtp, " ", "_"))

df_fair |>
  group_by(fair_group_wtc) |>
  count()

df_fair <- df_fair |>
  pivot_longer(
    c(
      wtc, wtp,
      fair_group_wtc, fair_self_wtc,
      fair_group_wtp, fair_self_wtp
    ),
    names_to = "outcome"
  ) |>
  mutate(value = case_when(
    value %in% c("very_unwilling", "very_unlikely", "very_unfair") ~ 0,
    value %in% c("unwilling", "unlikely", "unfair") ~ 1,
    value %in% c("somewhat_unwilling", "somewhat_unlikely", "somewhat_unfair") ~ 2,
    value %in% c("somewhat_willing", "somewhat_likely", "somewhat_fair") ~ 3,
    value %in% c("willing", "likely", "fair") ~ 4,
    value %in% c("very_willing", "very_likely", "very_fair") ~ 5,
  )) |>
  pivot_wider(
    names_from = "outcome",
    values_from = "value"
  ) |>
  pivot_longer(
    cols = c(pre_flights, post_flights),
    names_to = "time",
    values_to = "planned_flights"
  ) |>
  mutate(
    time = ifelse(time == "pre_flights", "pre", "post")
  ) |>
  filter(!is.na(treatment) & !is.na(red_amt))

write_csv(df_fair, here("data", "wtc_wtp_fair_tidy.csv"))




##################### covariates ########################

df_demo <- map_dfr(
  list(df_us, df_ch, df_cn),
  ~ select(
    .x,
    id,
    country,
    clim_concern_wtc,
    clim_concern_wtp,
    eu_clim_conc,
    personal_income,
    flying_recent,
    flying_recent_number,
    add_cost
  )
)

df_demo <- df_demo |>
  mutate(across(
    c(clim_concern_wtc, clim_concern_wtp, eu_clim_conc),
    ~ case_when(
      .x %in% c("not_at_all_worried", "very_unimportant") ~ 0,
      .x %in% c("unimportant") ~ 1,
      .x %in% c("not_very_worried") ~ 1.67,
      .x %in% c("somewhat_unimportant") ~ 2,
      .x %in% c("somewhat_important") ~ 3,
      .x %in% c("somewhat_worried") ~ 3.33,
      .x %in% c("important") ~ 4,
      .x %in% c("extremely_worried", "very_important") ~ 5,
      .x %in% c("prefer_not_to_say") ~ NA_real_,
      TRUE ~ NA_real_
    )
  )) |>
  mutate(
    clim_concern_score = clim_concern_wtc + clim_concern_wtp + eu_clim_conc
  ) |>
  mutate(
    income_decile = case_when(
      country == "CH" & personal_income == "25k_35k" ~ 2,
      country == "US" & personal_income == "25k_35k" ~ 3,
      country == "CH" & personal_income == "35k_45k" ~ 3,
      country == "US" & personal_income == "35k_45k" ~ 4,
      country == "CH" & personal_income == "45k_55k" ~ 4,
      country == "US" & personal_income == "45k_55k" ~ 5,

      personal_income %in% c("below_15k", "25k_below", "10k_below") ~ 1,
      personal_income %in% c("15k_25k", "10k_20k") ~ 2,
      personal_income %in% c("20k_30k") ~ 3,
      personal_income %in% c("30k_45k") ~ 4,
      personal_income %in% c("55k_65k", "45k_60k") ~ 5,
      personal_income %in% c("55k_70k", "65k_80k", "60k_75k") ~ 6,
      personal_income %in% c("70k_90k", "80k_95k", "75k_100k") ~ 7,
      personal_income %in% c("90k_115k", "95k_115k", "100k_130k") ~ 8,
      personal_income %in% c("115k_175k", "115k_150k", "130k_180k") ~ 9,
      personal_income %in% c("175k_above", "above_150k", "above_180k") ~ 10,
      personal_income %in% c("prefer_not_to_say") ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

df_demo_tidy <- df_tidy |>
  left_join(df_demo, by = c("id", "country"))

write_csv(df_demo_tidy, here("data", "wtc_wtp_controls_tidy.csv"))
