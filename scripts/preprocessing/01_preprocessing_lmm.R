library(tidyverse)
library(here)
library(stringr)
library(dplyr)
library(purrr)
library(janitor)
library(tibble)

if (exists("snakemake")) {
  clean_us   <- snakemake@input[["us"]]
  clean_ch   <- snakemake@input[["ch"]]
  clean_cn   <- snakemake@input[["cn"]]
  out_tidy        <- snakemake@output[["wtc_wtp"]]
  out_fair        <- snakemake@output[["wtc_wtp_fair"]]
  out_controls    <- snakemake@output[["wtc_wtp_controls"]]
} else {
  clean_us   <- here("data", "data_clean_us.csv")
  clean_ch   <- here("data", "data_clean_ch.csv")
  clean_cn   <- here("data", "data_clean_cn.csv")
  out_tidy        <- here("data", "wtc_wtp_tidy.csv")
  out_fair        <- here("data", "wtc_wtp_fair_tidy.csv")
  out_controls    <- here("data", "wtc_wtp_controls_tidy.csv")
}

df_us <- read_csv(clean_us, show_col_types = FALSE) |> mutate(country = "us")
df_ch <- read_csv(clean_ch, show_col_types = FALSE) |> mutate(country = "ch")
df_cn <- read_csv(clean_cn, show_col_types = FALSE) |> mutate(country = "cn")

###################### remove outliers ####################

cutoff <- 200

df_cn <- df_cn |>
  filter(
    is.na(flying_recent_number) | flying_recent_number <= cutoff
  )

###################### wtc, wtp, flights ##################

ticket_lookup <- tribble(
  ~country, ~route_length, ~ticket_cost,
  "us",     "short",       150,
  "us",     "long",        400,
  "ch",     "short",       130,
  "ch",     "long",        350,
  "cn",     "short",       1000,
  "cn",     "long",        2140
)

reconstruct_add_cost_ch <- function(df) {
  df |> mutate(add_cost = case_when(
    treatment == "control" ~ 0,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "low"  ~ 7,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "mid"  ~ 18,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "high" ~ 31,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "low"  ~ 8,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "mid"  ~ 21,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "high" ~ 36,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "low"  ~ 10,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "mid"  ~ 24,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "high" ~ 41,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "low"  ~ 24,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "mid"  ~ 58,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "high" ~ 100,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "low"  ~ 27,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "mid"  ~ 67,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "high" ~ 115,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "low"  ~ 31,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "mid"  ~ 77,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "high" ~ 132,
    treatment == "limit" & route_length == "short" & red_amt == "15%" & planned_flight > limit_15_high ~ 43,
    treatment == "limit" & route_length == "short" & red_amt == "15%"                                  ~ 0,
    treatment == "limit" & route_length == "short" & red_amt == "30%" & planned_flight > limit_30_high ~ 50,
    treatment == "limit" & route_length == "short" & red_amt == "30%"                                  ~ 0,
    treatment == "limit" & route_length == "short" & red_amt == "45%" & planned_flight > limit_45_high ~ 57,
    treatment == "limit" & route_length == "short" & red_amt == "45%"                                  ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "15%" & planned_flight > limit_15_high ~ 139,
    treatment == "limit" & route_length == "long"  & red_amt == "15%"                                  ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "30%" & planned_flight > limit_30_high ~ 160,
    treatment == "limit" & route_length == "long"  & red_amt == "30%"                                  ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "45%" & planned_flight > limit_45_high ~ 184,
    treatment == "limit" & route_length == "long"  & red_amt == "45%"                                  ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "15%" & flight_tour > 0  ~ 48,
    treatment == "prior" & route_length == "short" & red_amt == "15%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "30%" & flight_tour > 0  ~ 55,
    treatment == "prior" & route_length == "short" & red_amt == "30%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "45%" & flight_tour > 0  ~ 63,
    treatment == "prior" & route_length == "short" & red_amt == "45%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "15%" & flight_tour > 0  ~ 153,
    treatment == "prior" & route_length == "long"  & red_amt == "15%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "30%" & flight_tour > 0  ~ 176,
    treatment == "prior" & route_length == "long"  & red_amt == "30%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "45%" & flight_tour > 0  ~ 203,
    treatment == "prior" & route_length == "long"  & red_amt == "45%" & flight_tour == 0 ~ 0,
    treatment == "prop"  & route_length == "short" & red_amt == "15%" ~ 24,
    treatment == "prop"  & route_length == "short" & red_amt == "30%" ~ 27,
    treatment == "prop"  & route_length == "short" & red_amt == "45%" ~ 31,
    treatment == "prop"  & route_length == "long"  & red_amt == "15%" ~ 77,
    treatment == "prop"  & route_length == "long"  & red_amt == "30%" ~ 88,
    treatment == "prop"  & route_length == "long"  & red_amt == "45%" ~ 101,
    TRUE ~ NA_real_
  ))
}

reconstruct_add_cost_cn <- function(df) {
  df |> mutate(add_cost = case_when(
    treatment == "control" ~ 0,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "low"  ~ 61,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "mid"  ~ 149,
    treatment == "egal" & route_length == "short" & red_amt == "15%" & income == "high" ~ 257,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "low"  ~ 70,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "mid"  ~ 172,
    treatment == "egal" & route_length == "short" & red_amt == "30%" & income == "high" ~ 295,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "low"  ~ 80,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "mid"  ~ 197,
    treatment == "egal" & route_length == "short" & red_amt == "45%" & income == "high" ~ 339,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "low"  ~ 194,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "mid"  ~ 479,
    treatment == "egal" & route_length == "long"  & red_amt == "15%" & income == "high" ~ 823,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "low"  ~ 224,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "mid"  ~ 550,
    treatment == "egal" & route_length == "long"  & red_amt == "30%" & income == "high" ~ 946,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "low"  ~ 257,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "mid"  ~ 633,
    treatment == "egal" & route_length == "long"  & red_amt == "45%" & income == "high" ~ 1087,
    treatment == "limit" & route_length == "short" & red_amt == "15%" & planned_flight > limit_high ~ 356,
    treatment == "limit" & route_length == "short" & red_amt == "15%"                               ~ 0,
    treatment == "limit" & route_length == "short" & red_amt == "30%" & planned_flight > limit_high ~ 410,
    treatment == "limit" & route_length == "short" & red_amt == "30%"                               ~ 0,
    treatment == "limit" & route_length == "short" & red_amt == "45%" & planned_flight > limit_high ~ 471,
    treatment == "limit" & route_length == "short" & red_amt == "45%"                               ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "15%" & planned_flight > limit_high ~ 1147,
    treatment == "limit" & route_length == "long"  & red_amt == "15%"                               ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "30%" & planned_flight > limit_high ~ 1319,
    treatment == "limit" & route_length == "long"  & red_amt == "30%"                               ~ 0,
    treatment == "limit" & route_length == "long"  & red_amt == "45%" & planned_flight > limit_high ~ 1516,
    treatment == "limit" & route_length == "long"  & red_amt == "45%"                               ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "15%" & flight_tour > 0  ~ 392,
    treatment == "prior" & route_length == "short" & red_amt == "15%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "30%" & flight_tour > 0  ~ 451,
    treatment == "prior" & route_length == "short" & red_amt == "30%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "short" & red_amt == "45%" & flight_tour > 0  ~ 518,
    treatment == "prior" & route_length == "short" & red_amt == "45%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "15%" & flight_tour > 0  ~ 1262,
    treatment == "prior" & route_length == "long"  & red_amt == "15%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "30%" & flight_tour > 0  ~ 1451,
    treatment == "prior" & route_length == "long"  & red_amt == "30%" & flight_tour == 0 ~ 0,
    treatment == "prior" & route_length == "long"  & red_amt == "45%" & flight_tour > 0  ~ 1668,
    treatment == "prior" & route_length == "long"  & red_amt == "45%" & flight_tour == 0 ~ 0,
    treatment == "prop"  & route_length == "short" & red_amt == "15%" ~ 196,
    treatment == "prop"  & route_length == "short" & red_amt == "30%" ~ 225,
    treatment == "prop"  & route_length == "short" & red_amt == "45%" ~ 259,
    treatment == "prop"  & route_length == "long"  & red_amt == "15%" ~ 631,
    treatment == "prop"  & route_length == "long"  & red_amt == "30%" ~ 725,
    treatment == "prop"  & route_length == "long"  & red_amt == "45%" ~ 834,
    TRUE ~ NA_real_
  ))
}

df_ch <- reconstruct_add_cost_ch(df_ch)
df_cn <- reconstruct_add_cost_cn(df_cn)

var_list <- c(
  "id", "country",
  "c_wtc_fly", "t_wtc_fly",
  "c_wtp_buy", "t_wtp_buy",
  "planned_flights", "c_wtc_fly_number", "t_wtc_fly_number",
  "treatment", "red_amt",
  "route_length", "add_cost"
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
    treatment, red_amt,
    route_length, add_cost
  )

df_tidy <- df_tidy |>
  mutate(
    add_cost = if_else(treatment == "control", 0, add_cost)
  ) |>
  left_join(ticket_lookup, by = c("country", "route_length")) |>
  mutate(
    relative_added_cost = add_cost / ticket_cost
  )

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

write_csv(df_tidy, out_tidy)

###################### fairness #########################

fair_self_group_regex <- "(?=.*fair)(?=.*(self|group))"
df_fair <- map_dfr(
  list(df_us, df_ch, df_cn),
  ~ select(.x, all_of(var_list), matches(fair_self_group_regex, perl = TRUE))
)

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
  mutate(
    fair_group_wtc = str_to_lower(fair_group_wtc),
    fair_group_wtc = str_replace_all(fair_group_wtc, " ", "_"),
    fair_self_wtc  = str_to_lower(fair_self_wtc),
    fair_self_wtc  = str_replace_all(fair_self_wtc, " ", "_"),
    fair_group_wtp = str_to_lower(fair_group_wtp),
    fair_group_wtp = str_replace_all(fair_group_wtp, " ", "_"),
    fair_self_wtp  = str_to_lower(fair_self_wtp),
    fair_self_wtp  = str_replace_all(fair_self_wtp, " ", "_")
  )

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

write_csv(df_fair, out_fair)

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
    flying_ever,
    flying_recent,
    flying_recent_number,
    flying_purpose
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
      country == "ch" & personal_income == "25k_35k" ~ 2,
      country == "us" & personal_income == "25k_35k" ~ 3,
      country == "ch" & personal_income == "35k_45k" ~ 3,
      country == "us" & personal_income == "35k_45k" ~ 4,
      country == "ch" & personal_income == "45k_55k" ~ 4,
      country == "us" & personal_income == "45k_55k" ~ 5,
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

write_csv(df_demo_tidy, out_controls)