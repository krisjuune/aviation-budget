library(lme4)
library(emmeans)
library(ggeffects)
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

if (exists("snakemake")) {
  controls_file <- snakemake@input[["controls"]]
  fair_file     <- snakemake@input[["fair"]]
  out_emm_wtc_income      <- snakemake@output[["emm_wtc_income"]]
  out_emm_wtp_income      <- snakemake@output[["emm_wtp_income"]]
  out_contr_wtc_income    <- snakemake@output[["contr_wtc_income"]]
  out_contr_wtp_income    <- snakemake@output[["contr_wtp_income"]]
  out_contr_flights_income <- snakemake@output[["contr_flights_income"]]
  out_emm_wtc_flier       <- snakemake@output[["emm_wtc_flier"]]
  out_emm_wtp_flier       <- snakemake@output[["emm_wtp_flier"]]
  out_contr_wtc_flier     <- snakemake@output[["contr_wtc_flier"]]
  out_contr_wtp_flier     <- snakemake@output[["contr_wtp_flier"]]
  out_contr_flights_flier <- snakemake@output[["contr_flights_flier"]]
  out_emm_wtc_clim        <- snakemake@output[["emm_wtc_clim"]]
  out_emm_wtp_clim        <- snakemake@output[["emm_wtp_clim"]]
  out_contr_wtc_clim      <- snakemake@output[["contr_wtc_clim"]]
  out_contr_wtp_clim      <- snakemake@output[["contr_wtp_clim"]]
  out_contr_flights_clim  <- snakemake@output[["contr_flights_clim"]]
} else {
  controls_file <- here("data", "wtc_wtp_controls_tidy.csv")
  fair_file     <- here("data", "wtc_wtp_fair_tidy.csv")
  out_emm_wtc_income       <- here("data", "emm_wtc_income.csv")
  out_emm_wtp_income       <- here("data", "emm_wtp_income.csv")
  out_contr_wtc_income     <- here("data", "contr_wtc_income.csv")
  out_contr_wtp_income     <- here("data", "contr_wtp_income.csv")
  out_contr_flights_income <- here("data", "contr_flights_income.csv")
  out_emm_wtc_flier        <- here("data", "emm_wtc_flier.csv")
  out_emm_wtp_flier        <- here("data", "emm_wtp_flier.csv")
  out_contr_wtc_flier      <- here("data", "contr_wtc_flier.csv")
  out_contr_wtp_flier      <- here("data", "contr_wtp_flier.csv")
  out_contr_flights_flier  <- here("data", "contr_flights_flier.csv")
  out_emm_wtc_clim         <- here("data", "emm_wtc_clim.csv")
  out_emm_wtp_clim         <- here("data", "emm_wtp_clim.csv")
  out_contr_wtc_clim       <- here("data", "contr_wtc_clim.csv")
  out_contr_wtp_clim       <- here("data", "contr_wtp_clim.csv")
  out_contr_flights_clim   <- here("data", "contr_flights_clim.csv")
}

data_controls <- read_csv(controls_file, show_col_types = FALSE) |>
  filter(!is.na(wtc)) |>
  mutate(
    income_group = case_when(
      income_decile %in% 1:3 ~ "low",
      income_decile %in% 4:7 ~ "mid",
      income_decile %in% 8:10 ~ "high",
      TRUE ~ NA_character_
    ),
    income_group = factor(income_group, levels = c("low", "mid", "high")),
    flying_group = case_when(
      flying_recent == "no" | flying_ever == "no" ~ "non-flier",
      flying_recent_number < 6 ~ "average flier",
      flying_recent_number >= 6 ~ "frequent flier",
      TRUE ~ NA_character_
    ),
    flying_group = factor(
      flying_group,
      levels = c("non-flier", "average flier", "frequent flier")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Control"         = "control",
        "Egalitarianism"  = "egal",
        "Limitarianism"   = "limit",
        "Prioritarianism" = "prior",
        "Proportionalism" = "prop"
      ) |>
      fct_relevel("Control"),
    time = factor(time, levels = c("post", "pre")),
    clim_concern = case_when(
      is.na(clim_concern_score) ~ NA_character_,
      clim_concern_score <= quantile(clim_concern_score, 0.33, na.rm = TRUE) ~ "low",
      clim_concern_score <= quantile(clim_concern_score, 0.67, na.rm = TRUE) ~ "mid",
      TRUE ~ "high"
    ),
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"))
  )

data_fair <- read_csv(fair_file, show_col_types = FALSE)

fair_vars <- data_fair |>
  select(id, fair_group_wtp, fair_self_wtp, fair_group_wtc, fair_self_wtc) |>
  distinct()

data_controls <- data_controls |>
  left_join(fair_vars, by = "id")

#################### helper function ###############################

add_group_n <- function(emm_obj, data, by) {
  by <- as.character(by)
  group_sizes <- data |>
    filter(!is.na(.data[[by]])) |>
    distinct(id, .data[[by]]) |>
    group_by(.data[[by]]) |>
    summarise(n = n(), .groups = "drop")
  as.data.frame(emm_obj) |>
    as_tibble() |>
    left_join(group_sizes, by = by)
}

#################### income models ###############################

model_wtc <- lmer(
  wtc ~ treatment * income_group + (1 | country),
  data = data_controls
)
model_wtp <- lmer(
  wtp ~ treatment * income_group + relative_added_cost + (1 | country),
  data = data_controls
)
model_flights <- lmer(
  planned_flights ~ time * treatment * income_group + (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_income    <- emmeans(model_wtc, ~ income_group * treatment)
emm_wtp_income    <- emmeans(model_wtp, ~ income_group * treatment)
emm_time_income   <- emmeans(model_flights, ~ time, by = c("income_group", "treatment"))

contr_wtc_income <- contrast(emm_wtc_income, method = "trt.vs.ctrl", ref = 1, by = "income_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_income <- contrast(emm_wtp_income, method = "trt.vs.ctrl", ref = 1, by = "income_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_delta_income <- contrast(emm_time_income, method = list("delta" = c(-1, 1)), by = c("treatment", "income_group"))
contr_flights_income <- contrast(emm_delta_income, method = "trt.vs.ctrl", ref = 1, by = "income_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_wtc_income   <- add_group_n(emm_wtc_income,   data_controls, "income_group")
emm_wtp_income   <- add_group_n(emm_wtp_income,   data_controls, "income_group")
contr_wtc_income <- add_group_n(contr_wtc_income, data_controls, "income_group")
contr_wtp_income <- add_group_n(contr_wtp_income, data_controls, "income_group")

#################### flying group models ###############################

model_wtc <- lmer(
  wtc ~ treatment * flying_group + (1 | country),
  data = data_controls
)
model_wtp <- lmer(
  wtp ~ treatment * flying_group + relative_added_cost + (1 | country),
  data = data_controls
)
model_flights <- lmer(
  planned_flights ~ time * treatment * flying_group + (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_flier  <- emmeans(model_wtc, ~ flying_group * treatment)
emm_wtp_flier  <- emmeans(model_wtp, ~ flying_group * treatment)
emm_time_flier <- emmeans(model_flights, ~ time, by = c("flying_group", "treatment"))

contr_wtc_flier <- contrast(emm_wtc_flier, method = "trt.vs.ctrl", ref = 1, by = "flying_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_flier <- contrast(emm_wtp_flier, method = "trt.vs.ctrl", ref = 1, by = "flying_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_delta_flier <- contrast(emm_time_flier, method = list("delta" = c(-1, 1)), by = c("treatment", "flying_group"))
contr_flights_flier <- contrast(emm_delta_flier, method = "trt.vs.ctrl", ref = 1, by = "flying_group") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_wtc_flier   <- add_group_n(emm_wtc_flier,   data_controls, "flying_group")
emm_wtp_flier   <- add_group_n(emm_wtp_flier,   data_controls, "flying_group")
contr_wtc_flier <- add_group_n(contr_wtc_flier, data_controls, "flying_group")
contr_wtp_flier <- add_group_n(contr_wtp_flier, data_controls, "flying_group")

#################### climate concern models ###############################

model_wtc <- lmer(
  wtc ~ treatment * clim_concern + (1 | country),
  data = data_controls
)
model_wtp <- lmer(
  wtp ~ treatment * clim_concern + relative_added_cost + (1 | country),
  data = data_controls
)
model_flights <- lmer(
  planned_flights ~ time * treatment * clim_concern + (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_clim  <- emmeans(model_wtc, ~ clim_concern * treatment)
emm_wtp_clim  <- emmeans(model_wtp, ~ clim_concern * treatment)
emm_time_clim <- emmeans(model_flights, ~ time, by = c("clim_concern", "treatment"))

contr_wtc_clim <- contrast(emm_wtc_clim, method = "trt.vs.ctrl", ref = 1, by = "clim_concern") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_clim <- contrast(emm_wtp_clim, method = "trt.vs.ctrl", ref = 1, by = "clim_concern") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_delta_clim <- contrast(emm_time_clim, method = list("delta" = c(1, -1)), by = c("treatment", "clim_concern"))
contr_flights_clim <- contrast(emm_delta_clim, method = "trt.vs.ctrl", ref = 1, by = "clim_concern") |>
  as.data.frame() |> as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_wtc_clim   <- add_group_n(emm_wtc_clim,   data_controls, "clim_concern")
emm_wtp_clim   <- add_group_n(emm_wtp_clim,   data_controls, "clim_concern")
contr_wtc_clim <- add_group_n(contr_wtc_clim, data_controls, "clim_concern")
contr_wtp_clim <- add_group_n(contr_wtp_clim, data_controls, "clim_concern")

#################### save outputs ###############################

write.csv(emm_wtc_income,        out_emm_wtc_income)
write.csv(emm_wtp_income,        out_emm_wtp_income)
write.csv(contr_wtc_income,      out_contr_wtc_income)
write.csv(contr_wtp_income,      out_contr_wtp_income)
write.csv(contr_flights_income,  out_contr_flights_income)
write.csv(emm_wtc_flier,         out_emm_wtc_flier)
write.csv(emm_wtp_flier,         out_emm_wtp_flier)
write.csv(contr_wtc_flier,       out_contr_wtc_flier)
write.csv(contr_wtp_flier,       out_contr_wtp_flier)
write.csv(contr_flights_flier,   out_contr_flights_flier)
write.csv(emm_wtc_clim,          out_emm_wtc_clim)
write.csv(emm_wtp_clim,          out_emm_wtp_clim)
write.csv(contr_wtc_clim,        out_contr_wtc_clim)
write.csv(contr_wtp_clim,        out_contr_wtp_clim)
write.csv(contr_flights_clim,    out_contr_flights_clim)