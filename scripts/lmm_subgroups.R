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

#flying_recent_number quantiles not accounting for those who did not fly at all
#25% 50% 75%
#  2   4   6
#non-flyer  average flyer frequent flyer (>6)
#   1172           3516            790
#low  mid high
#730 2086 1684


data_controls <- read_csv(
  here("data", "wtc_wtp_controls_tidy.csv"), show_col_types = FALSE
) |>
  filter(!is.na(wtc)) |>
  mutate(
    income_group = case_when(
      income_decile %in% 1:3 ~ "low",
      income_decile %in% 4:7 ~ "mid",
      income_decile %in% 8:10 ~ "high",
      TRUE ~ NA_character_ 
    ),
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high")
    ),
    flying_group = case_when(
      flying_recent == "no" ~ "non-flyer",
      flying_recent_number <= 6 ~ "average flyer",
      flying_recent_number > 6 ~ "frequent flyer",
      TRUE ~ NA_character_
    ),
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Control" = "control",
        "Egalitarianism" = "egal",
        "Limitarianism" = "limit",
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

data_fair <- read_csv(
  here("data", "wtc_wtp_fair_tidy.csv"),
  show_col_types = FALSE
)

fair_vars <- data_fair |>
  select(
    id,
    fair_group_wtp,
    fair_self_wtp,
    fair_group_wtc,
    fair_self_wtc
  ) |>
  distinct()

data_controls <- data_controls |>
  left_join(fair_vars, by = "id")

#################### models ###############################

table(data_controls$flying_group)
table(data_controls$income_group)
table(data_controls$clim_concern)

model_wtc <- lmer(
  wtc ~ treatment * income_group +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * income_group + relative_added_cost +
    (1 | country),
  data = data_controls
)

model_flights <- lmer(
  planned_flights ~ time * treatment * income_group +
    (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_income <- emmeans(model_wtc, ~ income_group * treatment)
emm_wtp_income <- emmeans(model_wtp, ~ income_group * treatment)
emm_flights_income <- emmeans(model_flights, ~ time | income_group * treatment)
emm_time_income <- emmeans(
  model_flights,
  ~ time,
  by = c("income_group", "treatment")
)

model_wtc <- lmer(
  wtc ~ treatment * flying_group +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * flying_group + relative_added_cost +
    (1 | country),
  data = data_controls
)

model_flights <- lmer(
  planned_flights ~ time * treatment * flying_group +
    (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_flyer <- emmeans(model_wtc, ~ flying_group * treatment)
emm_wtp_flyer <- emmeans(model_wtp, ~ flying_group * treatment)
emm_flights_flyer <- emmeans(model_flights, ~ time | flying_group * treatment)
emm_time_flyer <- emmeans(
  model_flights,
  ~ time,
  by = c("flying_group", "treatment")
)



model_wtc <- lmer(
  wtc ~ treatment * clim_concern +
    (1 | country),
  data = data_controls
)

model_wtp <- lmer(
  wtp ~ treatment * clim_concern + relative_added_cost +
    (1 | country),
  data = data_controls
)

model_flights <- lmer(
  planned_flights ~ time * treatment * clim_concern +
    (1 | id) + (1 | country),
  data = data_controls
)

emm_wtc_clim <- emmeans(model_wtc, ~ clim_concern * treatment)
emm_wtp_clim <- emmeans(model_wtp, ~ clim_concern * treatment)
emm_flights_clim <- emmeans(model_flights, ~ time | clim_concern * treatment)
emm_time_clim <- emmeans(
  model_flights,
  ~ time,
  by = c("clim_concern", "treatment")
)


# get contrasts against control
contr_wtc_income <- contrast(
  emm_wtc_income, method = "trt.vs.ctrl",
  ref = 1,
  by = "income_group"
) |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_income <- contrast(
  emm_wtp_income, method = "trt.vs.ctrl",
  ref = 1,
  by = "income_group"
)  |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtc_flyer <- contrast(
  emm_wtc_flyer, method = "trt.vs.ctrl",
  ref = 1,
  by = "flying_group"
)  |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_flyer <- contrast(
  emm_wtp_flyer, method = "trt.vs.ctrl",
  ref = 1,
  by = "flying_group"
)  |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtc_clim <- contrast(
  emm_wtc_clim, method = "trt.vs.ctrl",
  ref = 1,
  by = "clim_concern"
)  |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

contr_wtp_clim <- contrast(
  emm_wtp_clim, method = "trt.vs.ctrl",
  ref = 1,
  by = "clim_concern"
)  |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_delta_income <- contrast(
  emm_time_income,
  method = list("delta" = c(-1, 1)),
  by = c("treatment", "income_group")
)

contr_flights_income <- contrast(
  emm_delta_income,
  method = "trt.vs.ctrl",
  ref = 1,
  by = "income_group"
) |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

emm_delta_flyer <- contrast(
  emm_time_flyer,
  method = list("delta" = c(-1, 1)),
  by = c("treatment", "flying_group")
)

contr_flights_flyer <- contrast(
  emm_delta_flyer,
  method = "trt.vs.ctrl",
  ref = 1,
  by = "flying_group"
) |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

#TODO check order of 1 -1 here and why
emm_delta_clim <- contrast(
  emm_time_clim,
  method = list("delta" = c(1, -1)),
  by = c("treatment", "clim_concern")
)

contr_flights_clim <- contrast(
  emm_delta_clim,
  method = "trt.vs.ctrl",
  ref = 1,
  by = "clim_concern"
) |>
  as.data.frame() |>
  as_tibble() |>
  mutate(
    treatment = str_replace(contrast, "\\s*-\\s*control\\s*$", ""),
    treatment = factor(treatment, levels = c("egal", "limit", "prior", "prop"))
  )

add_group_n <- function(emm_obj, data, by) {
  by <- as.character(by)
  
  group_sizes <- data |>
    filter(!is.na(.data[[by]])) |>
    distinct(id, .data[[by]]) |>
    group_by(.data[[by]]) |>
    summarise(n = n(), .groups = "drop")
  
  emm_df <- as.data.frame(emm_obj) |>
    as_tibble()
  
  emm_df <- emm_df |>
    left_join(group_sizes, by = by)
  
  return(emm_df)
}

emm_wtc_income <- add_group_n(emm_wtc_income, data_controls, by = "income_group")
emm_wtp_income <- add_group_n(emm_wtp_income, data_controls, by = "income_group")
contr_wtc_income <- add_group_n(contr_wtc_income, data_controls, by = "income_group")
contr_wtp_income <- add_group_n(contr_wtp_income, data_controls, by = "income_group")

emm_wtc_flyer <- add_group_n(emm_wtc_flyer, data_controls, by = "flying_group")
emm_wtp_flyer <- add_group_n(emm_wtp_flyer, data_controls, by = "flying_group")
contr_wtc_flyer <- add_group_n(contr_wtc_flyer, data_controls, by = "flying_group")
contr_wtp_flyer <- add_group_n(contr_wtp_flyer, data_controls, by = "flying_group")

emm_wtc_clim <- add_group_n(emm_wtc_clim, data_controls, by = "clim_concern")
emm_wtp_clim <- add_group_n(emm_wtp_clim, data_controls, by = "clim_concern")
contr_wtc_clim <- add_group_n(contr_wtc_clim, data_controls, by = "clim_concern")
contr_wtp_clim <- add_group_n(contr_wtp_clim, data_controls, by = "clim_concern")

write.csv(emm_wtc_income, here("data", "emm_wtc_income.csv"))
write.csv(emm_wtp_income, here("data", "emm_wtp_income.csv"))
write.csv(contr_wtc_income, here("data", "contr_wtc_income.csv"))
write.csv(contr_wtp_income, here("data", "contr_wtp_income.csv"))
write.csv(contr_flights_income, here("data", "contr_flights_income.csv"))

write.csv(emm_wtc_flyer, here("data", "emm_wtc_flyer.csv"))
write.csv(emm_wtp_flyer, here("data", "emm_wtp_flyer.csv"))
write.csv(contr_wtc_flyer, here("data", "contr_wtc_flyer.csv"))
write.csv(contr_wtp_flyer, here("data", "contr_wtp_flyer.csv"))
write.csv(contr_flights_flyer, here("data", "contr_flights_flyer.csv"))

write.csv(emm_wtc_clim, here("data", "emm_wtc_clim.csv"))
write.csv(emm_wtp_clim, here("data", "emm_wtp_clim.csv"))
write.csv(contr_wtc_clim, here("data", "contr_wtc_clim.csv"))
write.csv(contr_wtp_clim, here("data", "contr_wtp_clim.csv"))
write.csv(contr_flights_clim, here("data", "contr_flights_clim.csv"))