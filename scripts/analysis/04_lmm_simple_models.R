library(lme4)
library(emmeans)
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
  wtc_wtp_file   <- snakemake@input[["wtc_wtp"]]
  fair_file      <- snakemake@input[["wtc_wtp_fair"]]
  out_emm_wtc         <- snakemake@output[["emm_wtc"]]
  out_emm_wtp         <- snakemake@output[["emm_wtp"]]
  out_emm_wtc_redamt  <- snakemake@output[["emm_wtc_redamt"]]
  out_emm_wtp_redamt  <- snakemake@output[["emm_wtp_redamt"]]
  out_emm_wtp_haul    <- snakemake@output[["emm_wtp_haul"]]
  out_contr_wtc       <- snakemake@output[["contr_wtc"]]
  out_contr_wtp       <- snakemake@output[["contr_wtp"]]
  out_assumptions_wtc <- snakemake@output[["assumptions_wtc"]]
  out_assumptions_wtp <- snakemake@output[["assumptions_wtp"]]
} else {
  wtc_wtp_file   <- here("data", "wtc_wtp_tidy.csv")
  fair_file      <- here("data", "wtc_wtp_fair_tidy.csv")
  out_emm_wtc         <- here("data", "emm_wtc.csv")
  out_emm_wtp         <- here("data", "emm_wtp.csv")
  out_emm_wtc_redamt  <- here("data", "emm_wtc_redamt.csv")
  out_emm_wtp_redamt  <- here("data", "emm_wtp_redamt.csv")
  out_emm_wtp_haul    <- here("data", "emm_wtp_haul.csv")
  out_contr_wtc       <- here("data", "contr_wtc.csv")
  out_contr_wtp       <- here("data", "contr_wtp.csv")
  out_assumptions_wtc <- here("output", "assumptions_wtc.png")
  out_assumptions_wtp <- here("output", "assumptions_wtp.png")
}

data <- read_csv(wtc_wtp_file, show_col_types = FALSE)
data_fair <- read_csv(fair_file, show_col_types = FALSE)

fair_vars <- data_fair |>
  select(
    id,
    fair_group_wtp,
    fair_self_wtp,
    fair_group_wtc,
    fair_self_wtc
  ) |>
  distinct()

data_flights <- data |>
  filter(!is.na(wtc)) |>
  mutate(
    time = factor(time, levels = c("post", "pre")),
    treatment = factor(treatment) |>
      fct_recode(
        "Control"          = "control",
        "Egalitarianism"   = "egal",
        "Limitarianism"    = "limit",
        "Prioritarianism"  = "prior",
        "Proportionalism"  = "prop"
      ) |>
      fct_relevel("Control"),
    red_amt = factor(red_amt, levels = c("15%", "30%", "45%"))
  )

data_flights <- data_flights |>
  left_join(fair_vars, by = "id")

################## plot functions #####################

plot_flight_change <- function(emm, plot_30 = TRUE, main_text_size = 14) {
  diffs_df <- contrast(emm, method = "pairwise") |>
    as.data.frame()

  if (!plot_30) {
    diffs_df <- diffs_df |>
      filter(red_amt != "30%")
  }

  ggplot(diffs_df, aes(x = treatment, y = estimate, color = red_amt)) +
    geom_point(size = 3, position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * SE, ymax = estimate + 1.96 * SE),
      width = 0.2,
      position = position_dodge(0.3)
    ) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    theme_classic() +
    labs(y = NULL, x = NULL, color = "Emissions reductions") +
    ylim(1, 4) +
    theme(text = element_text(size = main_text_size))
}

################## basic analysis #####################

model_wtc <- lmer(
  wtc ~ treatment + red_amt + (1 | country),
  data = data_flights
)

emm_wtc        <- emmeans(model_wtc, ~ treatment)
emm_wtc_redamt <- emmeans(model_wtc, ~ red_amt)
contrast_wtc   <- contrast(emm_wtc, method = "trt.vs.ctrl", ref = "Control") |>
  as.data.frame() |>
  as_tibble()

model_wtp <- lmer(
  wtp ~ treatment + red_amt + relative_added_cost + (1 | country),
  data = data_flights
)

model_wtp_haul <- lmer(
  wtp ~ treatment + red_amt + route_length + relative_added_cost + (1 | country),
  data = data_flights
)

emm_wtp        <- emmeans(model_wtp, ~ treatment)
emm_wtp_redamt <- emmeans(model_wtp, ~ red_amt)
emm_wtp_haul   <- emmeans(model_wtp_haul, ~ route_length)
contrast_wtp   <- contrast(emm_wtp, method = "trt.vs.ctrl", ref = "Control") |>
  as.data.frame() |>
  as_tibble()

model <- lmer(
  planned_flights ~ time * treatment * red_amt + (1 | id) + (1 | country),
  data = data_flights
)

################## save csvs #####################

write.csv(emm_wtc,        out_emm_wtc)
write.csv(emm_wtp,        out_emm_wtp)
write.csv(emm_wtc_redamt, out_emm_wtc_redamt)
write.csv(emm_wtp_redamt, out_emm_wtp_redamt)
write.csv(emm_wtp_haul,   out_emm_wtp_haul)
write.csv(contrast_wtc,   out_contr_wtc)
write.csv(contrast_wtp,   out_contr_wtp)

#################### assumptions #######################

assumptions_check_wtc <- check_model(model_wtc)
assumptions_check_wtp <- check_model(model_wtp)

ggsave(out_assumptions_wtc, plot(assumptions_check_wtc), width = 12, height = 10)
ggsave(out_assumptions_wtp, plot(assumptions_check_wtp), width = 12, height = 10)