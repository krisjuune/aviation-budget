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

data <- read_csv(here("data", "wtc_wtp_tidy.csv"), show_col_types = FALSE)
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

data_flights <- data |>
  filter(!is.na(wtc)) |>
  mutate(
    time = factor(
      time,
      levels = c("post", "pre")
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
    red_amt = factor(
      red_amt,
      levels = c("15%", "30%", "45%")
    )
  )

data_flights <- data_flights |>
  left_join(fair_vars, by = "id")

################## plot functions #####################

plot_flight_change <- function(
  emm,
  plot_30 = TRUE,
  main_text_size = 14
) {
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
    # geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
    theme_classic() +
    labs(
      y = NULL,
      x = NULL,
      color = "Emissions reductions"
    ) +
    ylim(1, 4) +
    theme(text = element_text(size = main_text_size))
}

################## basic analysis #####################
# willingness to reduce flying
model_wtc <- lmer(
  wtc ~ treatment + red_amt + (1 | country),
  data = data_flights
)

emm_wtc <- emmeans(model_wtc, ~ treatment)
emm_wtc_redamt <- emmeans(model_wtc, ~ red_amt)
contrast_wtc <- contrast(
  emm_wtc,
  method = "trt.vs.ctrl",
  ref = "Control"
) |>
  as.data.frame() |>
  as_tibble()

# willingness to pay for SAFs
model_wtp <- lmer(
  wtp ~ treatment + red_amt + relative_added_cost + (1 | country),
  data = data_flights
)

emm_wtp <- emmeans(model_wtp, ~ treatment)
emm_wtp_redamt <- emmeans(model_wtp, ~ red_amt)
contrast_wtp <- contrast(
  emm_wtp,
  method = "trt.vs.ctrl",
  ref = "Control"
) |>
  as.data.frame() |>
  as_tibble()

# change in planned flights
model <- lmer(
  planned_flights ~ time * treatment * red_amt + (1 | id) + (1 | country),
  data = data_flights
)


# save stuff
write.csv(emm_wtc, here("data", "emm_wtc.csv"))
write.csv(emm_wtp, here("data", "emm_wtp.csv"))
write.csv(emm_wtc_redamt, here("data", "emm_wtc_redamt.csv"))
write.csv(emm_wtp_redamt, here("data", "emm_wtp_redamt.csv"))
write.csv(contrast_wtc, here("data", "contr_wtc.csv"))
write.csv(contrast_wtp, here("data", "contr_wtp.csv"))

#################### assumptions #######################

# check whether assumptions of normally distirbuted
# residuals and random effects hold
assumptions_check_wtc <- check_model(model_wtc)
assumptions_check_wtp <- check_model(model_wtp)