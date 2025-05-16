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
source(here("scripts", "lmm_simple_models.R"))

data_fair <- read_csv(
    here("data", "wtc_wtp_fair_tidy.csv"),
    show_col_types = FALSE
) |>
  filter(!is.na(wtc))

###### run model 

model <- lmer(
  wtc ~ treatment * red_amt +
    fair_group_wtc + fair_self_wtc +
    (1 | country),
  data = data_fair
)

# check model
summary(model)
group_trend <- emtrends(model, ~1, var = "fair_group_wtc")
self_trend  <- emtrends(model, ~1, var = "fair_self_wtc")
# main fairness WTC resuls:
group_trend
self_trend

# run with fairness as outcome variable
model_fair_group <- lmer(
  fair_group_wtc ~ treatment * red_amt + (1 | country),
  data = data_fair
)
model_fair_self <- lmer(
  fair_self_wtc ~ treatment * red_amt + (1 | country),
  data = data_fair
)
summary(model_fair_group)
summary(model_fair_self)
emm_group <- emmeans(model_fair_group, ~ treatment * red_amt)
emm_self  <- emmeans(model_fair_self, ~ treatment * red_amt)
pairs(emm_group)
pairs(emm_self)

# plot fairness results
plot_self_fair <- plot_emmeans(emm_self, plot_30 = FALSE) +
  scale_y_continuous(limits = c(1, 4))
plot_self_contrasts <- plot_pairwise_contrasts(
  emm_self,
  plot_30 = FALSE,
  no_control = TRUE
) +
  scale_x_continuous(limits = c(-1, 1))

plot_group_fair <- emm_group |>
  plot_emmeans(plot_30 = FALSE) +
  scale_y_continuous(limits = c(1, 4))
plot_group_contrasts <- plot_pairwise_contrasts(
  emm_group,
  plot_30 = FALSE,
  no_control = TRUE,
  alpha = .5
) +
  scale_x_continuous(limits = c(-1.2, 1.2))



# contrasts
emm_group_pairs <- as.data.frame(pairs(emm_group))
emm_group_pairs$fairness_type <- "Group fairness"
emm_self_pairs <- as.data.frame(pairs(emm_self))
emm_self_pairs$fairness_type <- "Self fairness"
all_pairs <- rbind(emm_group_pairs, emm_self_pairs)
all_pairs$contrast <- factor(all_pairs$contrast, levels = unique(all_pairs$contrast[order(all_pairs$estimate)]))
ggplot(all_pairs, aes(x = estimate, y = contrast, color = fairness_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96 * SE, xmax = estimate + 1.96 * SE), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Estimated fairness difference (contrast)",
    y = "Treatment contrast",
    title = "Pairwise contrasts of WTC treatment fairness ratings",
    color = "Fairness Type"
  ) +
  theme_minimal()



model <- lmer(
  wtp ~ treatment * red_amt +
    fair_group_wtp + fair_self_wtp +
    (1 | country),
  data = data_fair
)
summary(model)
group_trend <- emtrends(model, ~1, var = "fair_group_wtc")
self_trend  <- emtrends(model, ~1, var = "fair_self_wtc")
# main fairness WTP resuls:
group_trend
self_trend


model_fair_group <- lmer(
  fair_group_wtp ~ treatment * red_amt + (1 | country),
  data = data_fair
)
model_fair_self <- lmer(
  fair_self_wtp ~ treatment * red_amt + (1 | country),
  data = data_fair
)
summary(model_fair_group)
summary(model_fair_self)
emm_group <- emmeans(model_fair_group, ~ treatment)
emm_self  <- emmeans(model_fair_self, ~ treatment)
pairs(emm_group)
pairs(emm_self)

# contrasts
emm_group_pairs <- as.data.frame(pairs(emm_group))
emm_group_pairs$fairness_type <- "Group fairness"
emm_self_pairs <- as.data.frame(pairs(emm_self))
emm_self_pairs$fairness_type <- "Self fairness"
all_pairs <- rbind(emm_group_pairs, emm_self_pairs)
all_pairs$contrast <- factor(all_pairs$contrast, levels = unique(all_pairs$contrast[order(all_pairs$estimate)]))
ggplot(all_pairs, aes(x = estimate, y = contrast, color = fairness_type)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96 * SE, xmax = estimate + 1.96 * SE), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x = "Estimated fairness difference (contrast)",
    y = "Treatment contrast",
    title = "Pairwise contrasts of WTP treatment fairness ratings",
    color = "Fairness Type"
  ) +
  theme_minimal()