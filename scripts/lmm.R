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

data <- read_csv(here("data", "wtc_wtp_tidy.csv"))
data_fair <- read_csv(here("data", "wtc_wtp_fair_tidy.csv"))

data_flights <- data |>
  filter(!is.na(wtc))

data_fair_flights <- data_fair |>
  filter(!is.na(wtc))


################## basic analysis #####################
model <- lmer(
  wtc ~ treatment * red_amt + (1 | country),
  data = data_flights
)
summary(model)
emm <- emmeans(model, ~ red_amt * treatment)
summary(emm)
contrast(emm, method = "pairwise", by = "red_amt")
plot(emm, comparisons = TRUE, by = "red_amt")
pwpp(emm, by = "red_amt")


model <- lmer(
  wtp ~ treatment * red_amt + (1 | country),
  data = data
)
summary(model)
emm <- emmeans(model, ~ treatment * red_amt)
summary(emm)
contrast(emm, method = "pairwise", by = "red_amt")
plot(emm, comparisons = TRUE, by = "red_amt")
pwpp(emm, by = "red_amt")




model <- lmer(
  planned_flights ~ time * treatment * red_amt + (1 | id) + (1 | country),
  data = data_flights
)
summary(model)
emm <- emmeans(model, ~ time * treatment * red_amt)
pairs(emm, by = c("treatment", "red_amt"))
emmip(model, treatment ~ time | red_amt, CIs = TRUE)
plot(emm, comparisons = TRUE, by = "time")
pwpp(emm, by = c("time", "red_amt"))

#################### assumptions #######################

# check whether assumptions of normally distirbuted
# residuals and random effects hold

qqnorm(resid(model))
qqline(resid(model))
plot(model)

ranef_vals <- ranef(model)$country[, "(Intercept)"]
qqnorm(ranef_vals)
qqline(ranef_vals)
check_model(model)

# can drop the interaction of the factors to resolve
# high collinearity


################### test Bayesian ######################

library(brms)
brm(wtc ~ treatment * red_amt + (1 | country), data = data_flights)

###################### fairness #########################
model <- lmer(
  wtc ~ treatment * red_amt +
    fair_group_wtc + fair_self_wtc +
    (1 | country),
  data = data_fair_flights
)
summary(model)
group_trend <- emtrends(model, ~1, var = "fair_group_wtc")
self_trend  <- emtrends(model, ~1, var = "fair_self_wtc")
# main fairness WTC resuls:
group_trend
self_trend


model_fair_group <- lmer(
  fair_group_wtc ~ treatment * red_amt + (1 | country),
  data = data_fair_flights
)
model_fair_self <- lmer(
  fair_self_wtc ~ treatment * red_amt + (1 | country),
  data = data_fair_flights
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
