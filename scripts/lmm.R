library(lme4)
library(emmeans)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)

data <- read_csv(here("data", "wtc_wtp_tidy.csv"))

model <- lmer(
  wtc ~ treatment * red_amt + (1 | country),
  data = data
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
  data = data
)
summary(model)
emm <- emmeans(model, ~ time * treatment * red_amt)
pairs(emm, by = c("treatment", "red_amt"))
emmip(model, treatment ~ time | red_amt, CIs = TRUE)
plot(emm, comparisons = TRUE, by = "time")
pwpp(emm, by = "red_amt")
