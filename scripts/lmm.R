library(lme4)
library(emmeans)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)

data <- read_csv(here("data", "wtc_wtp_tidy.csv")) |>
  pivot_wider(
    names_from = "outcome",
    values_from = "value"
  )

model <- lmer(
  wtc ~ treatment * red_amt + (1 | country),
  data = data
)
summary(model)
emm <- emmeans(model, ~ red_amt * treatment)
summary(emm)
contrast(emm, method = "pairwise", by = "red_amt")
plot(emm, comparisons = FALSE)
pwpp(emm, by = "red_amt")



model <- lmer(
  wtp ~ treatment * red_amt + (1 | country),
  data = data
)
summary(model)
emm <- emmeans(model, ~ treatment * red_amt)
summary(emm)
contrast(emm, method = "pairwise", by = "red_amt")
plot(emm, comparisons = TRUE)
pwpp(emm)
