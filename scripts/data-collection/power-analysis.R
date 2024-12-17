library(Matrix)
library(lme4)
library(simr)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(tidyr)
source("functions/pre-analysis.R")

# fit the linear mixed effects model







# create data frame for simulation
data <- expand.grid(
  participant = factor(1:n_participants),
  treatment_justice = factor(1:4),
  treatment_emissions = factor(1:3),
  condition = c("control", "treatment"),
  rep = 1:n_reps
)

# assuming some effect sizes (change later or loop over a few)
# fixed effects (mean changes for treatments)
data$wtc <- 1 + 0.5 * as.numeric(data$treatment_justice) +
  0.3 * as.numeric(data$treatment_emissions) +
  0.2 * as.numeric(data$treatment_justice) *
    as.numeric(data$treatment_emissions) +
  # and some random noise
  rnorm(nrow(data), 0, 1)

# random intercept for participant (each participant has their own baseline)
data$participant_effect <- rnorm(n_participants, 0, 1)
data$wtc <- data$wtc + data$participant_effect[data$participant]

head(data)
tail(data)

################### fit model and get power ###################

model <- lmer(wtc ~ treatment_justice * treatment_emissions + (1 | participant),
              data = data)

summary(model)

# convert model to a power analysis-ready object
model_sim <- extend(model, along = "participant", n = 200)

# run a power analysis to detect interaction effect at 80% power
power_sim <- powerSim(model_sim,
                      test = fixed("treatment_justice:treatment_emissions",
                                   "lr"),
                      nsim = 100)

power_sim


#################### plot power analysis #####################

# define ranges for sample sizes and effect sizes (noise levels)
sample_size <- 100
effect_size <- 0.2

results <- simulate_power(sample_size,
                          effect_size_treatment1 = effect_size,
                          effect_size_treatment2 = effect_size,
                          effect_size_treatment3 = effect_size,
                          effect_size_treatment4 = effect_size,
                          nsim = 100)


# view results
str(results)
