library(lme4)
library(simr)

df <- read.csv("data/synthetic_1000.csv")

# define mixed effects model
model <- lmer(WTC ~ Treatment + (1|Participant), data = df)

# extend the model for the desired sample size
model_extended <- extend(model, along = "Participant", n = 250)

# conduct power simulation
power_result <- powerSim(model_extended, nsim = 1000)

# output power results
print(power_result)