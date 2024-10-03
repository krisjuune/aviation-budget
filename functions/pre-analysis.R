library(lme4)
library(simr)



simulate_power <- function(sample_size,
                           effect_size_treatment1,
                           effect_size_treatment2,
                           effect_size_treatment3,
                           effect_size_treatment4,
                           nsim = 100) {

  # get stat power for the given sample size (nr of participants),
  # effect sizes, and nr of simulations
  n_participants <- sample_size
  data <- expand.grid(
    participant = factor(1:n_participants),
    framing = factor(1:4),        # Four treatment levels
    emissions = factor(1:3),      # Three emission levels
    condition = c("control", "treatment")
  )

  # assigning different effect sizes for each treatment level
  data$effect_size <- ifelse(data$framing == 1, effect_size_treatment1,
                             ifelse(data$framing == 2, effect_size_treatment2,
                                    ifelse(data$framing == 3,
                                           effect_size_treatment3,
                                           effect_size_treatment4)))

  # simulate the outcome variable with effect sizes dep on the treatment
  data$willingness_change <- 1 + data$effect_size * as.numeric(data$framing) +
    0.3 * as.numeric(data$emissions) +
    rnorm(nrow(data), 0, 1)

  data$participant_effect <- rnorm(n_participants, 0, 1)
  data$willingness_change <- data$willingness_change +
    data$participant_effect[data$participant]

  # fit the model
  model <- lmer(willingness_change ~
                  framing * emissions +
                    (1 | participant),
                data = data)

  # extend the model for power simulation
  model_sim <- tryCatch(
    extend(model, along = "participant", n = n_participants),
    error = function(e) {
      cat("Error extending model:", e$message, "\n")
      return(NULL)  # Return NULL on error
    }
  )

  if (is.null(model_sim)) return(NULL)
  return(model_sim)

#   # run power simulation
#   power_result <- tryCatch(
#     powerSim(model_sim, test = fixed("framing:emissions", "lr"), nsim = nsim),
#     error = function(e) {
#       cat("Error in powerSim:", e$message, "\n")
#       return(NULL)
#     }
#   )

#   if (!is.null(power_result)) {
#     return(power_result$power)
#   } else {
#     return(NULL)
#   }
}
