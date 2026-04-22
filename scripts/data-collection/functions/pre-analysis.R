library(Matrix)
library(lme4)
library(simr)
library(dplyr)
library(tidyr)

simulate_data <- function(sample_size,
                          effect_wtc_base = 1,
                          effect_wtc_egal = 0,
                          effect_wtc_prior = 0,
                          effect_wtc_limit = 0.5,
                          effect_wtc_prop = -0.5,
                          effect_size_flights = 1) {
  set.seed(42)

# Randomly assign participants to control (20%) or treatment (80%)
  control_assignment <- sample(c(TRUE, FALSE), size = sample_size, replace = TRUE, prob = c(0.2, 0.8))

  # simulate participant data
  participant_id <- 1:sample_size
  condition <- ifelse(control_assignment, "control", "treatment")
  age <- factor(sample(18:60, sample_size, replace = TRUE))
  gender <- factor(sample(c("Male", "Female"), sample_size, replace = TRUE))

  # Assign emissions and framing only for treatment group (control has no framing or emissions)
  emissions <- ifelse(condition == "treatment", sample(c("15%", "30%", "45%"), size = sum(!control_assignment), replace = TRUE), "control")
  framing <- ifelse(condition == "treatment", sample(c("egal", "prior", "limit", "prop"), size = sum(!control_assignment), replace = TRUE), "control")

  # Base wtc effect for control
  wtc <- ifelse(condition == "control",
                rnorm(sum(control_assignment), mean = 0, sd = 1.5),
                NA)  # initialize for treatment group

  # Define effects for framing categories
  for (i in which(condition == "treatment")) {
    emission_level <- emissions[i]
    framing_category <- framing[i]
    
    # Set base effect size depending on framing
    effect_size_wtc <- switch(framing_category,
                              "egal"  = effect_wtc_base + effect_wtc_egal,
                              "prior" = effect_wtc_base + effect_wtc_prior,
                              "limit" = effect_wtc_base + effect_wtc_limit,
                              "prop"  = effect_wtc_base + effect_wtc_prop)
    
    # Adjust "prop" and "limit" further based on emissions
    if (framing_category == "prop") {
      if (emission_level == "45%") {
        effect_size_wtc <- 0  # No effect for prop at 45% emissions reduction
      } else if (emission_level == "30%") {
        effect_size_wtc <- effect_size_wtc * 0.5  # Reduced effect
      }
    } else if (framing_category == "limit") {
      if (emission_level == "45%") {
        effect_size_wtc <- effect_size_wtc + 0.5  # Boost for limit at 45%
      }
    }
    
    # Simulate wtc for treatment participants
    wtc[i] <- rnorm(1, mean = effect_size_wtc, sd = 1.5)
  }

  wtc <- round(wtc)
  wtc <- ifelse(wtc < -3, -3, ifelse(wtc > 3, 3, wtc))

  # Simulate planned flights for all participants
  planned_flights <- round(rnorm(sample_size, mean = 10, sd = 5))
  planned_flights <- pmin(pmax(planned_flights, 0), 300)


  # Simulate flights: control has planned flights, treatment has reduced flights
  flights <- planned_flights  # default for control group
  flights[!control_assignment] <- planned_flights[!control_assignment] - rnorm(sum(!control_assignment), mean = effect_size_flights, sd = 5)
  flights <- round(flights)
  reduced_flights <- planned_flights - flights

  # Create dataframe
  data <- data.frame(
    participant_id = participant_id,
    age = age,
    gender = gender,
    condition = condition,
    emissions = emissions,
    framing = framing,
    wtc = wtc,
    reduced_flights = reduced_flights
  )

  # Pivot the data into long format
  data_long <- data %>%
    # pivot_longer(
    #   cols = c(wtc, reduced_flights),  # Pivot only the indicator variables (not emissions and framing)
    #   names_to = "indicator_vars",     # These are the variables that will be pivoted
    #   values_to = "value"              # The corresponding values of those variables
    # ) %>%
    pivot_longer(
      cols = c(age, gender),  # Keeping condition, emissions, framing separately
      names_to = "dem_vars",              # The name for the condition-related columns
      values_to = "dem_value"             # The value for those columns
    )

  return(data_long)
}

data <- simulate_data(300)
head(data)
tail(data)




# Step 1: Fit the mixed-effects model to the simulated data
fit_model <- function(simulated_data) {
  
  # Fit linear mixed model with random participant effects
  # We include (1 | participant_id) to model the repeated measures for each participant
  model <- lmer(wtc ~ emissions * framing + (1 | participant_id),
                data = simulated_data)
  
  return(model)
}

# Step 2: Extend the model for power analysis by increasing sample size
extend_model <- function(model, new_sample_size) {
  
  # Extend the model to the new sample size
  extended_model <- extend(model, along = "participant_id", n = new_sample_size)
  
  return(extended_model)
}

# Step 3: Run power analysis simulations
run_power_analysis <- function(extended_model, nsim = 100) {
  
  # Perform power analysis for the interaction between framing and emissions
  power_analysis <- powerSim(extended_model, test = fixed("framing:emissions", "t"), nsim = nsim)
  
  return(power_analysis)
}

# Step 4: Calculate the required sample size for a given power threshold
calculate_required_sample_size <- function(model, target_power = 0.8, nsim = 100) {
  
  # Find the sample size needed to achieve the target power
  power_curve <- powerCurve(model, test = fixed("framing:emissions", "t"), along = "participant_id", 
                            nsim = nsim, breaks = seq(100, 1000, by = 50))
  
  plot(power_curve)  # Plot the power curve
  return(power_curve)
}



# Example usage of the functions
simulated_data <- simulate_data(1000)

# Step 1: Fit the mixed-effects model
model <- fit_model(simulated_data)
summary(model)

# Step 2: Extend the model to a larger sample size (e.g., 500 participants)
extended_model <- extend_model(model, new_sample_size = 500)

# Step 3: Run power analysis on the extended model
power_analysis <- run_power_analysis(extended_model, nsim = 100)
print(power_analysis)

# Step 4: Calculate the required sample size to reach 80% power
required_sample_size <- calculate_required_sample_size(extended_model, target_power = 0.8, nsim = 100)











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
