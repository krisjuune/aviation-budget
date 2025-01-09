library(mclust)
library(tidyLPA)
# library(nnet)
library(tidyverse)
# library(dplyr)
library(ggplot2)
library(patchwork)
library(grid) # needed??
# library(Matrix)

lpa_data <- read.csv("data/lpa_input.csv")
set.seed(42)

# set nr of models to test, 1:m models will be checked
m <- 10

################### get LPA models #####################

# run model and store results
lpa_columns <- lpa_data %>%
  # filter(country == "US") %>%
  select(-X, -id, -country)
lpa_columns <- lpa_columns %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_if(is.character, as.numeric)

# perform lpa
lpa_results <- estimate_profiles(
  lpa_columns,
  1:m,
  # choosing the mclust model EEI for class-invariant parameterization
  variances = "equal",
  covariances = "zero",
  package = "mclust"
)

#TODO try to work without tidylpa and use purrr to format the results
# purrr::map_dfr(
#   lpa_results,
#   ~ tibble::as_tibble_row(.x$fit),
#   .id = "model"
# ) |>
#   janitor::clean_names() |>
#   dplyr::select(classes, aic, awe, bic, sabic, icl, entropy, prob_min, prob_max)

# summary(lpa_results$model_1_class_1$estimates)

fit_stats <- data.frame(
  G = 1:m,
  AIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[4]]),
  AWE = sapply(1:m, function(x) lpa_results[[x]]$fit[[5]]),
  BIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[6]]),
  SABIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[10]]),
  ICL = sapply(1:m, function(x) lpa_results[[x]]$fit[[11]]),
  entropy = sapply(1:m, function(x) lpa_results[[x]]$fit[[12]]),
  prob_min = sapply(1:m, function(x) lpa_results[[x]]$fit[[13]]),
  prob_max = sapply(1:m, function(x) lpa_results[[x]]$fit[[14]])
)

# Calculate proportions for each G
min_proportions <- numeric(length(lpa_results))

# Get smallest proportions for each model (each G)
for (i in seq_along(lpa_results)) {
  class_assignments <- lpa_results[[i]]$dff$Class
  class_proportions <- table(class_assignments) / length(class_assignments) * 100
  min_proportions[i] <- round(min(class_proportions), 1)
}

fit_stats$min_proportion <- min_proportions

# write fit stats to file
write.csv(fit_stats, "output/lpa_fit_stats.csv", row.names = TRUE)

# reshape the data for plotting
fit_stats_plot <- fit_stats %>%
  select(G, AIC, AWE, BIC, SABIC, ICL) %>%
  mutate(ICL = ICL * -1)

fit_stats_long <- pivot_longer(fit_stats_plot,
  cols = -G,
  names_to = "Statistic",
  values_to = "Value"
)

# create elbow plot of fit statistics
plot <- ggplot(fit_stats_long, aes(x = G, y = Value, color = Statistic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Elbow Plot of Fit Statistics for LPA",
    x = "Number of Profiles (G)",
    y = "Fit Statistics"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Save the plot to a PNG file
ggsave("output/fit-stats-lpa.png", plot = plot, width = 8, height = 6, dpi = 300)


###################### select model ###########################

class_assignments_3 <- lpa_results[[3]]$dff$Class
class_assignments_5 <- lpa_results[[5]]$dff$Class

lpa_data <- lpa_data %>%
  mutate(justice_class_3 = class_assignments_3) %>%
  mutate(justice_class_5 = class_assignments_5)

write.csv(lpa_data, "data/lpa_output.csv", row.names = TRUE)


################### check models 3 to 5 ######################

lpa_tidy <- lpa_columns %>%
  single_imputation() %>%    # handle missing data if necessary
  estimate_profiles(n_profiles = 3:5,
                    package = "mclust")

# summarize the tidyLPA model
lpa_tidy

# visualize the profiles from tidyLPA
plot_profiles(lpa_tidy)

counts <- table(lpa_data$justice_class)
counts
