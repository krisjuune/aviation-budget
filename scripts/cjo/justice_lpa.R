library(mclust)
library(tidyLPA)
# library(nnet)
library(tidyverse)
# library(dplyr)
library(ggplot2)

lpa_data <- read.csv("data/lpa_input.csv") |>
  select(-X)
set.seed(43)

# set nr of models to test, 1:m models will be checked
m <- 8

################### get LPA models #####################

# choose columns for the lpa
lpa_columns <- lpa_data |>
  select(-id, -country)

# also test with CH data only
lpa_data_ch <- lpa_data |>
  filter(country == "CH")

lpa_columns_ch <- lpa_data_ch |>
  select(-id, -country)

perform_lpa_analysis <- function(lpa_columns, m, output_dir = "output", country = "") {
  # check if data is numeric as it should
  lpa_columns <- lpa_columns |>
    mutate_if(is.factor, as.numeric) |>
    mutate_if(is.character, as.numeric)
  
  # adjust filenames based on country
  csv_filename <- if (country != "") paste0("lpa_fit_stats_", country, ".csv") else "lpa_fit_stats.csv"
  png_filename <- if (country != "") paste0("lpa_fit_stats_elbow_", country, ".png") else "lpa_fit_stats_elbow.png"

  # perform LPA using mclust
  lpa_results <- estimate_profiles(
    lpa_columns,
    1:m,
    variances = "equal",
    covariances = "zero",
    package = "mclust"
  )

  # extract fit statistics
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

  # calculate proportion of smallest class
  min_proportions <- numeric(length(lpa_results))
  for (i in seq_along(lpa_results)) {
    class_assignments <- lpa_results[[i]]$dff$Class
    class_proportions <- table(class_assignments) / length(class_assignments) * 100
    min_proportions[i] <- round(min(class_proportions), 1)
  }
  fit_stats$min_proportion <- min_proportions

  # write fit statistics to CSV
  write.csv(fit_stats, file.path(output_dir, csv_filename), row.names = TRUE)

  # prepare data for plotting
  fit_stats_plot <- fit_stats |>
    select(G, AIC, AWE, BIC, SABIC, ICL) |>
    mutate(ICL = ICL * -1)

  fit_stats_long <- pivot_longer(fit_stats_plot,
                                 cols = -G,
                                 names_to = "Statistic",
                                 values_to = "Value")

  # generate elbow plot with fit stats
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

  # save the plot
  ggsave(file.path(output_dir, png_filename),
         plot = plot,
         width = 8,
         height = 6,
         dpi = 300,
         bg = 'white')

  return(lpa_results)
}

# running the function takes a couple of mins
lpa_results <- perform_lpa_analysis(lpa_columns, m)
lpa_results_ch <- perform_lpa_analysis(lpa_columns_ch, m, country = "ch")

# perform lpa
# lpa_results <- estimate_profiles(
#   lpa_columns,
#   1:m,
#   # choosing the mclust model EEI for class-invariant parameterization
#   variances = "equal",
#   covariances = "zero",
#   package = "mclust"
# )

#TODO try to work without tidylpa and use purrr to format the results
# purrr::map_dfr(
#   lpa_results,
#   ~ tibble::as_tibble_row(.x$fit),
#   .id = "model"
# ) |>
#   janitor::clean_names() |>
#   dplyr::select(classes, aic, awe, bic, sabic, icl, entropy, prob_min, prob_max)

# summary(lpa_results$model_1_class_1$estimates)

# fit_stats <- data.frame(
#   G = 1:m,
#   AIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[4]]),
#   AWE = sapply(1:m, function(x) lpa_results[[x]]$fit[[5]]),
#   BIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[6]]),
#   SABIC = sapply(1:m, function(x) lpa_results[[x]]$fit[[10]]),
#   ICL = sapply(1:m, function(x) lpa_results[[x]]$fit[[11]]),
#   entropy = sapply(1:m, function(x) lpa_results[[x]]$fit[[12]]),
#   prob_min = sapply(1:m, function(x) lpa_results[[x]]$fit[[13]]),
#   prob_max = sapply(1:m, function(x) lpa_results[[x]]$fit[[14]])
# )

###################### select model ###########################

class_assignments_3 <- lpa_results[[3]]$dff$Class
class_assignments_5 <- lpa_results[[5]]$dff$Class

lpa_data <- lpa_data |>
  mutate(justice_class_3 = class_assignments_3) |>
  mutate(justice_class_5 = class_assignments_5)

write.csv(lpa_data, "data/lpa_output.csv", row.names = TRUE)


################### check models 3 to 5 ######################

lpa_tidy <- lpa_columns |>
  single_imputation() |>    # handle missing data if necessary
  estimate_profiles(n_profiles = 3:5,
                    package = "mclust")

# summarize the tidyLPA model
lpa_tidy

# visualize the profiles from tidyLPA
plot_profiles(lpa_tidy)

################## interpret models 3 and 5 #################
#TODO counts numbering doesn't match what is one the plot
# because the model was run separately

counts_3 <- table(lpa_data$justice_class_3)
counts_3

counts_5 <- table(lpa_data$justice_class_5)
counts_5


