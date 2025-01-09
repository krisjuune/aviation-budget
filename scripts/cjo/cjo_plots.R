library(tidyverse)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

lpa_data <- read.csv("data/lpa_output.csv")

output_dir <- "output"
hist_filename <- "cjo_test_histogram.png"
lpa_filename_3 <- "lpa_subplot_G3.png"
lpa_filename_5 <- "lpa_subplot_G5.png"

###################### basic CJO plots #####################

# Reshape the data to long format for easier plotting
lpa_data_long <- lpa_data |>
  pivot_longer(
    cols = utilitarian:limitarian, 
    names_to = "variable", 
    values_to = "value"
  )

# Create the histogram plot with facets for each variable and fill by country
histogram_plot <- ggplot(lpa_data_long, aes(x = value, fill = country)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Distribution of Justice Principles per Country",
    x = "Score",
    y = "Count",
    fill = "Country"
  ) +
  theme_minimal()

# Display the plot
ggsave(file.path(output_dir, hist_filename), plot = histogram_plot, width = 8, height = 6, dpi = 300, bg = "white")

################### interpret model 3 and 5 ################

create_lpa_plots <- function(data, justice_class_column, principle_means = TRUE, output_dir, lpa_filename) {
  # set justice class column
  data$justice_class <- data[[justice_class_column]]
  
  # bar plot of latent profiles
  plot1 <- ggplot(data, aes(x = factor(justice_class), fill = factor(justice_class))) +
    geom_bar() +
    labs(title = "Distribution of Classes by tidyLPA",
         x = "Latent Profile",
         y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")

  # compute mean values depending on principle_means flag
  if (principle_means) {
    mean_values <- data |>
      group_by(justice_class) |>
      summarize(
        utilitarian = mean(utilitarian, na.rm = TRUE),
        egalitarian = mean(egalitarian, na.rm = TRUE),
        sufficientarian = mean(sufficientarian, na.rm = TRUE),
        limitarian = mean(limitarian, na.rm = TRUE)
      )
    cols_to_pivot <- c("utilitarian", "egalitarian", "sufficientarian", "limitarian")
  } else {
    mean_values <- data |>
      group_by(justice_class) |>
      summarize(
        utilitarian_gen = mean(justice_general_1, na.rm = TRUE),
        utilitarian_tax = mean(justice_tax_1, na.rm = TRUE),
        utilitarian_sub = mean(justice_subsidy_1, na.rm = TRUE),
        egalitarian_gen = mean(justice_general_2, na.rm = TRUE),
        egalitarian_tax = mean(justice_tax_2, na.rm = TRUE),
        egalitarian_sub = mean(justice_subsidy_2, na.rm = TRUE),
        sufficientarian_gen = mean(justice_general_3, na.rm = TRUE),
        sufficientarian_tax = mean(justice_tax_3, na.rm = TRUE),
        sufficientarian_sub = mean(justice_subsidy_3, na.rm = TRUE),
        limitarian_gen = mean(justice_general_4, na.rm = TRUE),
        limitarian_tax = mean(justice_tax_4, na.rm = TRUE),
        limitarian_sub = mean(justice_subsidy_4, na.rm = TRUE)
      )
    cols_to_pivot <- c("utilitarian_gen", "utilitarian_tax", "utilitarian_sub", 
                       "egalitarian_gen", "egalitarian_tax", "egalitarian_sub",
                       "sufficientarian_gen", "sufficientarian_tax", "sufficientarian_sub",
                       "limitarian_gen", "limitarian_tax", "limitarian_sub")
  }

  # convert to long format and add error bars
  mean_values_long <- mean_values |>
    pivot_longer(cols = all_of(cols_to_pivot), 
                 names_to = "variable", 
                 values_to = "value") |>
    mutate(
      ymin = value - 0.95,
      ymax = value + 0.95
    )

  #line plot with error bars
  plot2 <- ggplot(mean_values_long, aes(x = variable, y = value, color = factor(justice_class), group = factor(justice_class))) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 0.8) +
    labs(title = "Profiles Across Latent Classes",
         x = "Variable",
         y = "Mean Score",
         color = "Latent Profile") +
    theme_minimal()

  # combine plots side by side
  combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)

  # save the plot
  ggsave(file.path(output_dir, lpa_filename),
         plot = combined_plot,
         width = 12,
         height = 6,
         dpi = 300)
}

create_lpa_plots(lpa_data,
                 "justice_class_3",
                 principle_means = TRUE,
                 output_dir,
                 lpa_filename_3)
create_lpa_plots(lpa_data,
                 "justice_class_5",
                 principle_means = TRUE,
                 output_dir,
                 lpa_filename_5)