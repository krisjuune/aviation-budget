library(tidyverse)
library(ggplot2)
library(patchwork)
library(grid) 

lpa_data <- read.csv("data/lpa_output.csv")

output_dir = "output"
hist_filename = "cjo_test_histogram.png"
lpa_filename = "lpa_test_G5.png"

###################### basic CJO plots #####################

# Reshape the data to long format for easier plotting
lpa_data_long <- lpa_data %>%
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

lpa_data$justice_class <- lpa_data$justice_class_5

plot1 <- ggplot(lpa_data,
                aes(x = factor(justice_class),
                    fill = factor(justice_class))) +
  geom_bar() +
  labs(title = "Distribution of Classes by tidyLPA",
       x = "Latent Profile",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

mean_values <- lpa_data %>%
  group_by(justice_class) %>%
  summarize(
    utilitarian = mean(utilitarian, na.rm = TRUE),
    egalitarian = mean(egalitarian, na.rm = TRUE),
    sufficientarian = mean(sufficientarian, na.rm = TRUE),
    limitarian = mean(limitarian, na.rm = TRUE)
  )

mean_values_long <- mean_values %>%
  pivot_longer(cols = utilitarian:limitarian, 
               names_to = "variable", 
               values_to = "value")

# mean_values <- lpa_data %>%
#     group_by(justice_class) %>%
#     summarize(
#       utilitarian_gen = mean(justice_general_1, na.rm = TRUE),
#       utilitarian_tax = mean(justice_tax_1, na.rm = TRUE),
#       utilitarian_sub = mean(justice_subsidy_1, na.rm = TRUE),
#       egalitarian_gen = mean(justice_general_2, na.rm = TRUE),
#       egalitarian_tax = mean(justice_tax_2, na.rm = TRUE),
#       egalitarian_sub = mean(justice_subsidy_2, na.rm = TRUE),
#       sufficientarian_gen = mean(justice_general_3, na.rm = TRUE),
#       sufficientarian_tax = mean(justice_tax_3, na.rm = TRUE),
#       sufficientarian_sub = mean(justice_subsidy_3, na.rm = TRUE),
#       limitarian_gen = mean(justice_general_4, na.rm = TRUE),
#       limitarian_tax = mean(justice_tax_4, na.rm = TRUE),
#       limitarian_sub = mean(justice_subsidy_4, na.rm = TRUE),
#     )

# mean_values_long <- mean_values %>%
#   pivot_longer(cols = utilitarian_gen:limitarian_sub, 
#                names_to = "variable", 
#                values_to = "value")

mean_values_long <- mean_values_long %>%
  mutate(
    ymin = value - 0.95,  
    ymax = value + 0.95
  )

# Plotting with variables on x-axis and latent profiles as colors in the legend
plot2 <- ggplot(mean_values_long, aes(x = variable, y = value, color = factor(justice_class), group = factor(justice_class))) +
  geom_line(linewidth = 1.5) +      # Draws lines connecting points within each latent profile
  geom_point(size = 3) +       # Adds points at each mean value
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 0.8) +  # Adds error bars
  labs(title = "Profiles Across Latent Classes",
       x = "Variable",
       y = "Mean Score",
       color = "Latent Profile") +
  theme_minimal()

combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)

ggsave(file.path(output_dir, lpa_filename), plot = combined_plot, width = 12, height = 6, dpi = 300)
