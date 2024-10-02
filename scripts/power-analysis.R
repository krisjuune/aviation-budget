library("RCT")

data <- data.frame(y_1 = rbinom(n = 100, size = 1, prob = 0.3),
                   y_2 = rnorm(n = 100, mean = 8, sd = 2))
N_min(data$y_1,
      tau_min = 0.01,
      power = 0.8,
      significance = 0.05,
      share_control = seq(0, 1, 0.1),
      n_groups = 3)