library(dplyr)
library(ggplot2)
library(ggtext)
library(rlang)
library(viridis)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)
library(emmeans)

if (exists("snakemake")) {
  emm_wtc_file          <- snakemake@input[["emm_wtc"]]
  emm_wtp_file          <- snakemake@input[["emm_wtp"]]
  contr_wtc_file        <- snakemake@input[["contr_wtc"]]
  contr_wtp_file        <- snakemake@input[["contr_wtp"]]
  emm_wtc_income_file   <- snakemake@input[["emm_wtc_income"]]
  emm_wtp_income_file   <- snakemake@input[["emm_wtp_income"]]
  contr_wtc_income_file <- snakemake@input[["contr_wtc_income"]]
  contr_wtp_income_file <- snakemake@input[["contr_wtp_income"]]
  contr_flights_income_file <- snakemake@input[["contr_flights_income"]]
  emm_wtc_flier_file    <- snakemake@input[["emm_wtc_flier"]]
  emm_wtp_flier_file    <- snakemake@input[["emm_wtp_flier"]]
  contr_wtc_flier_file  <- snakemake@input[["contr_wtc_flier"]]
  contr_wtp_flier_file  <- snakemake@input[["contr_wtp_flier"]]
  contr_flights_flier_file <- snakemake@input[["contr_flights_flier"]]
  emm_wtc_clim_file     <- snakemake@input[["emm_wtc_clim"]]
  emm_wtp_clim_file     <- snakemake@input[["emm_wtp_clim"]]
  contr_wtc_clim_file   <- snakemake@input[["contr_wtc_clim"]]
  contr_wtp_clim_file   <- snakemake@input[["contr_wtp_clim"]]
  contr_flights_clim_file <- snakemake@input[["contr_flights_clim"]]
  out_income_emm_contr  <- snakemake@output[["income_emm_contr"]]
  out_flier_emm_contr   <- snakemake@output[["flier_emm_contr"]]
  out_clim_emm_contr    <- snakemake@output[["clim_emm_contr"]]
  out_contr_income      <- snakemake@output[["contr_income"]]
  out_contr_flier       <- snakemake@output[["contr_flier"]]
  out_contr_clim        <- snakemake@output[["contr_clim"]]
} else {
  emm_wtc_file          <- here("data", "emm_wtc.csv")
  emm_wtp_file          <- here("data", "emm_wtp.csv")
  contr_wtc_file        <- here("data", "contr_wtc.csv")
  contr_wtp_file        <- here("data", "contr_wtp.csv")
  emm_wtc_income_file   <- here("data", "emm_wtc_income.csv")
  emm_wtp_income_file   <- here("data", "emm_wtp_income.csv")
  contr_wtc_income_file <- here("data", "contr_wtc_income.csv")
  contr_wtp_income_file <- here("data", "contr_wtp_income.csv")
  contr_flights_income_file <- here("data", "contr_flights_income.csv")
  emm_wtc_flier_file    <- here("data", "emm_wtc_flier.csv")
  emm_wtp_flier_file    <- here("data", "emm_wtp_flier.csv")
  contr_wtc_flier_file  <- here("data", "contr_wtc_flier.csv")
  contr_wtp_flier_file  <- here("data", "contr_wtp_flier.csv")
  contr_flights_flier_file <- here("data", "contr_flights_flier.csv")
  emm_wtc_clim_file     <- here("data", "emm_wtc_clim.csv")
  emm_wtp_clim_file     <- here("data", "emm_wtp_clim.csv")
  contr_wtc_clim_file   <- here("data", "contr_wtc_clim.csv")
  contr_wtp_clim_file   <- here("data", "contr_wtp_clim.csv")
  contr_flights_clim_file <- here("data", "contr_flights_clim.csv")
  out_income_emm_contr  <- here("output", "plot_income_emm_contr.png")
  out_flier_emm_contr   <- here("output", "plot_flier_emm_contr.png")
  out_clim_emm_contr    <- here("output", "plot_clim_emm_contr.png")
  out_contr_income      <- here("output", "plot_contr_income.png")
  out_contr_flier       <- here("output", "plot_contr_flier.png")
  out_contr_clim        <- here("output", "plot_contr_clim.png")
}

standardize_emm_columns <- function(emm_df) {
  if ("lower.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.LCL = lower.CL)
  }
  if ("upper.CL" %in% colnames(emm_df)) {
    emm_df <- rename(emm_df, asymp.UCL = upper.CL)
  }
  return(emm_df)
}

theme_main <- function(main_text_size = 14) {
  theme_classic() +
    theme(
      text = element_text(size = main_text_size),
      plot.title = element_text(face = "bold")
    )
}

############## read emm files ###############

emm_wtc <- read_csv(emm_wtc_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp <- read_csv(emm_wtp_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

emm_wtp_income <- read_csv(emm_wtp_income_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

emm_wtc_income <- read_csv(emm_wtc_income_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp_flier <- read_csv(emm_wtp_flier_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flier", "average flier", "frequent flier"),
      labels = c("Non-flier", "Average flier", "Frequent flier")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

emm_wtc_flier <- read_csv(emm_wtc_flier_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flier", "average flier", "frequent flier"),
      labels = c("Non-flier", "Average flier", "Frequent flier")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp_clim <- read_csv(emm_wtp_clim_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    treatment = factor(treatment) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

emm_wtc_clim <- read_csv(emm_wtc_clim_file, show_col_types = FALSE) |>
  standardize_emm_columns() |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

########## read contr files ################

contr_wtc <- read_csv(contr_wtc_file, show_col_types = FALSE) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp <- read_csv(contr_wtp_file, show_col_types = FALSE) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

contr_wtp_income <- read_csv(contr_wtp_income_file, show_col_types = FALSE) |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

contr_wtc_income <- read_csv(contr_wtc_income_file, show_col_types = FALSE) |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp_flier <- read_csv(contr_wtp_flier_file, show_col_types = FALSE) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flier", "average flier", "frequent flier"),
      labels = c("Non-flier", "Average flier", "Frequent flier")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

contr_wtc_flier <- read_csv(contr_wtc_flier_file, show_col_types = FALSE) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flier", "average flier", "frequent flier"),
      labels = c("Non-flier", "Average flier", "Frequent flier")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp_clim <- read_csv(contr_wtp_clim_file, show_col_types = FALSE) |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based fee"    = "Egalitarianism",
        "Frequent-flying fee" = "Limitarianism",
        "Tourism fee"         = "Prioritarianism",
        "Flying fee"          = "Proportionalism"
      )
  )

contr_wtc_clim <- read_csv(contr_wtc_clim_file, show_col_types = FALSE) |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"), labels = c("Low", "Mid", "High")),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget"           = "Egalitarianism",
        "Frequent-flying cap"    = "Limitarianism",
        "Tourism cap"            = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_delta_income <- read_csv(contr_flights_income_file, show_col_types = FALSE) |>
  mutate(income_group = factor(income_group, levels = c("low", "mid", "high")))

contr_delta_flier <- read_csv(contr_flights_flier_file, show_col_types = FALSE) |>
  mutate(flying_group = factor(flying_group, levels = c("non-flier", "average flier", "frequent flier")))

contr_delta_clim <- read_csv(contr_flights_clim_file, show_col_types = FALSE) |>
  mutate(clim_concern = factor(clim_concern, levels = c("low", "mid", "high")))

############# colour maps ###################

make_subgroup_scale <- function(subgroups, legend_title) {
  subgroup_colors <- viridis::viridis(length(subgroups), option = "plasma", end = 0.8)
  color_vector <- c(subgroup_colors, "Overall" = "grey40")
  names(color_vector) <- c(subgroups, "Overall")
  scale_color_manual(
    values = color_vector,
    breaks = c(subgroups, "Overall"),
    name = legend_title,
    guide = guide_legend()
  )
}

subgroups_income <- emm_wtp_income |>
  mutate(label = paste0(income_group, " (n = ", n, ")")) |>
  distinct(label) |>
  pull(label)

scale_income <- make_subgroup_scale(subgroups = subgroups_income, legend_title = "Income group")

subgroups_flier <- emm_wtp_flier |>
  mutate(label = paste0(flying_group, " (n = ", n, ")")) |>
  distinct(label) |>
  pull(label)

scale_flier <- make_subgroup_scale(subgroups = subgroups_flier, legend_title = "Flying behaviour")

subgroups_clim <- emm_wtp_clim |>
  mutate(label = paste0(clim_concern, " (n = ", n, ")")) |>
  distinct(label) |>
  pull(label)

scale_clim <- make_subgroup_scale(subgroups = subgroups_clim, legend_title = "Climate concern")

############## marginal means ################

plot_emmeans_subgroup <- function(
  emm_subgroup,
  emm_overall = NULL,
  by,
  legend_title = NULL,
  main_text_size = 14,
  alpha = 1,
  color_scale = NULL
) {
  by <- as.character(by)
  ymin_col <- if ("asymp.LCL" %in% names(emm_subgroup)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(emm_subgroup)) "asymp.UCL" else "upper.CL"

  emm_subgroup <- emm_subgroup |>
    mutate(legend_label = paste0(.data[[by]], " (n = ", n, ")"))

  if (!is.null(emm_overall)) {
    emm_overall <- emm_overall |>
      mutate(legend_label = "Overall", n = NA)
    if (!(by %in% names(emm_overall))) emm_overall[[by]] <- "Overall"
    plot_data <- bind_rows(emm_subgroup, emm_overall)
  } else {
    plot_data <- emm_subgroup
  }

  legend_levels <- c(unique(emm_subgroup$legend_label), "Overall")
  plot_data$legend_label <- factor(plot_data$legend_label, levels = legend_levels)
  pd <- position_dodge(width = 0.3)

  p <- ggplot(plot_data, aes(x = treatment, y = emmean, color = legend_label, group = legend_label)) +
    geom_point(position = pd, size = 3, alpha = alpha) +
    geom_errorbar(
      aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
      width = 0.2, position = pd, alpha = alpha
    ) +
    geom_hline(yintercept = 2.5, linetype = 2, colour = "gray40", linewidth = 0.3) +
    coord_cartesian(ylim = c(0.5, 4.5)) +
    labs(y = "Marginal means", x = NULL) +
    theme_main(main_text_size)

  if (!is.null(color_scale)) p <- p + color_scale
  return(p)
}

################ plot contrasts ####################

plot_contrasts_subgroup <- function(
  contr_subgroup,
  contr_overall = NULL,
  by,
  legend_title = "Group",
  main_text_size = 14,
  alpha = 1,
  color_scale = NULL
) {
  by_sym <- ensym(by)

  if (!is.null(contr_overall)) {
    contr_overall <- contr_overall |> mutate(!!by_sym := "Overall")
    contr_plot <- bind_rows(contr_subgroup, contr_overall)
  } else {
    contr_plot <- contr_subgroup
  }

  if ("n" %in% colnames(contr_plot)) {
    contr_plot <- contr_plot |>
      mutate(
        legend_label = ifelse(
          .data[[by]] == "Overall",
          "Overall",
          paste0(.data[[by]], " (n = ", n, ")")
        )
      )
  }

  legend_levels <- c(setdiff(unique(contr_plot$legend_label), "Overall"), "Overall")
  contr_plot$legend_label <- factor(contr_plot$legend_label, levels = legend_levels)

  p <- ggplot(contr_plot, aes(x = contrast, y = estimate, color = legend_label, group = legend_label)) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * SE, ymax = estimate + 1.96 * SE),
      width = 0.2, position = position_dodge(0.3), alpha = alpha
    ) +
    geom_hline(yintercept = 0, linetype = 2, colour = "gray40", linewidth = 0.3) +
    labs(y = "Contrast with control", x = NULL) +
    ylim(-1.65, 1.5) +
    theme_main(main_text_size)

  if (!is.null(color_scale)) p <- p + color_scale
  return(p)
}

############ build plots #################

plot_wtp_income <- plot_emmeans_subgroup(emm_wtp_income, by = "income_group", color_scale = scale_income) +
  labs(title = "A. Effect of surcharge designs on willingness to pay")
plot_wtc_income <- plot_emmeans_subgroup(emm_wtc_income, by = "income_group", color_scale = scale_income) +
  labs(title = "B. Effect of budget designs on willingness to change")
plot_wtp_flier  <- plot_emmeans_subgroup(emm_wtp_flier,  by = "flying_group", color_scale = scale_flier) +
  labs(title = "A. Effect of surcharge designs on willingness to pay")
plot_wtc_flier  <- plot_emmeans_subgroup(emm_wtc_flier,  by = "flying_group", color_scale = scale_flier) +
  labs(title = "B. Effect of budget designs on willingness to change")
plot_wtp_clim   <- plot_emmeans_subgroup(emm_wtp_clim,   by = "clim_concern", color_scale = scale_clim) +
  labs(title = "A. Effect of surcharge designs on willingness to pay")
plot_wtc_clim   <- plot_emmeans_subgroup(emm_wtc_clim,   by = "clim_concern", color_scale = scale_clim) +
  labs(title = "B. Effect of budget designs on willingness to change")

plot_contr_wtp_income <- plot_contrasts_subgroup(contr_wtp_income, by = "income_group", color_scale = scale_income)
plot_contr_wtc_income <- plot_contrasts_subgroup(contr_wtc_income, by = "income_group", color_scale = scale_income)
plot_contr_wtp_flier  <- plot_contrasts_subgroup(contr_wtp_flier,  by = "flying_group", color_scale = scale_flier)
plot_contr_wtc_flier  <- plot_contrasts_subgroup(contr_wtc_flier,  by = "flying_group", color_scale = scale_flier)
plot_contr_wtp_clim   <- plot_contrasts_subgroup(contr_wtp_clim,   by = "clim_concern", color_scale = scale_clim)
plot_contr_wtc_clim   <- plot_contrasts_subgroup(contr_wtc_clim,   by = "clim_concern", color_scale = scale_clim)

############ compound plots #################

plot_income_emm_contr <- (plot_wtp_income | plot_contr_wtp_income) /
  (plot_wtc_income | plot_contr_wtc_income) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_flier_emm_contr <- (plot_wtp_flier | plot_contr_wtp_flier) /
  (plot_wtc_flier | plot_contr_wtc_flier) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_clim_emm_contr <- (plot_wtp_clim | plot_contr_wtp_clim) /
  (plot_wtc_clim | plot_contr_wtc_clim) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_income_contr_only <- (plot_contr_wtp_income + labs(title = "A. Effect of surcharge designs on willingness to pay")) /
  (plot_contr_wtc_income + labs(title = "B. Effect of budget designs on willingness to change")) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_flier_contr_only <- (plot_contr_wtp_flier + labs(title = "A. Effect of surcharge designs on willingness to pay")) /
  (plot_contr_wtc_flier + labs(title = "B. Effect of budget designs on willingness to change")) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_clim_contr_only <- (plot_contr_wtp_clim + labs(title = "A. Effect of surcharge designs on willingness to pay")) /
  (plot_contr_wtc_clim + labs(title = "B. Effect of budget designs on willingness to change")) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

################### save #################

ggsave(plot = plot_income_emm_contr, out_income_emm_contr, height = 10, width = 15)
ggsave(plot = plot_flier_emm_contr,  out_flier_emm_contr,  height = 10, width = 15)
ggsave(plot = plot_clim_emm_contr,   out_clim_emm_contr,   height = 10, width = 15)
ggsave(plot = plot_income_contr_only, out_contr_income,     height = 10, width = 15)
ggsave(plot = plot_flier_contr_only,  out_contr_flier,      height = 10, width = 15)
ggsave(plot = plot_clim_contr_only,   out_contr_clim,       height = 10, width = 15)