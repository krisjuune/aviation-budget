library(dplyr)
library(ggplot2)
library(rlang)
library(viridis)
library(purrr)
library(forcats)
library(tidyr)
library(tidyverse)
library(here)
library(colorspace)
library(patchwork)


standardize_emm_columns <- function(emm_df) {
  emm_df <- emm_df |>
    rename(
      asymp.LCL = dplyr::case_when(
        "asymp.LCL" %in% colnames(emm_df) ~ "asymp.LCL",
        "lower.CL" %in% colnames(emm_df) ~ "lower.CL",
        TRUE ~ NA_character_
      ),
      asymp.UCL = dplyr::case_when(
        "asymp.UCL" %in% colnames(emm_df) ~ "asymp.UCL",
        "upper.CL" %in% colnames(emm_df) ~ "upper.CL",
        TRUE ~ NA_character_
      )
    )
  if ("lower.CL" %in% colnames(emm_df)) {
    emm_df <- dplyr::rename(emm_df, asymp.LCL = lower.CL)
  }
  if ("upper.CL" %in% colnames(emm_df)) {
    emm_df <- dplyr::rename(emm_df, asymp.UCL = upper.CL)
  }
  return(emm_df)
}



############## read emmm files ###############


emm_wtp_income <- read_csv(
  here("data", "emm_wtp_income.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Base price" = "Control",
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

emm_wtc_income <- read_csv(
  here("data", "emm_wtc_income.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp_flyer <- read_csv(
  here("data", "emm_wtp_flyer.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Base price" = "Control",
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

emm_wtc_flyer <- read_csv(
  here("data", "emm_wtc_flyer.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp_clim <- read_csv(
  here("data", "emm_wtp_clim.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Base price" = "Control",
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

emm_wtc_clim <- read_csv(
  here("data", "emm_wtc_clim.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtc <- read_csv(
  here("data", "emm_wtc.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

emm_wtp <- read_csv(
  here("data", "emm_wtp.csv"), show_col_types = FALSE
) |>
  standardize_emm_columns() |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Base price" = "Control",
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )


########## read contr files ################

contr_wtp_income <- read_csv(
  here("data", "contr_wtp_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

contr_wtc_income <- read_csv(
  here("data", "contr_wtc_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(
      income_group,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp_flyer <- read_csv(
  here("data", "contr_wtp_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

contr_wtc_flyer <- read_csv(
  here("data", "contr_wtc_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(
      flying_group,
      levels = c("non-flyer", "average flyer", "frequent flyer"),
      labels = c("Non-flyer", "Average flyer", "Frequent flyer")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp_clim <- read_csv(
  here("data", "contr_wtp_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

contr_wtc_clim <- read_csv(
  here("data", "contr_wtc_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(
      clim_concern,
      levels = c("low", "mid", "high"),
      labels = c("Low", "Mid", "High")
    ),
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )


contr_delta_income <- read_csv(
  here("data", "contr_flights_income.csv"), show_col_types = FALSE
) |>
  mutate(
    income_group = factor(income_group, levels = c("low", "mid", "high"))
  )

contr_delta_flyer <- read_csv(
  here("data", "contr_flights_flyer.csv"), show_col_types = FALSE
) |>
  mutate(
    flying_group = factor(flying_group, levels = c("non-flyer", "average flyer", "frequent flyer"))
  )

contr_delta_clim <- read_csv(
  here("data", "contr_flights_clim.csv"), show_col_types = FALSE
) |>
  mutate(
    clim_concern = factor(clim_concern, levels = c("low", "mid", "high"))
  )

contr_wtc <- read_csv(
  here("data", "contr_wtc.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Equal budget" = "Egalitarianism",
        "Frequent flying cap" = "Limitarianism",
        "Tourism cap" = "Prioritarianism",
        "Proportional reduction" = "Proportionalism"
      )
  )

contr_wtp <- read_csv(
  here("data", "contr_wtp.csv"), show_col_types = FALSE
) |>
  mutate(
    contrast = stringr::str_remove(contrast, " - Control$"),
    contrast = factor(contrast) |>
      fct_recode(
        "Income-based tax" = "Egalitarianism",
        "Frequent flying tax" = "Limitarianism",
        "Tourism tax" = "Prioritarianism",
        "Flying tax" = "Proportionalism"
      )
  )

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

scale_income <- make_subgroup_scale(
  subgroups = subgroups_income,
  legend_title = "Income group"
)

subgroups_flyer <- emm_wtp_flyer |>
  mutate(label = paste0(flying_group, " (n = ", n, ")")) |>
  distinct(label) |>
  pull(label)

scale_flyer <- make_subgroup_scale(
  subgroups = subgroups_flyer,
  legend_title = "Flying behaviour"
)

subgroups_clim <- emm_wtp_clim |>
  mutate(label = paste0(clim_concern, " (n = ", n, ")")) |>
  distinct(label) |>
  pull(label)

scale_clim <- make_subgroup_scale(
  subgroups = subgroups_clim,
  legend_title = "Climate concern"
)




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

  # Determine which columns are used for CIs
  ymin_col <- if ("asymp.LCL" %in% names(emm_subgroup)) "asymp.LCL" else "lower.CL"
  ymax_col <- if ("asymp.UCL" %in% names(emm_subgroup)) "asymp.UCL" else "upper.CL"

  # Add legend labels with n for subgroups
  emm_subgroup <- emm_subgroup |>
    mutate(
      legend_label = paste0(.data[[by]], " (n = ", n, ")")
    )

  # Merge overall sample into the subgroup data
  if (!is.null(emm_overall)) {
    emm_overall <- emm_overall |>
      mutate(
        legend_label = "Overall",
        n = NA  # no n for overall
      )
    # Ensure overall has the same 'by' column
    if (!(by %in% names(emm_overall))) {
      emm_overall[[by]] <- "Overall"
    }
    plot_data <- bind_rows(emm_subgroup, emm_overall)
  } else {
    plot_data <- emm_subgroup
  }

  # Order legend: subgroups first, overall last
  legend_levels <- c(unique(emm_subgroup$legend_label), "Overall")
  plot_data$legend_label <- factor(plot_data$legend_label, levels = legend_levels)

  # Shared dodge
  pd <- position_dodge(width = 0.3)

  p <- ggplot(plot_data, aes(
    x = treatment,
    y = emmean,
    color = legend_label,
    group = legend_label
  )) +
    geom_point(position = pd, size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = .data[[ymin_col]],
        ymax = .data[[ymax_col]]
      ),
      width = 0.2,
      position = pd,
      alpha = alpha
    ) +
    labs(
      y = "Marginal means",
      x = NULL
    ) +
    geom_hline(
      yintercept = 2.5,
      linetype = 2,
      colour = "gray40",
      linewidth = 0.3
    ) +
    coord_cartesian(ylim = c(0.5, 4.5)) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))

  if (!is.null(color_scale)) {
    p <- p + color_scale
  }
  return(p)
}

plot_wtp_income <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtp_income,
  emm_overall = emm_wtp,
  by = "income_group",
  legend_title = "Income group",
  color_scale = scale_income
) + labs(title = "A. Willingness to pay for SAFs for SAFs")
plot_wtc_income <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtc_income,
  emm_overall = emm_wtc,
  by = "income_group",
  legend_title = "Income group",
  color_scale = scale_income
) + labs(title = "B. Willingness to constrain own flying")




plot_wtp_flyer <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtp_flyer,
  emm_overall = emm_wtp,
  by = "flying_group",
  legend_title = "Flying behaviour",
  color_scale = scale_flyer
) + labs(title = "A. Willingness to pay for SAFs")
plot_wtc_flyer <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtc_flyer,
  emm_overall = emm_wtc,
  by = "flying_group",
  legend_title = "Flying behaviour",
  color_scale = scale_flyer
) + labs(title = "B. Willingness to constrain own flying")


plot_wtp_clim <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtp_clim,
  emm_overall = emm_wtp,
  by = "clim_concern",
  legend_title = "Climate concern",
  color_scale = scale_clim
) + labs(title = "A. Willingness to pay for SAFs")
plot_wtc_clim <- plot_emmeans_subgroup(
  emm_subgroup = emm_wtc_clim,
  emm_overall = emm_wtc,
  by = "clim_concern",
  legend_title = "Climate concern",
  color_scale = scale_clim
) + labs(title = "B. Willingness to constrain own flying")


################### delta flights ##################


plot_flights_subgroup <- function(
  emm,
  by,
  legend_title = "Income group",
  main_text_size = 14,
  alpha = 1
) {
  data <- contrast(emm, method = "pairwise") |>
    as.data.frame()

  by_sym <- ensym(by)

  ggplot(data, aes(
      x = treatment,
      y = estimate,
      color = !!by_sym,
      group = !!by_sym
    )) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * SE,
        ymax = estimate + 1.96 * SE
      ),
      width = 0.2,
      position = position_dodge(0.3),
      alpha = alpha
    ) +
    labs(
      y = NULL,
      x = NULL,
      color = legend_title
    ) +
    scale_color_viridis_d(option = "plasma", end = .8) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    ylim(1,4) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))
}

plot_flights_income <- plot_flights_subgroup(
  emm_flights_income,
  by = income_group
) +
  labs(title = "C. Change in planned flights")



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
    contr_overall <- contr_overall |>
      mutate(!!by_sym := "Overall")
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

  legend_levels <- c(
    setdiff(unique(contr_plot$legend_label), "Overall"),
    "Overall"
  )

  contr_plot$legend_label <- factor(
    contr_plot$legend_label,
    levels = legend_levels
  )

  p <- ggplot(contr_plot, aes(
    x = contrast,
    y = estimate,
    color = legend_label,
    group = legend_label
  )) +
    geom_point(position = position_dodge(0.3), size = 3, alpha = alpha) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * SE,
        ymax = estimate + 1.96 * SE
      ),
      width = 0.2,
      position = position_dodge(0.3),
      alpha = alpha
    ) +
    geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "gray40",
      linewidth = 0.3
    ) +
    labs(
      y = "Contrast with control",
      x = NULL
    ) +
    ylim(-1.65, 1.5) +
    theme_classic() +
    theme(text = element_text(size = main_text_size))

  if (!is.null(color_scale)) {
    p <- p + color_scale
  }
  return(p)
}



plot_contr_wtp_income <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_income,
  contr_overall  = contr_wtp,
  by = "income_group",
  legend_title = "Income group",
  color_scale = scale_income
)

plot_contr_wtc_income <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_income,
  contr_overall  = contr_wtc,
  by = "income_group",
  legend_title = "Income group",
  color_scale = scale_income
)

plot_contr_wtp_flyer <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_flyer,
  contr_overall  = contr_wtp,
  by = "flying_group",
  legend_title = "Flying behaviour",
  color_scale = scale_flyer
)

plot_contr_wtc_flyer <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_flyer,
  contr_overall  = contr_wtc,
  by = "flying_group",
  legend_title = "Flying behaviour",
  color_scale = scale_flyer
)

plot_contr_wtp_clim <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtp_clim,
  contr_overall  = contr_wtp,
  by = "clim_concern",
  legend_title = "Climate concern",
  color_scale = scale_clim
)

plot_contr_wtc_clim <- plot_contrasts_subgroup(
  contr_subgroup = contr_wtc_clim,
  contr_overall  = contr_wtc,
  by = "clim_concern",
  legend_title = "Climate concern",
  color_scale = scale_clim
)


############ compound plots #################

plot_income_emm_contr <- (plot_wtp_income | plot_contr_wtp_income) /
  (plot_wtc_income | plot_contr_wtc_income) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

plot_flyer_emm_contr <- (plot_wtp_flyer | plot_contr_wtp_flyer) /
      (plot_wtc_flyer | plot_contr_wtc_flyer) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom")

plot_clim_emm_contr <- (plot_wtp_clim | plot_contr_wtp_clim) /
  (plot_wtc_clim | plot_contr_wtc_clim) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")


################### save stuff #####################


ggsave(
  plot = plot_income_emm_contr,
  here("output", "plot_income_emm_contr.png"),
  height = 10, width = 15
)

ggsave(
  plot = plot_flyer_emm_contr,
  here("output", "plot_flyer_emm_contr.png"),
  height = 10, width = 15
)

ggsave(
  plot = plot_clim_emm_contr,
  here("output", "plot_clim_emm_contr.png"),
  height = 10, width = 15
)