library(lme4)
library(emmeans)
library(dplyr)
library(forcats)
library(tidyverse)
library(here)

if (exists("snakemake")) {
  controls_file <- snakemake@input[["controls"]]
  us_file       <- snakemake@input[["us"]]
  ch_file       <- snakemake@input[["ch"]]
  cn_file       <- snakemake@input[["cn"]]
  out_emm_wtc_highincome     <- snakemake@output[["emm_wtc_highincome"]]
  out_emm_wtp_highincome     <- snakemake@output[["emm_wtp_highincome"]]
  out_contr_wtc_highincome   <- snakemake@output[["contr_wtc_highincome"]]
  out_contr_wtp_highincome   <- snakemake@output[["contr_wtp_highincome"]]
  out_emm_wtc_lowincome      <- snakemake@output[["emm_wtc_lowincome"]]
  out_emm_wtp_lowincome      <- snakemake@output[["emm_wtp_lowincome"]]
  out_contr_wtc_lowincome    <- snakemake@output[["contr_wtc_lowincome"]]
  out_contr_wtp_lowincome    <- snakemake@output[["contr_wtp_lowincome"]]
  out_emm_wtc_nonfliers_cn   <- snakemake@output[["emm_wtc_nonfliers_cn"]]
  out_emm_wtp_nonfliers_cn   <- snakemake@output[["emm_wtp_nonfliers_cn"]]
  out_contr_wtc_nonfliers_cn <- snakemake@output[["contr_wtc_nonfliers_cn"]]
  out_contr_wtp_nonfliers_cn <- snakemake@output[["contr_wtp_nonfliers_cn"]]
  out_emm_wtc_speeders       <- snakemake@output[["emm_wtc_speeders"]]
  out_emm_wtp_speeders       <- snakemake@output[["emm_wtp_speeders"]]
  out_contr_wtc_speeders     <- snakemake@output[["contr_wtc_speeders"]]
  out_contr_wtp_speeders     <- snakemake@output[["contr_wtp_speeders"]]
  out_rob_tables             <- snakemake@output[["rob_tables"]]
} else {
  controls_file <- here("data", "wtc_wtp_controls_tidy.csv")
  us_file       <- here("data", "data_clean_us.csv")
  ch_file       <- here("data", "data_clean_ch.csv")
  cn_file       <- here("data", "data_clean_cn.csv")
  out_emm_wtc_highincome     <- here("data", "emm_wtc_rob_highincome.csv")
  out_emm_wtp_highincome     <- here("data", "emm_wtp_rob_highincome.csv")
  out_contr_wtc_highincome   <- here("data", "contr_wtc_rob_highincome.csv")
  out_contr_wtp_highincome   <- here("data", "contr_wtp_rob_highincome.csv")
  out_emm_wtc_lowincome      <- here("data", "emm_wtc_rob_lowincome.csv")
  out_emm_wtp_lowincome      <- here("data", "emm_wtp_rob_lowincome.csv")
  out_contr_wtc_lowincome    <- here("data", "contr_wtc_rob_lowincome.csv")
  out_contr_wtp_lowincome    <- here("data", "contr_wtp_rob_lowincome.csv")
  out_emm_wtc_nonfliers_cn   <- here("data", "emm_wtc_rob_nonfliers_cn.csv")
  out_emm_wtp_nonfliers_cn   <- here("data", "emm_wtp_rob_nonfliers_cn.csv")
  out_contr_wtc_nonfliers_cn <- here("data", "contr_wtc_rob_nonfliers_cn.csv")
  out_contr_wtp_nonfliers_cn <- here("data", "contr_wtp_rob_nonfliers_cn.csv")
  out_emm_wtc_speeders       <- here("data", "emm_wtc_rob_speeders.csv")
  out_emm_wtp_speeders       <- here("data", "emm_wtp_rob_speeders.csv")
  out_contr_wtc_speeders     <- here("data", "contr_wtc_rob_speeders.csv")
  out_contr_wtp_speeders     <- here("data", "contr_wtp_rob_speeders.csv")
  out_rob_tables             <- here("output", "robustness_tables.tex")
}

###################### load data ######################

data_controls <- read_csv(controls_file, show_col_types = FALSE) |>
  filter(!is.na(wtc)) |>
  mutate(
    treatment = factor(treatment) |>
      fct_recode(
        "Control"         = "control",
        "Egalitarianism"  = "egal",
        "Limitarianism"   = "limit",
        "Prioritarianism" = "prior",
        "Proportionalism" = "prop"
      ) |>
      fct_relevel("Control"),
    red_amt       = factor(red_amt, levels = c("15%", "30%", "45%")),
    income_decile = as.integer(income_decile)
  )

duration_data <- bind_rows(
  read_csv(us_file, show_col_types = FALSE) |>
    select(id, duration = `Duration (in seconds)`),
  read_csv(ch_file, show_col_types = FALSE) |>
    select(id, duration = `Duration (in seconds)`),
  read_csv(cn_file, show_col_types = FALSE) |>
    select(id, duration = `Duration (in seconds)`)
)

data_controls <- data_controls |>
  left_join(duration_data, by = "id")

###################### helper functions ######################

run_robustness <- function(data) {
  model_wtc <- lmer(
    wtc ~ treatment + red_amt + (1 | country),
    data = data
  )
  model_wtp <- lmer(
    wtp ~ treatment + red_amt + relative_added_cost + (1 | country),
    data = data
  )

  emm_wtc   <- emmeans(model_wtc, ~ treatment) |> as.data.frame() |> as_tibble()
  emm_wtp   <- emmeans(model_wtp, ~ treatment) |> as.data.frame() |> as_tibble()
  contr_wtc <- contrast(
    emmeans(model_wtc, ~ treatment), method = "trt.vs.ctrl", ref = "Control"
  ) |> as.data.frame() |> as_tibble()
  contr_wtp <- contrast(
    emmeans(model_wtp, ~ treatment), method = "trt.vs.ctrl", ref = "Control"
  ) |> as.data.frame() |> as_tibble()

  list(
    emm_wtc   = emm_wtc,
    emm_wtp   = emm_wtp,
    contr_wtc = contr_wtc,
    contr_wtp = contr_wtp
  )
}

# Format estimate (SE) for a table cell
fmt_cell <- function(est, se, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), est, se)
}

make_rob_table <- function(full, filtered, caption, label) {
  trt_levels <- c(
    "Control", "Egalitarianism", "Limitarianism",
    "Prioritarianism", "Proportionalism"
  )
  contr_names <- paste(
    c("Egalitarianism", "Limitarianism", "Prioritarianism", "Proportionalism"),
    "- Control"
  )

  emm_rows <- sapply(trt_levels, function(trt) {
    fw  <- full$emm_wtc     |> filter(.data$treatment == trt)
    ff  <- filtered$emm_wtc |> filter(.data$treatment == trt)
    wp  <- full$emm_wtp     |> filter(.data$treatment == trt)
    wfp <- filtered$emm_wtp |> filter(.data$treatment == trt)
    trt_label <- if (trt == "Control") paste0("\\textit{", trt, "}") else trt
    paste0(
      trt_label, " & ",
      fmt_cell(fw$emmean, fw$SE), " & ",
      fmt_cell(ff$emmean, ff$SE), " & ",
      fmt_cell(wp$emmean, wp$SE), " & ",
      fmt_cell(wfp$emmean, wfp$SE), " \\\\"
    )
  }, USE.NAMES = FALSE)

  contr_rows <- sapply(contr_names, function(cn) {
    cl  <- sub(" - Control", "", cn)
    fc  <- full$contr_wtc     |> filter(contrast == cn)
    fic <- filtered$contr_wtc |> filter(contrast == cn)
    fp  <- full$contr_wtp     |> filter(contrast == cn)
    fip <- filtered$contr_wtp |> filter(contrast == cn)
    paste0(
      cl, " & ",
      fmt_cell(fc$estimate, fc$SE), " & ",
      fmt_cell(fic$estimate, fic$SE), " & ",
      fmt_cell(fp$estimate, fp$SE), " & ",
      fmt_cell(fip$estimate, fip$SE), " \\\\"
    )
  }, USE.NAMES = FALSE)

  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{WTC} & \\multicolumn{2}{c}{WTP} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    paste0(
      "\\textbf{Treatment} & Full sample & Filtered",
      " & Full sample & Filtered \\\\"
    ),
    "\\midrule",
    "",
    "\\addlinespace",
    "\\textbf{Marginal means} \\\\",
    emm_rows,
    "",
    "\\addlinespace",
    "\\textbf{Contrasts vs.\\ control} \\\\",
    contr_rows,
    "",
    "\\bottomrule",
    "\\end{tabular}",
    paste0(
      "\\caption{", caption,
      " Estimates are marginal means or treatment--control",
      " contrasts from linear mixed-effects models with a",
      " country random intercept. Standard errors in parentheses.}"
    ),
    paste0("\\label{tab:", label, "}"),
    "\\end{table}"
  )

  paste(lines, collapse = "\n")
}

###################### robustness checks ######################

results_full <- run_robustness(data_controls)

# Without highest income decile in CH and US
data_highincome <- data_controls |>
  filter(!(!is.na(income_decile) & country %in% c("ch", "us") & income_decile == 10))
results_highincome <- run_robustness(data_highincome)

# Without lowest four income deciles (all countries)
data_lowincome <- data_controls |>
  filter(is.na(income_decile) | income_decile > 4)
results_lowincome <- run_robustness(data_lowincome)

# Without non-fliers in China
data_nonfliers_cn <- data_controls |>
  filter(!(country == "cn" & flying_recent == "no"))
results_nonfliers_cn <- run_robustness(data_nonfliers_cn)

# Without speeders (fastest 5% by survey duration)
speeder_cutoff <- quantile(data_controls$duration, 0.05, na.rm = TRUE)
data_speeders <- data_controls |>
  filter(is.na(duration) | duration > speeder_cutoff)
results_speeders <- run_robustness(data_speeders)

###################### save csvs ######################

write.csv(results_highincome$emm_wtc,     out_emm_wtc_highincome)
write.csv(results_highincome$emm_wtp,     out_emm_wtp_highincome)
write.csv(results_highincome$contr_wtc,   out_contr_wtc_highincome)
write.csv(results_highincome$contr_wtp,   out_contr_wtp_highincome)

write.csv(results_lowincome$emm_wtc,      out_emm_wtc_lowincome)
write.csv(results_lowincome$emm_wtp,      out_emm_wtp_lowincome)
write.csv(results_lowincome$contr_wtc,    out_contr_wtc_lowincome)
write.csv(results_lowincome$contr_wtp,    out_contr_wtp_lowincome)

write.csv(results_nonfliers_cn$emm_wtc,   out_emm_wtc_nonfliers_cn)
write.csv(results_nonfliers_cn$emm_wtp,   out_emm_wtp_nonfliers_cn)
write.csv(results_nonfliers_cn$contr_wtc, out_contr_wtc_nonfliers_cn)
write.csv(results_nonfliers_cn$contr_wtp, out_contr_wtp_nonfliers_cn)

write.csv(results_speeders$emm_wtc,       out_emm_wtc_speeders)
write.csv(results_speeders$emm_wtp,       out_emm_wtp_speeders)
write.csv(results_speeders$contr_wtc,     out_contr_wtc_speeders)
write.csv(results_speeders$contr_wtp,     out_contr_wtp_speeders)

###################### save latex tables ######################

table_highincome <- make_rob_table(
  full     = results_full,
  filtered = results_highincome,
  caption  = paste0(
    "Robustness check: excluding the highest income decile in Switzerland and ",
    "the United States."
  ),
  label    = "rob-highincome"
)

table_lowincome <- make_rob_table(
  full     = results_full,
  filtered = results_lowincome,
  caption  = paste0(
    "Robustness check: excluding the lowest four ",
    "income deciles across all countries."
  ),
  label    = "rob-lowincome"
)

table_nonfliers_cn <- make_rob_table(
  full     = results_full,
  filtered = results_nonfliers_cn,
  caption  = "Robustness check: excluding non-fliers in China.",
  label    = "rob-nonfliers-cn"
)

table_speeders <- make_rob_table(
  full     = results_full,
  filtered = results_speeders,
  caption  = paste0(
    "Robustness check: excluding speeders, defined as respondents in the ",
    "fastest 5\\% of survey completion times."
  ),
  label    = "rob-speeders"
)

all_tables <- paste(
  c(
    "% Required LaTeX packages: \\usepackage{booktabs}, \\usepackage{float}",
    "",
    table_highincome,
    "",
    table_lowincome,
    "",
    table_nonfliers_cn,
    "",
    table_speeders
  ),
  collapse = "\n"
)

writeLines(all_tables, out_rob_tables)