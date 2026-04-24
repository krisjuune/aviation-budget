# data
RAW_US              = "raw-data/Aviation_Justice_US_111224_1531.csv"
RAW_CH              = "raw-data/Aviation_Justice_CH_111224_1531.csv"
RAW_CN              = "raw-data/Aviation_Justice_CN_111224_1532.csv"
CLEAN_US            = "data/data_clean_us.csv"
CLEAN_CH            = "data/data_clean_ch.csv"
CLEAN_CN            = "data/data_clean_cn.csv"
WTC_WTP_DATA        = "data/wtc_wtp_tidy.csv"
WTC_WTP_FAIR_DATA   = "data/wtc_wtp_fair_tidy.csv"
WTC_WTP_CTRL_DATA   = "data/wtc_wtp_controls_tidy.csv"

# robustness check outputs
ROB_EMM_WTC_HIGHINCOME     = "data/emm_wtc_rob_highincome.csv"
ROB_EMM_WTP_HIGHINCOME     = "data/emm_wtp_rob_highincome.csv"
ROB_CONTR_WTC_HIGHINCOME   = "data/contr_wtc_rob_highincome.csv"
ROB_CONTR_WTP_HIGHINCOME   = "data/contr_wtp_rob_highincome.csv"
ROB_EMM_WTC_LOWINCOME      = "data/emm_wtc_rob_lowincome.csv"
ROB_EMM_WTP_LOWINCOME      = "data/emm_wtp_rob_lowincome.csv"
ROB_CONTR_WTC_LOWINCOME    = "data/contr_wtc_rob_lowincome.csv"
ROB_CONTR_WTP_LOWINCOME    = "data/contr_wtp_rob_lowincome.csv"
ROB_EMM_WTC_NONFLIERS_CN   = "data/emm_wtc_rob_nonfliers_cn.csv"
ROB_EMM_WTP_NONFLIERS_CN   = "data/emm_wtp_rob_nonfliers_cn.csv"
ROB_CONTR_WTC_NONFLIERS_CN = "data/contr_wtc_rob_nonfliers_cn.csv"
ROB_CONTR_WTP_NONFLIERS_CN = "data/contr_wtp_rob_nonfliers_cn.csv"
ROB_EMM_WTC_SPEEDERS       = "data/emm_wtc_rob_speeders.csv"
ROB_EMM_WTP_SPEEDERS       = "data/emm_wtp_rob_speeders.csv"
ROB_CONTR_WTC_SPEEDERS     = "data/contr_wtc_rob_speeders.csv"
ROB_CONTR_WTP_SPEEDERS     = "data/contr_wtp_rob_speeders.csv"
ROB_TABLES                 = "output/robustness_tables.tex"

# data outputs
EMM_WTC               = "data/emm_wtc.csv"
EMM_WTP               = "data/emm_wtp.csv"
EMM_WTC_REDAMT        = "data/emm_wtc_redamt.csv"
EMM_WTP_REDAMT        = "data/emm_wtp_redamt.csv"
EMM_WTP_HAUL          = "data/emm_wtp_haul.csv"
CONTR_WTC             = "data/contr_wtc.csv"
CONTR_WTP             = "data/contr_wtp.csv"
EMM_WTC_INCOME        = "data/emm_wtc_income.csv"
EMM_WTP_INCOME        = "data/emm_wtp_income.csv"
CONTR_WTC_INCOME      = "data/contr_wtc_income.csv"
CONTR_WTP_INCOME      = "data/contr_wtp_income.csv"
CONTR_FLIGHTS_INCOME  = "data/contr_flights_income.csv"
EMM_WTC_FLIER         = "data/emm_wtc_flier.csv"
EMM_WTP_FLIER         = "data/emm_wtp_flier.csv"
CONTR_WTC_FLIER       = "data/contr_wtc_flier.csv"
CONTR_WTP_FLIER       = "data/contr_wtp_flier.csv"
CONTR_FLIGHTS_FLIER   = "data/contr_flights_flier.csv"
EMM_WTC_CLIM          = "data/emm_wtc_clim.csv"
EMM_WTP_CLIM          = "data/emm_wtp_clim.csv"
CONTR_WTC_CLIM        = "data/contr_wtc_clim.csv"
CONTR_WTP_CLIM        = "data/contr_wtp_clim.csv"
CONTR_FLIGHTS_CLIM    = "data/contr_flights_clim.csv"
EMM_WTC_PURPOSE       = "data/emm_wtc_purpose.csv"
EMM_WTP_PURPOSE       = "data/emm_wtp_purpose.csv"
CONTR_WTC_PURPOSE     = "data/contr_wtc_purpose.csv"
CONTR_WTP_PURPOSE     = "data/contr_wtp_purpose.csv"
CONTR_FLIGHTS_PURPOSE = "data/contr_flights_purpose.csv"
CORR_INCOME_FLYING    = "data/corr_income_flying.csv"

# sample exploration plots
PLOT_SAMPLE_DISTS        = "output/plot_sample_distributions.png"
PLOT_FLYING_PURPOSE      = "output/plot_flying_purpose.png"
PLOT_FLYING_PURPOSE_COV  = "output/plot_flying_purpose_covariates.png"
PLOT_FAIRNESS_ASSOC      = "output/plot_fairness_associations.png"
PLOT_CORRELATIONS        = "output/plot_variable_correlations.png"

# output plots and txt files
SAMPLE_SUMMARY        = "output/sample_summary.txt"
ASSUMPTIONS_WTC       = "output/assumptions_wtc.png"
ASSUMPTIONS_WTP       = "output/assumptions_wtp.png"
COVARIATES_PLOT       = "output/plot_covariates.png"
OVERALL_PLOT          = "output/plot_overall_results.png"
PLOT_INCOME_FLYING    = "output/plot_income_flying.png"
FAIRNESS_PLOT         = "output/fairness_scores.png"
FAIRNESS_REPORT       = "output/fairness_means.txt"
PLOT_INCOME_EMM_CONTR  = "output/plot_income_emm_contr.png"
PLOT_FLIER_EMM_CONTR   = "output/plot_flier_emm_contr.png"
PLOT_CLIM_EMM_CONTR    = "output/plot_clim_emm_contr.png"
PLOT_PURPOSE_EMM_CONTR = "output/plot_purpose_emm_contr.png"
PLOT_CONTR_COMBINED    = "output/plot_contr_flier_purpose.png"


rule all:
    input:
        CLEAN_US,
        CLEAN_CH,
        CLEAN_CN,
        WTC_WTP_DATA,
        WTC_WTP_FAIR_DATA,
        WTC_WTP_CTRL_DATA,
        SAMPLE_SUMMARY,
        EMM_WTC,
        EMM_WTP,
        EMM_WTC_REDAMT,
        EMM_WTP_REDAMT,
        EMM_WTP_HAUL,
        CONTR_WTC,
        CONTR_WTP,
        ASSUMPTIONS_WTC,
        ASSUMPTIONS_WTP,
        COVARIATES_PLOT,
        EMM_WTC_INCOME,
        EMM_WTP_INCOME,
        CONTR_WTC_INCOME,
        CONTR_WTP_INCOME,
        CONTR_FLIGHTS_INCOME,
        EMM_WTC_FLIER,
        EMM_WTP_FLIER,
        CONTR_WTC_FLIER,
        CONTR_WTP_FLIER,
        CONTR_FLIGHTS_FLIER,
        EMM_WTC_CLIM,
        EMM_WTP_CLIM,
        CONTR_WTC_CLIM,
        CONTR_WTP_CLIM,
        CONTR_FLIGHTS_CLIM,
        EMM_WTC_PURPOSE,
        EMM_WTP_PURPOSE,
        CONTR_WTC_PURPOSE,
        CONTR_WTP_PURPOSE,
        CONTR_FLIGHTS_PURPOSE,
        OVERALL_PLOT,
        CORR_INCOME_FLYING,
        PLOT_INCOME_FLYING,
        FAIRNESS_PLOT,
        FAIRNESS_REPORT,
        PLOT_INCOME_EMM_CONTR,
        PLOT_FLIER_EMM_CONTR,
        PLOT_CLIM_EMM_CONTR,
        PLOT_PURPOSE_EMM_CONTR,
        PLOT_CONTR_COMBINED,
        ROB_EMM_WTC_HIGHINCOME,
        ROB_EMM_WTP_HIGHINCOME,
        ROB_CONTR_WTC_HIGHINCOME,
        ROB_CONTR_WTP_HIGHINCOME,
        ROB_EMM_WTC_LOWINCOME,
        ROB_EMM_WTP_LOWINCOME,
        ROB_CONTR_WTC_LOWINCOME,
        ROB_CONTR_WTP_LOWINCOME,
        ROB_EMM_WTC_NONFLIERS_CN,
        ROB_EMM_WTP_NONFLIERS_CN,
        ROB_CONTR_WTC_NONFLIERS_CN,
        ROB_CONTR_WTP_NONFLIERS_CN,
        ROB_EMM_WTC_SPEEDERS,
        ROB_EMM_WTP_SPEEDERS,
        ROB_CONTR_WTC_SPEEDERS,
        ROB_CONTR_WTP_SPEEDERS,
        ROB_TABLES,
        PLOT_SAMPLE_DISTS,
        PLOT_FLYING_PURPOSE,
        PLOT_FLYING_PURPOSE_COV,
        PLOT_FAIRNESS_ASSOC,
        PLOT_CORRELATIONS

# ----------------------------
# Preprocess data
# ----------------------------
rule preprocess_basics:
    input:
        us = RAW_US,
        ch = RAW_CH,
        cn = RAW_CN
    output:
        us = CLEAN_US,
        ch = CLEAN_CH,
        cn = CLEAN_CN
    script:
        "scripts/preprocessing/00_preprocessing_basics.py"

rule preprocess_lmm:
    input:
        us = CLEAN_US,
        ch = CLEAN_CH,
        cn = CLEAN_CN
    output:
        wtc_wtp          = WTC_WTP_DATA,
        wtc_wtp_fair     = WTC_WTP_FAIR_DATA,
        wtc_wtp_controls = WTC_WTP_CTRL_DATA
    script:
        "scripts/preprocessing/01_preprocessing_lmm.R"

# ----------------------------
# Sample description
# ----------------------------
rule sample_description:
    input:
        us       = CLEAN_US,
        ch       = CLEAN_CH,
        cn       = CLEAN_CN,
        controls = WTC_WTP_CTRL_DATA
    output:
        summary = SAMPLE_SUMMARY
    script:
        "scripts/analysis/02_sample_description.R"

# ----------------------------
# LMM analysis
# ----------------------------
rule lmm_simple_models:
    input:
        wtc_wtp      = WTC_WTP_DATA,
        wtc_wtp_fair = WTC_WTP_FAIR_DATA
    output:
        emm_wtc         = EMM_WTC,
        emm_wtp         = EMM_WTP,
        emm_wtc_redamt  = EMM_WTC_REDAMT,
        emm_wtp_redamt  = EMM_WTP_REDAMT,
        emm_wtp_haul    = EMM_WTP_HAUL,
        contr_wtc       = CONTR_WTC,
        contr_wtp       = CONTR_WTP,
        assumptions_wtc = ASSUMPTIONS_WTC,
        assumptions_wtp = ASSUMPTIONS_WTP
    script:
        "scripts/analysis/04_lmm_simple_models.R"

rule lmm_covariates:
    input:
        controls = WTC_WTP_CTRL_DATA,
        fair     = WTC_WTP_FAIR_DATA
    output:
        covariates_plot = COVARIATES_PLOT
    script:
        "scripts/analysis/05_lmm_covariates.R"

rule lmm_subgroups:
    input:
        controls = WTC_WTP_CTRL_DATA,
        fair     = WTC_WTP_FAIR_DATA
    output:
        emm_wtc_income       = EMM_WTC_INCOME,
        emm_wtp_income       = EMM_WTP_INCOME,
        contr_wtc_income     = CONTR_WTC_INCOME,
        contr_wtp_income     = CONTR_WTP_INCOME,
        contr_flights_income = CONTR_FLIGHTS_INCOME,
        emm_wtc_flier        = EMM_WTC_FLIER,
        emm_wtp_flier        = EMM_WTP_FLIER,
        contr_wtc_flier      = CONTR_WTC_FLIER,
        contr_wtp_flier      = CONTR_WTP_FLIER,
        contr_flights_flier  = CONTR_FLIGHTS_FLIER,
        emm_wtc_clim         = EMM_WTC_CLIM,
        emm_wtp_clim         = EMM_WTP_CLIM,
        contr_wtc_clim       = CONTR_WTC_CLIM,
        contr_wtp_clim       = CONTR_WTP_CLIM,
        contr_flights_clim   = CONTR_FLIGHTS_CLIM,
        emm_wtc_purpose       = EMM_WTC_PURPOSE,
        emm_wtp_purpose       = EMM_WTP_PURPOSE,
        contr_wtc_purpose     = CONTR_WTC_PURPOSE,
        contr_wtp_purpose     = CONTR_WTP_PURPOSE,
        contr_flights_purpose = CONTR_FLIGHTS_PURPOSE
    script:
        "scripts/analysis/06_lmm_subgroups.R"

# ----------------------------
# Plotting
# ----------------------------

rule plot_overall_emm:
    input:
        emm_wtc   = EMM_WTC,
        emm_wtp   = EMM_WTP,
        contr_wtc = CONTR_WTC,
        contr_wtp = CONTR_WTP
    output:
        overall_plot = OVERALL_PLOT
    script:
        "scripts/plots/10_overall_emm.R"

rule plot_income_flying_corr:
    input:
        controls = WTC_WTP_CTRL_DATA
    output:
        corr = CORR_INCOME_FLYING,
        plot = PLOT_INCOME_FLYING
    script:
        "scripts/plots/11_income_flying_corr.R"

rule plot_fairness_scores:
    input:
        fair = WTC_WTP_FAIR_DATA
    output:
        fairness_plot   = FAIRNESS_PLOT,
        fairness_report = FAIRNESS_REPORT
    script:
        "scripts/plots/12_fairness_scores.R"

rule plot_subgroup_emm:
    input:
        emm_wtc               = EMM_WTC,
        emm_wtp               = EMM_WTP,
        contr_wtc             = CONTR_WTC,
        contr_wtp             = CONTR_WTP,
        emm_wtc_income        = EMM_WTC_INCOME,
        emm_wtp_income        = EMM_WTP_INCOME,
        contr_wtc_income      = CONTR_WTC_INCOME,
        contr_wtp_income      = CONTR_WTP_INCOME,
        contr_flights_income  = CONTR_FLIGHTS_INCOME,
        emm_wtc_flier         = EMM_WTC_FLIER,
        emm_wtp_flier         = EMM_WTP_FLIER,
        contr_wtc_flier       = CONTR_WTC_FLIER,
        contr_wtp_flier       = CONTR_WTP_FLIER,
        contr_flights_flier   = CONTR_FLIGHTS_FLIER,
        emm_wtc_clim          = EMM_WTC_CLIM,
        emm_wtp_clim          = EMM_WTP_CLIM,
        contr_wtc_clim        = CONTR_WTC_CLIM,
        contr_wtp_clim        = CONTR_WTP_CLIM,
        contr_flights_clim    = CONTR_FLIGHTS_CLIM,
        emm_wtc_purpose        = EMM_WTC_PURPOSE,
        emm_wtp_purpose        = EMM_WTP_PURPOSE,
        contr_wtc_purpose      = CONTR_WTC_PURPOSE,
        contr_wtp_purpose      = CONTR_WTP_PURPOSE,
        contr_flights_purpose  = CONTR_FLIGHTS_PURPOSE
    output:
        income_emm_contr  = PLOT_INCOME_EMM_CONTR,
        flier_emm_contr   = PLOT_FLIER_EMM_CONTR,
        clim_emm_contr    = PLOT_CLIM_EMM_CONTR,
        purpose_emm_contr = PLOT_PURPOSE_EMM_CONTR,
        contr_combined    = PLOT_CONTR_COMBINED
    script:
        "scripts/plots/13_subgroup_emm.R"

rule robustness_checks:
    input:
        controls = WTC_WTP_CTRL_DATA,
        us       = CLEAN_US,
        ch       = CLEAN_CH,
        cn       = CLEAN_CN
    output:
        emm_wtc_highincome     = ROB_EMM_WTC_HIGHINCOME,
        emm_wtp_highincome     = ROB_EMM_WTP_HIGHINCOME,
        contr_wtc_highincome   = ROB_CONTR_WTC_HIGHINCOME,
        contr_wtp_highincome   = ROB_CONTR_WTP_HIGHINCOME,
        emm_wtc_lowincome      = ROB_EMM_WTC_LOWINCOME,
        emm_wtp_lowincome      = ROB_EMM_WTP_LOWINCOME,
        contr_wtc_lowincome    = ROB_CONTR_WTC_LOWINCOME,
        contr_wtp_lowincome    = ROB_CONTR_WTP_LOWINCOME,
        emm_wtc_nonfliers_cn   = ROB_EMM_WTC_NONFLIERS_CN,
        emm_wtp_nonfliers_cn   = ROB_EMM_WTP_NONFLIERS_CN,
        contr_wtc_nonfliers_cn = ROB_CONTR_WTC_NONFLIERS_CN,
        contr_wtp_nonfliers_cn = ROB_CONTR_WTP_NONFLIERS_CN,
        emm_wtc_speeders       = ROB_EMM_WTC_SPEEDERS,
        emm_wtp_speeders       = ROB_EMM_WTP_SPEEDERS,
        contr_wtc_speeders     = ROB_CONTR_WTC_SPEEDERS,
        contr_wtp_speeders     = ROB_CONTR_WTP_SPEEDERS,
        rob_tables             = ROB_TABLES
    script:
        "scripts/analysis/07_robustness_checks.R"

rule sample_exploration:
    input:
        controls = WTC_WTP_CTRL_DATA,
        fair     = WTC_WTP_FAIR_DATA,
        us       = CLEAN_US,
        ch       = CLEAN_CH,
        cn       = CLEAN_CN
    output:
        distributions      = PLOT_SAMPLE_DISTS,
        flying_purpose     = PLOT_FLYING_PURPOSE,
        purpose_covariates = PLOT_FLYING_PURPOSE_COV,
        fairness_assoc     = PLOT_FAIRNESS_ASSOC,
        correlations       = PLOT_CORRELATIONS
    script:
        "scripts/analysis/03_sample_exploration.R"