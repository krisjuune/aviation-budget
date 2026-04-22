RAW_US = "raw-data/Aviation_Justice_US_111224_1531.csv"
RAW_CH = "raw-data/Aviation_Justice_CH_111224_1531.csv"
RAW_CN = "raw-data/Aviation_Justice_CN_111224_1532.csv"
CLEAN_US = "data/data_clean_us.csv"
CLEAN_CH = "data/data_clean_ch.csv"
CLEAN_CN = "data/data_clean_cn.csv"
WTC_WTP_DATA        = "data/wtc_wtp_tidy.csv"
WTC_WTP_FAIR_DATA   = "data/wtc_wtp_fair_tidy.csv"
WTC_WTP_CTRL_DATA   = "data/wtc_wtp_controls_tidy.csv"

SAMPLE_SUMMARY = "output/sample_summary.txt"

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