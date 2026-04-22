import pandas as pd
import sys
import os

# ----------------------------
# Snakemake / local paths
# ----------------------------
if 'snakemake' in dir():
    file_us = snakemake.input['us']
    file_ch = snakemake.input['ch']
    file_cn = snakemake.input['cn']
    out_us  = snakemake.output['us']
    out_ch  = snakemake.output['ch']
    out_cn  = snakemake.output['cn']
else:
    file_us = "raw-data/Aviation_Justice_US_111224_1531.csv"
    file_ch = "raw-data/Aviation_Justice_CH_111224_1531.csv"
    file_cn = "raw-data/Aviation_Justice_CN_111224_1532.csv"
    out_us  = "data/data_clean_us.csv"
    out_ch  = "data/data_clean_ch.csv"
    out_cn  = "data/data_clean_cn.csv"

# ----------------------------
# Import data
# ----------------------------
files = {"US": file_us, "CH": file_ch, "CN": file_cn}

dataframes = {}
for country, file_name in files.items():
    df = pd.read_csv(file_name, skiprows=[1, 2])
    dataframes[country] = df

# ----------------------------
# Filter previews, incompletes, screened out, failed traps
# ----------------------------
for country, df in dataframes.items():
    df = df[(df['DistributionChannel'] != 'preview') & (df['Finished'] != False)]
    df = df[(df['screened_out'] != "true")]
    df = df[(df['screened_out'] != "true_trap1")]
    df = df[(df['screened_out'] != "true_trap2")]
    df = df[(df['screened_out'] != "true_trap3")]
    df = df[(df['Q_TerminateFlag'] != "QuotaMet")]
    df = df[(df['Q_TerminateFlag'] != "Screened")]

    if country == "CH":
        df = df[(df['screened_out'] != "true_region")]

    dataframes[country] = df

# ----------------------------
# Fix typos
# ----------------------------
dataframes["US"]["personal_income"] = dataframes["US"]["personal_income"].replace("15k_35k", "15k_25k")
dataframes["CH"] = dataframes["CH"].rename(columns={"recent_flights": "flying_recent_number"})
dataframes["CN"] = dataframes["CN"].rename(columns={"recent_flights": "flying_recent_number"})

# ----------------------------
# Add response IDs
# ----------------------------
id_counter = 1
for country, df in dataframes.items():
    df['id'] = range(id_counter, id_counter + len(df))
    id_counter += len(df)
    dataframes[country] = df

# ----------------------------
# Save clean data
# ----------------------------
dataframes["US"].to_csv(out_us, index=False)
dataframes["CH"].to_csv(out_ch, index=False)
dataframes["CN"].to_csv(out_cn, index=False)