import pandas as pd

# %% import data 

files = {
    "US": "raw-data/Aviation_Justice_US_111224_1531.csv",
    "CH": "raw-data/Aviation_Justice_CH_111224_1531.csv",
    "CN": "raw-data/Aviation_Justice_CN_111224_1532.csv"
}


dataframes = {}

for country, file_name in files.items():
    # read the file, skipping the first two rows
    df = pd.read_csv(file_name, skiprows=[1, 2])
    
    # store the processed dataframe in the dictionary
    dataframes[country] = df

# %% filter out previews, incompletes, screened out, and failed traps

for country, df in dataframes.items():
    # filter out rows where DistributionChannel is 'preview' or screened_out is True
    df = df[(df['DistributionChannel'] != 'preview') & (df['Finished'] != False)]
    df = df[(df['screened_out'] != "true")]
    df = df[(df['screened_out'] != "true_trap1")]
    df = df[(df['screened_out'] != "true_trap2")]
    df = df[(df['screened_out'] != "true_trap3")]
    df = df[(df['Q_TerminateFlag'] != "QuotaMet")]
    df = df[(df['Q_TerminateFlag'] != "Screened")]

    if country == "CH":
        df = df[(df['screened_out'] != "true_region")]
    
    # update the dataframe in the dictionary
    dataframes[country] = df

# %% add response IDs

# get the total number of rows across all dataframes
total_rows = sum(len(df) for df in dataframes.values())

# initialize an ID counter
id_counter = 1

# Assign unique IDs across all countries
for country, df in dataframes.items():
    # create a new 'ID' column starting from the current id_counter
    df['id'] = range(id_counter, id_counter + len(df))
    
    # update the ID counter for the next dataframe
    id_counter += len(df)

    # turn pct in reduction amount to numeric values
    # df['red_amt'] = df['red_amt'].str.rstrip('%').astype(float) / 100

    dataframes[country] = df



us_df = dataframes["US"]
ch_df = dataframes["CH"]
cn_df = dataframes["CN"]

# %% save clean data

us_df.to_csv("data/data_clean_us.csv")
ch_df.to_csv("data/data_clean_ch.csv")
cn_df.to_csv("data/data_clean_cn.csv")

# %%
