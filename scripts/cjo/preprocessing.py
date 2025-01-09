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

# Get the total number of rows across all dataframes
total_rows = sum(len(df) for df in dataframes.values())

# Initialize an ID counter
id_counter = 1

# Assign unique IDs across all countries
for country, df in dataframes.items():
    # Create a new 'ID' column starting from the current id_counter
    df['id'] = range(id_counter, id_counter + len(df))
    
    # Update the ID counter for the next dataframe
    id_counter += len(df)

    # Update the dataframe in the dictionary
    dataframes[country] = df

us_df = dataframes["US"]
ch_df = dataframes["CH"]
cn_df = dataframes["CN"]

# %% clean data for LPA 

# make justice columns numeric
columns_to_num = ['justice_general_1', 
                  'justice_general_2', 
                  'justice_general_3', 
                  'justice_general_4',
                  'justice_tax_1', 
                  'justice_tax_2', 
                  'justice_tax_3',
                  'justice_tax_4',
                  'justice_subsidy_1',
                  'justice_subsidy_2',
                  'justice_subsidy_3',
                  'justice_subsidy_4',
                  ]

for country, df in dataframes.items():
    for col in columns_to_num:
        df[col] = pd.to_numeric(df[col], errors='coerce')

    dataframes[country] = df

lpa_data = []

for country, df in dataframes.items():
    cols = ['justice_general_1', 'justice_general_2', 'justice_general_3', 'justice_general_4',
            'justice_tax_1', 'justice_tax_2', 'justice_tax_3', 'justice_tax_4',
            'justice_subsidy_1', 'justice_subsidy_2', 'justice_subsidy_3', 'justice_subsidy_4', 'id']
    
    # create a new dataframe and add a 'country' column
    df_selected = df[cols].copy()
    df_selected['country'] = country
    
    # Append the new dataframe to the list
    lpa_data.append(df_selected)

lpa_data = pd.concat(lpa_data, ignore_index=True)

justice_columns = {
    'utilitarian': ['justice_general_1', 'justice_tax_1', 'justice_subsidy_1'], 
    'egalitarian': ['justice_general_2', 'justice_tax_2', 'justice_subsidy_2'], 
    'sufficientarian': ['justice_general_3', 'justice_tax_3', 'justice_subsidy_3'], 
    'limitarian': ['justice_general_4', 'justice_tax_4', 'justice_subsidy_4']
}

# convert columns to numeric types
for just_columns in justice_columns.values():
    for col in just_columns:
        lpa_data[col] = pd.to_numeric(lpa_data[col], errors='coerce')

# get mean for each key and append to dataframe
for key, just_columns in justice_columns.items():
    lpa_data[key] = lpa_data[just_columns].mean(axis=1).round(3)

lpa_input = lpa_data[[
    'id', 
    "country",
    'utilitarian',
    'egalitarian',
    'sufficientarian',
    'limitarian',
]]

lpa_data.to_csv("data/cjo_icc_input.csv")
lpa_input.to_csv("data/lpa_input.csv")

# %%
