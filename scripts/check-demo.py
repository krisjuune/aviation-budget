import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots

# %% define target proportions

quotas = {
    "Demographic Variable": [
        "gender", "gender", 
        "age", "age", "age", "age", "age", "age", 
        "flying_recent", 
        "ch_region", "ch_region", "ch_region", "ch_region"
    ],
    "Category": [
        "male", "female", 
        "18y_24y", "25y_34y", "35y_44y", "45y_54y", "55y_64y", "65y_above", 
        "no", 
        "german", "french", "italian", "romansch"
    ],
    "US quota": [0.49, 0.51, 0.12, 0.18, 0.18, 0.16, 0.17, 0.19, 0.2, None, None, None, None],
    "CH quota": [0.5, 0.5, 0.09, 0.18, 0.19, 0.18, 0.18, 0.18, 0.2, 0.73, 0.27, None, None],
    "CN quota": [0.51, 0.49, 0.12, 0.2, 0.18, 0.16, 0.34, None, 0.3, None, None, None, None]
}

df_quotas = pd.DataFrame(quotas)

# %% import data

pd.set_option('display.max_columns', None)

files = {
    "US": "data/US_Aviation_Justice_271124.csv",
    "CH": "data/CH_Aviation_Justice_271124.csv",
    "CN": "data/CN_Aviation_Justice_271124.csv"
}

dataframes = {}

for country, file_name in files.items():
    # Read the file, skipping the first two rows
    df = pd.read_csv(file_name, skiprows=[1, 2])
    
    # Store the processed dataframe in the dictionary
    dataframes[country] = df

# %% filter data

for country, df in dataframes.items():
    # Filter out rows where DistributionChannel is 'preview' or screened_out is True
    df = df[(df['DistributionChannel'] != 'preview') & (df['screened_out'] != True)]
    
    # Update the dataframe in the dictionary
    dataframes[country] = df

# %% check data 

us_df = dataframes["US"]
ch_df = dataframes["CH"]
cn_df = dataframes["CN"]

# %% compare demographics

def calculate_proportions(df, demographic_var):
    """Calculate proportions for each quota variable."""
    proportions = {}
    for category in df_quotas[df_quotas["Demographic Variable"] == demographic_var]["Category"]:
        # Calculate proportion for each category, defaulting to 0 if not present
        proportions[category] = round(df[demographic_var].value_counts(normalize=True).get(category, 0), 2)
    return proportions

for country, df in dataframes.items():
    # Calculate proportions for each demographic variable
    gender_data = calculate_proportions(df, "gender")
    age_data = calculate_proportions(df, "age")
    region_data = (
        calculate_proportions(df, "ch_region") if country == "CH" else {}
    )
    flying_data = calculate_proportions(df, "flying_recent")

    # For China (CN), after calculating, aggregate 55y_64y and 65y_above
    if country == "CN":
        # Sum the proportions for 55y_64y and 65y_above
        combined_data["55y_64y"] = round(
            combined_data.get("55y_64y", 0) + combined_data.get("65y_above", 0), 2
        )
        # Set the 65y_above category to None
        combined_data["65y_above"] = None

    # Combine all proportions into one dictionary
    combined_data = {**gender_data, **age_data, **region_data, **flying_data}

    # Map the proportions to the 'Category' column in df_quotas
    df_quotas[f"{country} data"] = df_quotas["Category"].map(combined_data)

# %%
