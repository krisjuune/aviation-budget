import pandas as pd

#%% Step 1: Load the CSV file into a DataFrame
# Replace 'data_031224_day/CH_Aviation_Justice_031224_0830.csv' with the correct path to your CSV file
df_test = pd.read_csv('data_031224_day/CH_Aviation_Justice_031224_0830.csv', skiprows=[1, 2])

#%% Step 2: Filter data
df_test = df_test[(df_test['DistributionChannel'] != 'preview') & (df_test['Finished'] != False)]
df_test = df_test[(df_test['screened_out'] != "true")]
df_test = df_test[(df_test['screened_out'] != "true_trap1")]
df_test = df_test[(df_test['screened_out'] != "true_trap2")]
df_test = df_test[(df_test['screened_out'] != "true_trap3")]
df_test = df_test[(df_test['Q_TerminateFlag'] != "QuotaMet")]
df_test = df_test[(df_test['Q_TerminateFlag'] != "Screened")]
df_test = df_test[(df_test['screened_out'] != "true_region")]
df_test = df_test[(df_test['screened_out'].notna())]

#%% Step 3: Count rows where the column 'flying_plan' is equal to 'no'
flying_plan_no_count = df_test[df_test['flying_plan'] == 'no'].shape[0]

#%% Step 4: Count distinct values in the columns 'age', 'gender', and 'ch_region'
distinct_age_count = df_test['age'].nunique()
distinct_gender_count = df_test['gender'].nunique()
distinct_ch_region_count = df_test['ch_region'].nunique()
distinct_nonflying_count = df_test['flying_plan'].nunique()

#%% Step 5: Count occurrences of each distinct value in 'age', 'gender', and 'ch_region'
age_value_counts = df_test['age'].value_counts()
gender_value_counts = df_test['gender'].value_counts()
ch_region_value_counts = df_test['ch_region'].value_counts()
nonflying_value_counts = df_test['flying_plan'].value_counts()

#%% Step 6: Count cross-categories for 'gender', 'age', and 'ch_region'
cross_category_counts = df_test.groupby(['gender', 'age', 'ch_region']).size()


#%%
