import pandas as pd
import numpy as np
import pingouin as pg

# %%
############### read data ###############

df = pd.read_csv("data/cjo_icc_input.csv")
df = df[df['country'].isin(['US'])]

# %% 
############## prep data ################

df_long = df.melt(id_vars=['id'], 
                        value_vars=['justice_general_1', 'justice_tax_1', 'justice_subsidy_1',
                                    'justice_general_2', 'justice_tax_2', 'justice_subsidy_2',
                                    'justice_general_3', 'justice_tax_3', 'justice_subsidy_3',
                                    'justice_general_4', 'justice_tax_4', 'justice_subsidy_4'],
                        var_name='variable', value_name='score')

# Extract set number from variable name (1, 2, 3, or 4)
df_long['principle'] = df_long['variable'].str.extract(r'(\d+)')

std = df_long.groupby('id')['score'].std()

# %% 

############# test plots ###############

import seaborn as sns
sns.boxplot(data=df_long, x="principle", y="score")

df_long.groupby('id')['score'].std().hist(bins=25)

# %%
############## run icc #################

icc_results = []

for principle in df_long['principle'].unique():
    df_subset = df_long[df_long['principle'] == principle]
    icc = pg.intraclass_corr(
        data=df_subset, 
        targets='id', 
        raters='variable', 
        ratings='score'
    )
    icc_3_1 = icc[icc['Type'] == 'ICC3'].copy()
    icc_3_1['principle'] = principle
    icc_results.append(icc_3_1)


icc_results = pd.concat(icc_results)

print(icc_results[['principle', 'ICC', 'CI95%']])

# %%
########## cronbach's alpha #############

# check if the items under one primciples are measuring the same thing
# whether there's internal consistency 

def cronbach_alpha(df):
    # df should be a dataframe where each column is a measurement/item for a principle
    item_scores = df.values
    item_variances = item_scores.var(axis=0, ddof=1)
    total_score_var = item_scores.sum(axis=1).var(ddof=1)
    n_items = df.shape[1]
    
    return (n_items / (n_items - 1)) * (1 - item_variances.sum() / total_score_var)

# Example usage per principle:
alpha_results = {}
for principle in df_long['principle'].unique():
    # Pivot to create one column per variable (item) for each principle
    df_pivot = df_long[df_long['principle'] == principle].pivot(index='id', columns='variable', values='score')
    alpha_results[principle] = cronbach_alpha(df_pivot)

alpha_results

# %%
