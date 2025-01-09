import pandas as pd
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
