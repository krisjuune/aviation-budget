import pandas as pd
import pingouin as pg

#%%
############### read data ###############

df = pd.read_csv("data/cjo_icc_input.csv")

# %%
############## run icc #################

df_long = df.melt(id_vars=['id'], 
                        value_vars=['justice_general_1', 'justice_tax_1', 'justice_subsidy_1',
                                    'justice_general_2', 'justice_tax_2', 'justice_subsidy_2',
                                    'justice_general_3', 'justice_tax_3', 'justice_subsidy_3',
                                    'justice_general_4', 'justice_tax_4', 'justice_subsidy_4'],
                        var_name='variable', value_name='score')

# Extract set number from variable name (1, 2, 3, or 4)
df_long['principle'] = df_long['variable'].str.extract(r'(\d+)')

icc_results = pg.intraclass_corr(data=df_long, 
                                 targets='id', 
                                 raters='principle', 
                                 ratings='score')

print(icc_results)

# %%
