import pandas as pd 
import numpy as np

#%% reset working directory
import os
print(os.getcwd())
wd = '/Users/kristiinajoon/Documents/Projects/aviation-budget'
os.chdir(wd)

#%% ############################# read data ##################################

df = pd.read_csv('data/survey-02-10-24.csv')

pd.set_option('display.max_columns', None)
columns = df.columns.tolist()
df = df.drop([0, 1]) # remove first two rows
df['ID'] = range(1, len(df) + 1)

# rename columns for power analysis 
df.rename(columns={
    'flights_plan': 'planned_flights',
    'c_red_flight_scale': 'control_wtc',
    'c_red_flight_plan': 'control_flights', 
    't_g_red_flight_scale': 'treatment_wtc', 
    't_g_red_flight_plan': 'treatment_flights'
    }, 
    inplace=True)

# %% ######################### synthetic data ################################

df = pd.DataFrame(columns=[
    'ID', 
    'planned_flights', 
    'control_wtc', 
    'control_flights', 
    'treatment_wtc', 
    'treatment_flights',
    'treatment'
])

# set sample size 
n = 1000
max_flights = 30
max_likert = 6

### populate the df with random data
# set see for reproducibility
np.random.seed(42)
df['ID'] = np.arange(1, n+1)
df['planned_flights'] = np.random.randint(0, max_flights + 1, size=n)
df['control_wtc'] = np.random.randint(1, max_likert + 1, size=n)
df['treatment'] = np.random.choice([1, 2, 3, 4], size=n)

# generate control and treatment flights
# 90% of the time the control flights should not exceed planned flights
# 10% of the time generate random flights

control_flights = []
treatment_flights = []
treatment_wtc = []

# generate control flights
# 90% chance that less or equal to planned flights
for flights in df['planned_flights']:
    if np.random.rand() < 0.9: 
        control_flights.append(np.random.randint(0, flights + 2))
    else:
        control_flights.append(np.random.randint(0, max_flights + 1))

# generate treatment flights and treatment wtc
for index in range(len(df)):
    # 60% chance for treatment flights to be less than control flights
    if np.random.rand() < 0.6:
        treatment_flights.append(np.random.randint(0, control_flights[index] + 1))
    else:
        treatment_flights.append(np.random.randint(0, max_flights + 1))

    # 60% chance for treatment wtc to be greater than control wtc
    if np.random.rand() < 0.6:
        treatment_wtc.append(np.random.randint(df['control_wtc'].iloc[index], max_likert + 1))
    else:
        treatment_wtc.append(np.random.randint(1, max_likert + 1))


# assign to df
df['control_flights'] = control_flights
df['treatment_flights'] = treatment_flights
df['treatment_wtc'] = treatment_wtc


# %%
