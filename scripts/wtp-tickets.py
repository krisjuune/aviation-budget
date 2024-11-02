import pandas as pd
from sympy import symbols, Eq, solve 

# %% #################### define parameters #######################
flight_types = ['short', 'long']

# long haul SAFs costs (corresponding to emissions reductions) per
# London - New York flight, assuming an average nr of passengers 
# of 255 on such a flight, costs are calculated based on 
# Brazzola et al 2024 and shown in eur

# short haul SAFs costs (corresponding to emissions reductions) 
# per London - Berlin flight, assuming an average nr of passengers 
# of 142 on such a flight, costs are calculated based on 
# Brazzola et al 2024 and shown in eur

# define stringency costs and number of passengers
stringency_costs = {
    'short': pd.DataFrame({'stringency': ['15%', '30%', '45%'], 'costs': [3597, 4135, 4754]}),
    'long': pd.DataFrame({'stringency': ['15%', '30%', '45%'], 'costs': [20785, 23894, 27469]})
}

nr_passengers = {
    'short': 142,
    'long': 255
}

# treatment levels
justice = [['egalitarian', 'limitarian', 'prioritarian', 'proportional']]

# income distribution for flying (from Statista)
income = {'low': [0.13, 0.13, 0.13], 
          'mid': [0.32, 0.32, 0.32], 
          'high': [0.55, 0.55, 0.55]}

# purpose distribution for flying (from Statista)
purpose = {'tourism': [0.5, 0.5, 0.5], 
           'non_tourism': [0.5, 0.5, 0.5]}

# assuming high-income flyers are frequent flyers
frequency = {'frequent': [0.55, 0.55, 0.55], 
             'non_frequent': [0.45, 0.45, 0.45]}

income = pd.DataFrame(income)
purpose = pd.DataFrame(purpose)
frequency = pd.DataFrame(frequency)

# constants for flying behaviour relative to low income group's flying
low_mid_ratio = income['mid'][0] / income['low'][0]
low_high_ratio = income['high'][0] / income['low'][0]

# currency conversion rates as of 04.10.24
eur_to_usd = 1.1
eur_to_chf = 0.94
eur_to_cny = 7.74

# %% ######################## calculate ###########################

cost_pp = {}

for flight_type in flight_types:
    stringency_df = stringency_costs[flight_type]
    num_passengers = nr_passengers[flight_type]
    
    cost_pp_type = dict()

    nr_low = round(income['low'][0]*num_passengers)
    nr_mid = round(income['mid'][0]*num_passengers)
    nr_high = round(income['high'][0]*num_passengers)

    # proportional
    cost_pp_type['proportional'] = stringency_df['costs'] / num_passengers

    # prioritarian
    cost_pp_type['tourism'] = stringency_df['costs'] / (num_passengers * purpose['tourism'])
    cost_pp_type['non_tourism'] = stringency_df['costs'] * 0

    # limitarian   
    cost_pp_type['frequent'] = stringency_df['costs'] / (num_passengers * frequency['frequent'])
    cost_pp_type['non_frequent'] = stringency_df['costs'] * 0

    # egalitarian
    # find egalitarian constant
    x = symbols('x')
    equation = Eq(nr_low * x + nr_mid * x * low_mid_ratio + nr_high * x * low_high_ratio, 1)
    constant = solve(equation, x)[0].evalf()

    # calculate costs per passenger
    cost_pp_type['low'] = (stringency_df['costs'] * constant).astype(float)
    cost_pp_type['mid'] = (stringency_df['costs'] * constant * low_mid_ratio).astype(float)
    cost_pp_type['high'] = (stringency_df['costs'] * constant * low_high_ratio).astype(float)

    # Save the DataFrame to the dictionary with the flight type as the key
    cost_pp[f'cost_pp_{flight_type}'] = pd.DataFrame(cost_pp_type)

# check cost_pp_short
print(cost_pp['cost_pp_short'])

# %% ######################### convert ###########################
#convert to usd, chf, and cny

# Create a dictionary to store converted costs
cost_pp_converted = {}

# Loop through each flight type in cost_pp
for flight_type, df in cost_pp.items():
    cost_pp_converted[flight_type] = {
        'usd': round(df * eur_to_usd),
        'chf': round(df * eur_to_chf),
        'cny': round(df * eur_to_cny)
    }

# check conversion for short haul flights in usd
cost_pp_short_usd = cost_pp_converted['cost_pp_short']['usd']
print(cost_pp_short_usd)

# %% ####################### write to file ########################
# write six files, one for each currency and flight type

for flight_type, conversions in cost_pp_converted.items():
    for currency, df in conversions.items():
        file_name = f"data/{flight_type}_{currency}.csv"
        df.to_csv(file_name, index=False)
# %%
