import pandas as pd
from sympy import symbols, Eq, solve 

# %% #################### define parameters #######################
# emissions reduction levels with corresponding SAFs costs per 
# London - New York flight, assuming an average nr of passengers 
# of 255 on such a flight, costs are calculated based on 
# Brazzola et al 2024 and shown in eur
stringency_costs = {'stringency': ['15%', '30%', '45%'], 
                    'costs': [5502, 8111, 9997]}
stringency_costs = pd.DataFrame(stringency_costs)
nr_passengers = 255 

# treatment levels
justice = [['egalitarian', 'limitarian', 'prioritarian', 'proportional']]

# income and purpose distribution
#TODO consider making this country-specific
income = {'low': [0.1, 0.1, 0.1], 
          'mid': [0.17, 0.17, 0.17], 
          'high': [0.73, 0.73, 0.73]}

purpose = {'tourism': [0.5, 0.5, 0.5], 
           'non_tourism': [0.5, 0.5, 0.5]}

# assuming high-income flyers are frequent flyers
frequency = {'frequent': [0.73, 0.73, 0.73], 
             'non_frequent': [0.27, 0.27, 0.27]}

income = pd.DataFrame(income)
purpose = pd.DataFrame(purpose)
frequency = pd.DataFrame(frequency)

# constants for flying behaviour relative to low income group's flying
low_mid_ratio = 1.7 
low_high_ratio = 7
nr_low = round(income['low'][0]*nr_passengers)
nr_mid = round(income['mid'][0]*nr_passengers)
nr_high = round(income['high'][0]*nr_passengers)

# currency conversion rates as of 04.10.24
eur_to_usd = 1.1
eur_to_chf = 0.94
eur_to_cny = 7.74

# %% ######################## calculate ###########################

cost_pp = dict()

# proportional
cost_pp['proportional'] = stringency_costs['costs']/nr_passengers

# prioritarian
cost_pp['tourism'] = stringency_costs['costs']/(nr_passengers*purpose['tourism'])

cost_pp['non_tourism'] = stringency_costs['costs']*0

# limitarian   
cost_pp['frequent'] = stringency_costs['costs']/(nr_passengers*frequency['frequent'])

cost_pp['non_frequent'] = stringency_costs['costs']*0

# egalitarian
# find egalitarian constant
x = symbols('x')
equation = Eq(nr_low*x + 
              nr_mid*x*low_mid_ratio + 
              nr_high*x*low_high_ratio, 
              1)

constant = solve(equation, x)[0].evalf()

# calculate costs pp
cost_pp['low'] = (stringency_costs['costs']*constant).astype(float)
cost_pp['mid'] = (stringency_costs['costs']*constant*low_mid_ratio).astype(float)
cost_pp['high'] = (stringency_costs['costs']*constant*low_high_ratio).astype(float)

cost_pp = pd.DataFrame(cost_pp)

# %% ######################### convert ###########################
#convert to usd, chf, and cny

cost_pp_usd = round(cost_pp*eur_to_usd)
cost_pp_chf = round(cost_pp*eur_to_chf)
cost_pp_cny = round(cost_pp*eur_to_cny)


# %% ####################### write to file ########################
# write three files, one for each currency

cost_pp_usd.to_csv('data/cost_pp_usd.csv', index=False)
cost_pp_chf.to_csv('data/cost_pp_chf.csv', index=False)
cost_pp_cny.to_csv('data/cost_pp_cny.csv', index=False)
