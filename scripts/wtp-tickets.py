import numpy as np # maybe not needed
import pandas as pd
#TODO install sympy
# from sympy import symbols, Eq, solve 

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

# currency conversion rates as of 04.10.24
eur_to_usd = 1.1
eur_to_chf = 0.94
eur_to_cny = 7.74

# %% ######################## calculate ###########################
#TODO write code to loop over principles and calculate the
# costs per passenger (pp)
cost_pp = dict()

# proportional
cost_pp['proportional'] = round(stringency_costs['costs']/nr_passengers)

# prioritarian
cost_pp['tourism'] = round(stringency_costs['costs']/
                            (nr_passengers*purpose['tourism']))
#TODO fix below, does not add to the dict for some reason but 
# returns an empty dict for some reason
cost_pp['non_tourism'] = stringency_costs['costs']*0

# limitarian   
cost_pp['frequent'] = round(stringency_costs['costs']/
                            (nr_passengers*frequency['frequent']))
#TODO fix below, does not add to the dict for some reason but 
# returns an empty dict for some reason
cost_pp['non_frequent'] = stringency_costs['costs']*0

# egalitarian
#TODO rewrite egal
# # Define the variable
# x = symbols('x')

# # Define the equation
# equation = Eq(25*x + 43*x*1.7 + 186*x*7, 1)

# # Solve the equation
# solution = solve(equation, x)
# solution

######################### convert ###########################
#TODO convert to usd, chf, and cny


####################### write to file ########################
#TODO write three files, one for each currency
# %%
