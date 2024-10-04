import numpy as np
import pandas as pd
from sympy import symbols, Eq, solve

#################### define parameters #######################
# treatment levels 
# emissions reductions levels)
stringency = [[0.15, 0.3, 0.45]]
justice = [['egalitarian', 'limitarian', 'prioritarian', 'proportional']]

# income and purpose distribution
#TODO consider making this country-specific
income = {'low': [0.1], 'mid': [0.17], 'high': [0.73]}
purpose = {'tourism': [0.5], 'other': [0.5]}
income = pd.DataFrame(income)
purpose = pd.DataFrame(purpose)

# conversion rates as of 04.10.24
eur_to_usd = 1.1
eur_to_chf = 0.94
eur_to_cny = 7.74

######################## calculate ###########################
#TODO write code to loop over principles and calculate the
# costs

# Define the variable
x = symbols('x')

# Define the equation
equation = Eq(25*x + 43*x*1.7 + 186*x*7, 1)

# Solve the equation
solution = solve(equation, x)
solution

######################### convert ###########################
#TODO convert to usd, chf, and cny


####################### write to file ########################
#TODO write three files, one for each currency