import pandas as pd
import numpy as np

# Data source:
# https://www.mortality.org/
# Choose countries, then get lifetable data 1x1
# Download and replace this with path to folder
data_folder = "/Github/misc/"

countries = ["Italy"]
sexes = ["Females", "Males"]
lifetable_dict = {}  # Using a dictionary instead of a list

# Import data
for sex in sexes:
    for country in countries:
        # Create a unique key for country and sex
        key = f"{country}_{sex}"
        
        # Import and rename cols
        data = pd.read_csv(f"{data_folder}lifetable_1x1_{country}_{sex}.txt", skiprows=2, na_values=".", delim_whitespace=True)
        data.columns = ["Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex"]
        data['Country'] = country
        data['Sex'] = sex
        data['Type'] = "Period"
        
        # Assign to the dictionary using the unique key
        lifetable_dict[key] = data

# Combine all data frames in the dictionary into one data frame
lifetable = pd.concat(lifetable_dict.values())
lifetable['Sex'] = lifetable['Sex'].astype('category')
lifetable['Age'] = pd.to_numeric(lifetable['Age'], errors='coerce') # This removes the open-ended age groups like 110+, but Im not sure if this is the correct thing to do

print(lifetable.describe())

# Function to calculate standard deviations
def sd_dx(group):
    Age = group['Age']
    dx = group['dx']
    lx = group['lx']
    ex = group['ex']
    ax = group['ax']
    return np.sqrt(np.cumsum((dx / lx.iloc[0] * (Age + ax - ex.iloc[0]) ** 2)[::-1])[::-1])

# Calculate standard deviations
lifetable['lx'] = lifetable['lx'] / 1e5
lifetable['dx'] = lifetable['dx'] / 1e5

# Apply the function and reset the index
sd_values = lifetable.groupby(['Age', 'Year', 'Sex']).apply(sd_dx).reset_index(name='sd')

# Merge the calculated sd values back into the original DataFrame
lifetable = lifetable.merge(sd_values, on=['Age', 'Year', 'Sex'])


# Pivot the table
df_pivoted = lifetable.pivot_table(index=['Year', 'Age', 'Country', 'Type'], columns='Sex', values=["mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex", "sd"]).reset_index()

# Calculate phi
def calculate_phi(group):
    dx_females = group['dx']['Females'].values
    lx_males = group['lx']['Males'].values
    dx_males = group['dx']['Males'].values
    lx_females = group['lx']['Females'].values
    
    # Use numpy's nansum to sum while ignoring NaN values
    part1 = np.nansum(dx_females[:-1] * lx_males[1:])
    part2 = np.nansum(group['dx']['Females'] * group['dx']['Males'].values) / 2
    
    return part1 + part2


phi_table = df_pivoted.groupby(['Country', 'Year']).apply(lambda group: pd.Series({
    'phi': calculate_phi(group),
    'e0_Females': group['ex']['Females'].iloc[0],
    'e0_Males': group['ex']['Males'].iloc[0],
    'sd_Females': group['sd']['Females'].iloc[0],
    'sd_Males': group['sd']['Males'].iloc[0]
})).reset_index()

phi_table = phi_table[['Country', 'Year', 'phi']]
phi_table.to_csv(f"{data_folder}phi_Pythonoutput.csv", index=False)
