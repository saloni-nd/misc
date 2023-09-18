import pandas as pd
import numpy as np
from scipy.interpolate import UnivariateSpline
from scipy.integrate import cumtrapz
import os
import seaborn as sns
import matplotlib.pyplot as plt

path = "" # replace with path to data here

# Function to interpolate death counts and exposures using splines
def ageInterpolationSpline(Dx, Nx, Age, startAge=0, endAge=110, sDx=None, sNx=None):
    df = pd.DataFrame({'Dx': Dx, 'Nx': Nx, 'Age': Age})
    
    # Use user-defined s or let the function determine it automatically
    splineDx = UnivariateSpline(df['Age'], df['Dx'], s=sDx)
    splineNx = UnivariateSpline(df['Age'], df['Nx'], s=sNx)
    
    Age_range = np.arange(startAge, endAge, 0.01)
    
    sDx = np.abs(splineDx(Age_range))
    sNx = np.abs(splineNx(Age_range))
    sMx = np.abs(sDx / sNx)
    
    out = pd.DataFrame({'Age': Age_range, 'Dx': sDx, 'Nx': sNx, 'Mx': sMx})
    return out


# Function to calculate survival, density, hazard, and cumulative hazards
def calculateSurvival(Age, hx):
    Hx = cumtrapz(hx, Age, initial=0)
    Sx = np.exp(-Hx)
    Fx = 1 - Sx
    fx = hx * Sx
    return pd.DataFrame({'Age': Age, 'hx': hx, 'Hx': Hx, 'Sx': Sx, 'Fx': Fx, 'fx': fx})

# Function to calculate survivorship ages
def calculateSurvivalAges(Age, fx, Sx, hx):
    df = pd.DataFrame({'Age': Age, 'fx': fx, 'Sx': Sx * 100, 'hx': hx})
    
    out = {}
    out['s0'] = max(df['Age'])
    
    # I'm using a for loop to simplify the logic here
    for i in range(1, 101):
        try:
            value = df['Age'][np.ceil(df['Sx'].round(3)) == i].iloc[0]
            out[f's{i}'] = value
        except IndexError:
            out[f's{i}'] = np.nan
    
    return pd.DataFrame(out, index=[0])


def getHMDdata(name, path):
    p = os.path.join(path, name)
    dat_raw = pd.read_csv(p, skiprows=2, na_values=".")
    
    # Splitting the name for Country and Type
    name_split = name.split('.')
    country = name_split[0]
    type_x = name_split[1]
    type_ = type_x.split('_')[0]
    
    dat = HMDparse(dat_raw, p)
    dat['Country'] = country
    dat['Type'] = type_
    
    return dat


def getHMDdata(name, path):
    filepath = f"{path}/{name}"
    dat_raw = pd.read_csv(filepath, delim_whitespace=True, skiprows=2, na_values=".")
    dat = HMDparse(dat_raw, filepath)
    country, type_x = name.split('.')[0], name.split('.')[1]
    type_ = type_x.split('_')[0]
    out = pd.DataFrame({
        'Country': [country] * len(dat),
        'Type': [type_] * len(dat)
    })
    out = pd.concat([out, dat], axis=1)
    return out


def HMDparse(DF, filepath):
    def age2int(age): return int(''.join(filter(str.isdigit, str(age)))) if isinstance(age, str) else int(age)
    if 'Age' in DF.columns: DF['Age'] = DF['Age'].apply(age2int)
    if 'pop' in filepath.lower(): all_years = sorted(DF['Year'].unique()); pluses = DF['Year'].str.contains('+'); minuses = DF['Year'].str.contains('-'); jan1 = DF[DF['Year'].isin(all_years[:-1]) | pluses]; dec31 = DF[DF['Year'].isin(all_years[1:]) | minuses]; jan1['Year'] = jan1['Year'].apply(age2int); dec31['Year'] = dec31['Year'].apply(age2int); cols1 = [col for col in jan1.columns if col.lower() in ['female', 'male', 'total']]; cols2 = [col for col in dec31.columns if col.lower() in ['female', 'male', 'total']]; jan1 = jan1.rename(columns={col: col + '1' for col in cols1}); dec31 = dec31.rename(columns={col: col + '2' for col in cols2}); DF = pd.merge(jan1, dec31[cols2], left_index=True, right_index=True); orgi = any([True for col in DF.columns if 'male' in col.lower() or 'total' in col.lower()]); DF = pd.concat([DF[~orgi], DF[orgi]], axis=1)
    if 'Year' in DF.columns: DF['Year'] = DF['Year'].apply(age2int)
    if 'Cohort' in DF.columns: DF['Cohort'] = DF['Cohort'].apply(age2int)
    return DF


# Assuming the functions `getHMDdata`, `ageInterpolationSpline`, `calculateSurvival`, and `calculateSurvivalAges` 
# are already translated to Python and available to use.

filenames = os.listdir(path)

# Filter filenames to only .txt files
txt_files = [name for name in filenames if name.endswith('.txt')]

# Get data for each file and store in a list
HMDlist = [getHMDdata(name, path) for name in txt_files]

# Merge dataframes in the list to a single dataframe
HMDdata = pd.concat(HMDlist, ignore_index=True)


Dx = HMDdata[HMDdata['Type'] == "Deaths"]
Nx = HMDdata[HMDdata['Type'] == "Exposures"]
Dx = Dx[["Country", "Year", "Age", "Female", "Male", "Total"]]
Nx = Nx[["Country", "Year", "Age", "Female", "Male", "Total"]]
Dx.columns = ["Country", "Year", "Age", "Dx.f", "Dx.m", "Dx.t"]
Nx.columns = ["Country", "Year", "Age", "Nx.f", "Nx.m", "Nx.t"]

Mx = pd.merge(Dx, Nx, on=["Country", "Year", "Age"])
Mx = Mx.applymap(str)
Mx['Age'] = Mx['Age'].replace("110+", "110")
Mx.iloc[:, 1:9] = Mx.iloc[:, 1:9].apply(pd.to_numeric)
Mx = Mx.sort_values(by=["Year", "Age"])

Mx['Nx.f_nozero'] = Mx['Nx.f'].replace(0, np.nan)
Mx['Mx.f'] = Mx['Dx.f'] / Mx['Nx.f_nozero']
del Mx['Nx.f_nozero']  # Delete the temporary column if not needed

Mx['Nx.m_nozero'] = Mx['Nx.m'].replace(0, np.nan)
Mx['Mx.m'] = Mx['Dx.f'] / Mx['Nx.m_nozero']
del Mx['Nx.m_nozero']  # Delete the temporary column if not needed

Mx['Nx.t_nozero'] = Mx['Nx.t'].replace(0, np.nan)
Mx['Mx.t'] = Mx['Dx.t'] / Mx['Nx.t_nozero']
del Mx['Nx.t_nozero']  # Delete the temporary column if not needed

Mx.fillna(0, inplace=True)

# Create a function to reshape based on prefix and suffix
def reshape_data(df, prefix, suffix):
    subset = df.filter(like=f'.{suffix}').copy()  # Make a copy to avoid warnings
    subset.columns = [col.split('.')[0] for col in subset.columns]
    subset.loc[:, 'Sex'] = suffix.upper()
    return pd.concat([df[['Country', 'Year', 'Age']], subset], axis=1)

# Reshape for each sex
females = reshape_data(Mx, 'Mx', 'f')
males = reshape_data(Mx, 'Mx', 'm')
total = reshape_data(Mx, 'Mx', 't')

# Combine the reshaped dataframes
Mx_reshaped = pd.concat([females, males, total])

# Let's assume you want to use sDx=10 and sNx=10 for now as a starting point.
smoothData = Mx_reshaped.groupby(['Country', 'Sex', 'Year']).apply(lambda group: ageInterpolationSpline(group['Dx'], group['Nx'], group['Age'], sDx=30, sNx=30)).reset_index()

# Calculate survivorship
survival = smoothData.groupby(['Country', 'Sex', 'Year']).apply(lambda group: calculateSurvival(group['Age'], group['Mx'])).reset_index()
survival.drop('level_3', axis=1, inplace=True)

# Calculate survivorship ages
survivalAges = survival.groupby(['Country', 'Sex', 'Year']).apply(lambda group: calculateSurvivalAges(group['Age'], group['fx'], group['Sx'], group['hx'])).reset_index()

# Convert the 'Year' column to integer
survivalAges['Year'] = survivalAges['Year'].astype(int)

# Melt the dataframe for easier plotting
melted_data = survivalAges.melt(id_vars=['Country', 'Sex', 'Year'], 
                                value_vars=['s1', 's10', 's20', 's30', 's40', 's50', 's60', 's70', 's80', 's90', 's99'],
                                var_name='s', 
                                value_name='Age')

melted_data.dropna(subset=['s'], inplace=True)

# Create separate line plots for each country
g = sns.FacetGrid(melted_data, col="Country", row="Sex", hue="s", palette="viridis", height=3, aspect=1)

# Map the lineplot onto the FacetGrid
g.map(sns.lineplot, "Year", "Age")

# Adjust the appearance of the FacetGrid
g.set_axis_labels("Year", "Age")
g.set_titles(col_template="{col_name} Country")
g.add_legend()

plt.show()

