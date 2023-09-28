import pandas as pd
import numpy as np

# Data source:
data_folder = "/Github/misc/"

countries = ["Italy"]
sexes = ["Females", "Males"]
lifetable_list = {}

# Import data
for sex in sexes:
    for country in countries:
        key = f"{country}_{sex}"
        data = pd.read_csv(f"{data_folder}lifetable_1x1_{country}_{sex}.txt", skiprows=2, na_values=".", sep="\t", header=None)
        data.columns = ["Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex"]
        data['Country'] = country
        data['Sex'] = sex
        data['Type'] = "Period"
        lifetable_list[key] = data

# Combine all data frames in the list into one data frame
lifetable = pd.concat(lifetable_list.values())
lifetable['Sex'] = lifetable['Sex'].astype('category')
print(lifetable.describe())

def AKm02a0(m0, sex="m"):
    if sex == "m":
        return np.where(m0 < .0230, 0.14929 - 1.99545 * m0,
                        np.where(m0 < 0.08307, 0.02832 + 3.26201 * m0, .29915))
    else:
        return np.where(m0 < 0.01724, 0.14903 - 2.05527 * m0,
                        np.where(m0 < 0.06891, 0.04667 + 3.88089 * m0, 0.31411))

def LifeExpectancy(mx, sex="f"):
    i_openage = len(mx)
    OPENAGE = i_openage - 1
    RADIX = 1
    ax = mx * 0 + .5
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if np.isnan(qx[i_openage-1]) else qx[i_openage-1]
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.concatenate(([RADIX], RADIX * np.cumprod(px[:OPENAGE])))
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.concatenate((np.flip(np.cumsum(np.flip(Lx[:OPENAGE]))), [0])) + Lx[i_openage-1]
    ex = Tx / lx
    return ex[0]
