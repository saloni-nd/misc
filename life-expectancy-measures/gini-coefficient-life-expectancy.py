import pandas as pd
import numpy as np

# Data source:
data_folder = "/Users/saloni/Documents/Github/misc/"

countries = ["Italy"]
sexes = ["Females", "Males"]
lifetable_list = {}

# Import data
for sex in sexes:
    for country in countries:
        key = f"{country}_{sex}"
        data = pd.read_csv(f"{data_folder}lifetable_1x1_{country}_{sex}.txt", skiprows=2, na_values=".", delim_whitespace=True, header=None)
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
        return np.where(m0 < 0.0230, 0.14929 - 1.99545 * m0,
                np.where(m0 < 0.08307, 0.02832 + 3.26201 * m0, 0.29915))
    else:
        return np.where(m0 < 0.01724, 0.14903 - 2.05527 * m0,
                np.where(m0 < 0.06891, 0.04667 + 3.88089 * m0, 0.31411))

def LifeExpectancy(mx, sex="f"):
    i_openage = len(mx)
    OPENAGE = i_openage - 1
    RADIX = 1
    ax = np.full_like(mx, 0.5)
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if not np.isnan(qx[i_openage-1]) else np.nan
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.concatenate(([RADIX], RADIX * np.cumprod(px[:OPENAGE])))
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.concatenate((np.cumsum(Lx[:OPENAGE][::-1])[::-1], [0])) + Lx[i_openage-1]
    ex = Tx / lx
    return ex[0]


def LifeTable(mx, sex="f"):
    mx = np.array(mx).reshape(-1, 1)
    i_openage = mx.shape[0]
    ax = np.full_like(mx, 0.5)
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if not np.isnan(qx[i_openage-1]) else np.nan
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.apply_along_axis(lambda px_: np.concatenate(([1], np.cumprod(px_[:i_openage-1]))), 0, px)
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.apply_along_axis(lambda Lx_: np.concatenate((np.cumsum(Lx_[:i_openage-1][::-1])[::-1], [0])) + Lx_[i_openage-1], 0, Lx)
    ex = Tx / lx
    return {'e0': ex[0], 'ex': ex, 'lx': lx, 'mx': mx}

def h_frommx(mx, sex):
    print(f"mx type: {type(mx[0])}")  
    i_openage = len(mx)
    OPENAGE = i_openage - 1
    RADIX = 1
    ax = np.full_like(mx, 0.5)
    print(f"ax before AKm02a0: {ax[0]}, type: {type(ax[0])}")
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    print(f"ax after AKm02a0: {ax[0]}, type: {type(ax[0])}")
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if not np.isnan(qx[i_openage-1]) else np.nan
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.concatenate(([RADIX], RADIX * np.cumprod(px[:OPENAGE])))
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.concatenate((np.cumsum(Lx[:OPENAGE][::-1])[::-1], [0])) + Lx[i_openage-1]
    ex = Tx / lx
    l = len(ex)
    v = (np.sum(dx[:-1] * (ex[:-1] + ax[:-1] * (ex[-1] - ex[:-1]))) + ex[-1])
    k = v / ex[0]
    eq = -np.log(k)
    return eq

def my_cv_from_mx(mx, sex):
    i_openage = len(mx)
    OPENAGE = i_openage - 1
    RADIX = 1
    ax = np.full_like(mx, 0.5)
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if not np.isnan(qx[i_openage-1]) else np.nan
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.concatenate(([RADIX], RADIX * np.cumprod(px[:OPENAGE])))
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.concatenate((np.cumsum(Lx[:OPENAGE][::-1])[::-1], [0])) + Lx[i_openage-1]
    ex = Tx / lx
    age = np.arange(i_openage)
    vx = np.sum(dx * (age + ax - ex[0]) ** 2)
    cv = np.sqrt(vx) / ex[0]
    cv_inv = -np.log(cv)
    return cv_inv

def log_G_frommx(mx, sex="f"):
    print(f"mx type: {type(mx[0])}")
    i_openage = len(mx)
    OPENAGE = i_openage - 1
    RADIX = 1
    ax = np.full_like(mx, 0.5)
    ax[0] = AKm02a0(m0=mx[0], sex=sex)
    qx = mx / (1 + (1 - ax) * mx)
    qx[i_openage-1] = 1 if not np.isnan(qx[i_openage-1]) else np.nan
    ax[i_openage-1] = 1 / mx[i_openage-1]
    px = 1 - qx
    px[np.isnan(px)] = 0
    lx = np.concatenate(([RADIX], RADIX * np.cumprod(px[:OPENAGE])))
    dx = lx * qx
    Lx = lx - (1 - ax) * dx
    Lx[i_openage-1] = lx[i_openage-1] * ax[i_openage-1]
    Tx = np.concatenate((np.cumsum(Lx[:OPENAGE][::-1])[::-1], [0])) + Lx[i_openage-1]
    ex = Tx / lx
    age = np.arange(i_openage) + ax
    e = np.ones_like(age)
    D = np.outer(dx, dx)
    X_ = np.abs(np.outer(e, age) - np.outer(age, e))
    G = np.sum(D * X_) / (2 * ex[0])
    return G

def compute_metrics(group):
    sex_val = group.name[1]  # Accessing the 'Sex' value from the group's name
    h_val = h_frommx(mx=group['mx'].values, sex=sex_val)
    v_val = my_cv_frommx(mx=group['mx'].values, sex=sex_val)
    G_val = log_G_frommx(mx=group['mx'].values, sex=sex_val)
    eo_val = group['ex'].iloc[0]
    return pd.Series({'h': h_val, 'v': v_val, 'G': G_val, 'eo': eo_val})

lifetable['mx'] = pd.to_numeric(lifetable['mx'], errors='coerce')

results = lifetable.groupby(['Country', 'Sex', 'Year']).apply(compute_metrics).reset_index()



grouped = lifetable.groupby(['Country', 'Sex', 'Year'])
for name, group in grouped:
    if name[1] == "Males":  # Only print male groups for brevity
        print(name)
        print(group)
        break  # Print only one group for inspection

def compute_metrics(group):
    sex_val = group.name[1]  # Accessing the 'Sex' value from the group's name
    print(f"Processing group: {group.name}")
    
    h_val = h_frommx(mx=group['mx'].values, sex=sex_val)
    print(f"h_val: {h_val}")
    
    v_val = my_cv_frommx(mx=group['mx'].values, sex=sex_val)
    print(f"v_val: {v_val}")
    
    G_val = log_G_frommx(mx=group['mx'].values, sex=sex_val)
    print(f"G_val: {G_val}")
    
    eo_val = group['ex'].iloc[0]
    print(f"eo_val: {eo_val}")
    
    return pd.Series({'h': h_val, 'v': v_val, 'G': G_val, 'eo': eo_val})

results = lifetable.groupby(['Country', 'Sex', 'Year']).apply(compute_metrics).reset_index()

results = results.drop_duplicates()


