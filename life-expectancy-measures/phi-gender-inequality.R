# Open libraries
library(tidyverse)
library(data.table)
library(magrittr)
library(readxl)
library(countrycode)

# Data source:
# https://www.mortality.org/
# Choose countries, then go to Cohort data > Death Rates > 1x10
# Download and replace this with path to folder
data_folder <- "/Github/misc/"
# set filename to cMx_1x10_(name of country).txt

countries <- c("Italy")
sexes <- c("Females", "Males")
lifetable_list <- list()  # Renaming to lifetable_list for clarity

# Import data
for (sex in sexes) {
  for (country in countries) {
    
    # Create a unique key for country and sex
    key <- paste(country, sex, sep = "_")
    
    # Import and rename cols
    data <- read_table(paste0(data_folder, "lifetable_1x1_", country, "_", sex, ".txt"), skip=2, na = ".")
    colnames(data) <- c("Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
    
    data <- data %>%
      mutate(Country = country, Sex = sex, Type = "Period")
    
    # Assign to the list using the unique key
    lifetable_list[[key]] <- data
  }
}

# Combine all data frames in the list into one data frame
lifetable <- do.call(rbind, lifetable_list)
lifetable$Sex <- as.factor(lifetable$Sex)
lifetable$Age <- as.numeric(lifetable$Age)
summary(lifetable)


# a function to calculate standard deviations
# see suppl to 10.1126/science.aau5811
sd_dx <- function(Age, dx, lx, ex, ax){
    { dx / lx[1] * (Age + ax - ex[1])^2 } %>% 
  rev %>% 
  cumsum %>% 
  rev %>% 
  sqrt
}

# Calculate standard deviations
lifetable <- lifetable %>%
mutate(lx = lx / 1e5, 
        dx = dx / 1e5) %>% 
    group_by(Age, Year, Sex) %>% 
    mutate(sd = sd_dx(Age, dx, lx, ex, ax)) 

# calculate phi - need to pivot so that theres a separate column for each sex, e.g. with _Females or _Males for males or females
df_pivoted <- lifetable %>%
  pivot_wider(
    id_cols = c(Year, Age, Country, Type),
    names_from = Sex,
    values_from = c(mx, qx, ax, lx, dx, Lx, Tx, ex, sd)
  )

phi_table <- df_pivoted %>%
    group_by(Country, Year) %>% 
    summarise(
        phi = sum(head(dx_Females, -1) * tail(lx_Males, -1), na.rm = T) + 
            sum(dx_Females * dx_Males, na.rm = T) / 2,
        e0_Females = ex_Females %>% first,
        e0_Males = ex_Males %>% first,
        sd_Females = sd_Females %>% first,
        sd_Males = sd_Males %>% first
    ) %>% 
    ungroup() %>%
    dplyr::select(c(Country, Year, phi))

write_csv(phi_table, paste0(data_folder, "phi_Routput.csv"))
           
