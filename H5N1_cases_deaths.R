# Open libraries
library(tidyverse)
library(openxlsx)

# Data

path <- ""

# Source: WHO via CDC - https://www.cdc.gov/flu/avianflu/chart-epi-curve-ah5n1.html
# Import dataset
cases <- read_csv(paste0(path, "H5N1_cases.csv"))

# Long format
cases_long <- gather(cases, "Country", "Cases", 3:26)

# Remove rows with 0 cases to shorten dataset
cases_long$Cases <- as.integer(cases_long$Cases)
cases_long <- cases_long %>%
                filter(Cases != 0)

# Source: WHO + Eurosurveillance via Adam Kucharski - https://github.com/adamkucharski/subcritical_chains
# Import dataset
cases_deaths <- read.xlsx(xlsxFile = paste0(path, "H5N1_list_2006_2014.xlsx"), 
                          sheet = 1, detectDates=TRUE)

# Variable for number of cases by date
cases1 <- cases_deaths %>%
  group_by(Country, Date_reported) %>%
  summarise(N_cases = n())

# Variable for number of deaths by date
deaths1 <- cases_deaths %>%
            filter(Outcome == "Fatal") %>%
            group_by(Country, Date_reported) %>%
            summarise(N_deaths = n())
