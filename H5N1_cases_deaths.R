# Open libraries
library(tidyverse)
library(openxlsx)

# Data
path <- ""

######

# Source: WHO via CDC - https://www.cdc.gov/flu/avianflu/chart-epi-curve-ah5n1.html
# Import dataset
cases <- read_csv(paste0(path, "H5N1_cases.csv"))

# Long format
cases_long <- gather(cases, "Country", "Cases", 3:26)

# Remove rows with 0 cases to shorten dataset
cases_long$Cases <- as.integer(cases_long$Cases)
cases_long <- cases_long %>%
  filter(Cases != 0) %>%
  dplyr::select(-Range)

# Create World aggregate
cases_world <- cases_long %>%
                group_by(Month) %>%
                summarise(Cases = n()) %>%
                mutate(Country = "World")

cases_long <- bind_rows(cases_long, cases_world)

########

# Source: WHO + Eurosurveillance via Adam Kucharski - https://github.com/adamkucharski/subcritical_chains
# Import dataset
cases_deaths <- read.xlsx(xlsxFile = paste0(path, "H5N1_list_2006_2014.xlsx"), 
                          sheet = 1, detectDates=TRUE)

# Variable for number of cases by date
cases1 <- cases_deaths %>%
  group_by(Country, Date_reported) %>%
  summarise(N_cases = n())

# Create World aggregate
cases_world <- cases_deaths %>%
                group_by(Date_reported) %>%
                summarise(N_cases = n()) %>%
                mutate(Country = "World")

cases1_long <- bind_rows(cases1, cases_world)

# Variable for number of deaths by date
deaths1 <- cases_deaths %>%
  filter(Outcome == "Fatal") %>%
  group_by(Country, Date_reported) %>%
  summarise(N_deaths = n())

# Create World aggregate
deaths_world <- cases_deaths %>%
  filter(Outcome == "Fatal") %>%
  group_by(Date_reported) %>%
  summarise(N_deaths = n()) %>%
  mutate(Country = "World")

deaths1_long <- bind_rows(deaths1, deaths_world)
