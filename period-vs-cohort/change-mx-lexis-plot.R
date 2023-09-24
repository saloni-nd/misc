library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)

# Data source: Human Mortality Database, on mortality.org
# Download and save to folder below, as `Mx_1x1_[country].txt`

# Code source: Methods for disentangling period and cohort changes in mortality risk over the twentieth century: comparing graphical and modelling approaches (2022). Phil Mike Jones, Jon Minton & Andrew Bell.
# https://link.springer.com/article/10.1007/s11135-022-01498-3
# Code: https://zenodo.org/record/6866402

file_path <- ""

countries <- c("FRATNP")
mortality <- list()

# Import data
for (country in countries) {
  
  # Import and rename cols
  mortality[[country]] <- read_table(paste0(file_path, "Mx_1x1_", country, ".txt"), skip=2)
  colnames(mortality[[country]]) <- c("Year", "Age", "Female", "Male", "Total")
  
  mortality[[country]] <- mortality[[country]] %>%
    mutate(Country = country)
  
}


# Join into single df
mortality <- do.call(rbind.data.frame, mortality)

# Reformat
mortality$Age <- as.integer(mortality$Age)
mortality$Female <- as.numeric(mortality$Female)
mortality$Male <- as.numeric(mortality$Male)
mortality$Total <- as.numeric(mortality$Total)
mortality$Country <- as.factor(mortality$Country)

# Gather
mortality <- mortality %>%
              gather(key = Sex, value = Mx, Female:Total)

# Create vars for log10 mortality rate and ln mortality rate
mortality <- mortality %>%
  mutate(
    birth_year = Year - Age
  ) %>%
  mutate(
    log10_cmr  = log(Mx, base = 10),
    ln_mr      = log(Mx, base = exp(1))
  )

mortality <- mortality %>%
  arrange(Country, Age, Sex) %>%
  group_by(Country, Sex) %>%
  mutate(ch_mr = Mx - lag(Mx)) %>%  # must be done separately by group
  ungroup()

mortality <- mortality %>%
  group_by(Sex, Age) %>%
  mutate(change_log10_cmr = log10_cmr - lag(log10_cmr)) %>%
  ungroup()


