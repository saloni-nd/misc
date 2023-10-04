# Libraries
library(ggplot2)
library(readr)
library(stringr)
library(dplyr)

# Filepath
file_path <- "/Github/misc/Infant_mortality/"

# 2020 US CDC Wonder data > Group by > Gender, Age at death in days
infant_2020 <- read_delim(paste0(file_path, "Linked Birth  Infant Death Records, 2017-2021 Expanded-by-gender.txt"), delim = "\t")

# Rename columns
colnames(infant_2020) <- c("Notes", "Gender", "Gender_code", "Age_days", "Age_code", "Deaths", "Births", "Death_rate")

# Remove string that says unreliable from Death_rate column
infant_2020$Death_rate <- str_replace(infant_2020$Death_rate, " \\(Unreliable\\)", "")

# Convert Death_rate to numeric
infant_2020$Death_rate <- as.numeric(infant_2020$Death_rate)

# PLOT 1: Daily mortality rates across the first year, per 1,000 births
ggplot(infant_2020, aes(x = Age_code, y = Death_rate, color=Gender)) +
  geom_smooth(size = 1) +
  scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10)) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  coord_cartesian(xlim=c(0,360)) +
  labs(title = "Infant mortality rates decline sharply after birth",
       subtitle = "The chances of dying are highest during the first few days of an infant's life.\nOver the following days, weeks and months, their chances of dying decrease sharply.",
       x = "Age (days)",
       y = "Mortality rate (per 1,000)",
       caption = "Source: US Centers for Disease Control and Prevention. Data from 2017-2021.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(file_path, "daily_infant_mortality_US_gender.png"))

# Cumulative deaths calculation
infant_2020 <- infant_2020 %>%
  group_by(Gender) %>%
  mutate(Cumulative_deaths = cumsum(Deaths))

# PLOT 2: Cumulative share of infants who have died by a given age
ggplot(infant_2020, aes(x = Age_code, y = Cumulative_deaths / Births, color=Gender)) +
  geom_line(size = 1.5) +
  coord_cartesian(xlim=c(0,360),ylim=c(0,0.006)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     breaks = seq(0, 0.006, 0.001)) +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  labs(title = "Share of infants who have died over the first year",
       caption = "The cumulative share of infants who have died by a given age.\nBased on US infant mortality rates between 2017-2021, using death certificates.\nSource: US Centers for Disease Control and Prevention.",
       x = "Age (days)",
       y = "") +
  theme_minimal()

ggsave(paste0(file_path, "cumulative_infant_mortality_US_gender.svg"))
