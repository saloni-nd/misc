library(tidyverse)
library(scales)
library(viridis)

# Data source: CDC Wonder database https://wonder.cdc.gov/natality.html
# Natality for 2007 - 2022 (expanded)
# Group by: Month, Year
# Select delivery characteristics: All years

# Import data
file_path <- "" # Replace with path to file

births_raw <- read_tsv(paste0(file_path, "Natality-2007-2022-expanded.txt"), col_names = TRUE)

# Remove rows where Notes equals "Total" using filter from the dplyr package
births_cleaned <- births_raw %>% 
  filter(is.na(Notes))

# Make month a factor
births_cleaned <- births_cleaned %>%
  mutate(Month = factor(Month, levels = unique(Month[order(-`Month Code`)])))

# Create heatmap
ggplot(births_cleaned, aes(x = Year, y = Month, fill = Births)) +
  geom_tile() + # Use geom_tile for the heatmap squares
  scale_fill_viridis_c(option = "magma", direction = 1, name = "Births", labels = comma) + # Explicitly use viridis color scale
  theme_minimal() + # Use a minimal theme
  labs(title = "Births are more common in the summer and autumn",
       subtitle = "Number of births in the United States by month and year",
       caption = "Data from the CDC Wonder database\nChart by Saloni Dattani",
       x = "",
       y = "",
       fill = "Number of births") +
  theme(plot.title = element_text(size = 20)) +
  scale_x_continuous(breaks = unique(births_cleaned$Year)) + # Ensure all years are labeled
  theme(axis.text.x = element_text(angle = 0)) # Improve x-axis labels readability
