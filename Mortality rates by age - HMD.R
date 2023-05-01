# Open libraries
library(tidyverse)
library(scales)
library(viridis)

# Data source:
# https://www.mortality.org/
# Choose countries, then go to Period data > Death Rates > 1x10
# Download and replace this with path to folder
data_folder <- ""

countries <- c("UK", "Italy", "Taiwan")
mortality <- list()

# Import data
for (country in countries) {
  
  # Import and rename cols
  mortality[[country]] <- read_fwf(paste0(data_folder, "Mx_1x10_", country, ".txt"), skip=3)
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

# Gather to make it long format and get only values for total
mortality_g <- gather(mortality, "Demographic", "Rate", 3:5)
mortality_g_total <- filter(mortality_g, Demographic == "Total")

# Get number of time periods shown for colour scale
n_colours <- nrow(count(mortality_g_total, Year))

# Set colour scale - n colours from red to blue
cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=n_colours))

# Plot
ggplot(data=mortality_g_total, aes(color=Year, x=Age, y=Rate)) +
  # Remove hash to show points
  geom_smooth(se=F,aes(fill=Year),alpha=0.25) +
  #geom_point(aes(fill=Year),size=0.2) +
  # Limit to 95 because ages above 100 are noisy and go above 100%
  coord_cartesian(xlim=c(0,95)) +
  facet_grid(cols=vars(Country)) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  # Replace with values for any lines you want to add to the chart
  #geom_vline(xintercept=90, linetype="dashed", color="grey") +
  #geom_hline(yintercept=0.22, linetype="dashed", color="grey") +
  #geom_hline(yintercept=0.35, linetype="dashed", color="grey") +
  scale_x_continuous(breaks = seq(0, 100, by=10)) +
  scale_y_continuous(labels = scales::percent, trans='log2', breaks = c(0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_colour_manual(values=cc) +
  labs(title = "Annual death rate by age", 
       y = "Death rate", 
       x = "Age",
       caption = "Period death rates.\nSource: Human Mortality Database. Max Planck Institute for Demographic Research (Germany),\nUniversity of California, Berkeley (USA), and French Institute for Demographic Studies (France).\nAvailable at www.mortality.org (data downloaded on [1 May 2023])\nChart by Saloni Dattani.") 
