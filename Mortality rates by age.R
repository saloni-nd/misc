# Open libraries
library(tidyverse)
library(scales)
library(viridis)

# Data source:
# https://www.mortality.org/Country/Country?cntr=GBR_NP
# Period data > Death Rates > 1x10
data_folder <- # Replace with path to file

# Import and rename cols
mortality <- read_fwf(paste0(data_folder, "Mx_1x10.txt"), skip=3)
colnames(mortality) <- c("Year", "Age", "Female", "Male", "Total")

# Reformat
#mortality$Year <- as.integer(mortality$Year)
mortality$Age <- as.integer(mortality$Age)
mortality$Female <- as.numeric(mortality$Female)
mortality$Male <- as.numeric(mortality$Male)
mortality$Total <- as.numeric(mortality$Total)

# Wrangle
mortality_g <- gather(mortality, "Demographic", "Rate", 3:5)
mortality_g_total <- filter(mortality_g, Demographic == "Total")

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=10))

# Plot
ggplot(data=mortality_g_total, aes(color=Year, x=Age, y=Rate)) +
  #geom_smooth(se=F,aes(fill=Year),alpha=0.25) +
  geom_point(aes(fill=Year),size=0.2) +
  coord_cartesian(xlim=c(0,110), ylim=c(0,1)) +
  theme_classic() +
  #geom_vline(xintercept=90, linetype="dashed", color="grey") +
  #geom_hline(yintercept=0.22, linetype="dashed", color="grey") +
  #geom_hline(yintercept=0.35, linetype="dashed", color="grey") +
  scale_x_continuous(breaks = seq(0, 100, by=10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_colour_manual(values=cc) +
  labs(title = "Annual death rate in the United Kingdom by age", 
       y = "Death rate", 
       x = "Age",
       caption = "Period death rates.\nSource: Human Mortality Database. Max Planck Institute for Demographic Research (Germany),\nUniversity of California, Berkeley (USA), and French Institute for Demographic Studies (France).\n Available at www.mortality.org (data downloaded on [9 Apr 2023]).") 




