# Open libraries
library(readxl)
library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)

print(sessionInfo())

# Data source: https://cy.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales/2013

# Import xls spreadsheet
file_path <- ""

infant_m <- read_excel(path = paste0(file_path, "cmstables2013correction.xls"), sheet = "Table 17", skip=19, col_names=F)

# Rename cols
colnames(infant_m) <- c("Year",
                        "Under.1.year",
                        "Under.4.weeks",
                        "Under.1.week",
                        "Under.1.day",
                        "One.day.and.under.1.week",
                        "One.week.and.under.4.weeks",
                        "Four.weeks.and.under.1.year",
                        "Four.weeks.and.under.3.months",
                        "Three.months.and.under.6.months",
                        "Six.months.and.under.1.year",
                        "Stillbirths",
                        "Stillbirths.plus.deaths.under.1.week",
                        "Stillbirths.plus.deaths.under.4.weeks",
                        "Stillbirths.plus.deaths.under.1.year")

# Reformat numeric               
infant_m <- data.frame(lapply(infant_m, as.numeric))

# Create variables for daily mortality -- divide by number of days in the period                 
infant_n <- infant_m %>%
            mutate("Day 1" = Under.1.day) %>%
            mutate("Day 7" = One.day.and.under.1.week / 6) %>%
            mutate("Day 28" = One.week.and.under.4.weeks / 21) %>%
            mutate("Day 91" = Four.weeks.and.under.3.months / 63) %>%
            mutate("Day 182" = Three.months.and.under.6.months / 91) %>%
            mutate("Day 365" = Six.months.and.under.1.year / 182)

# Gather into long format
infant_g <- infant_n %>%
              dplyr::select(Year, "Day 1":"Day 365") %>%
              gather("Age", "Mortality", 2:7)
            
# Replace with numerics for plotting       
infant_g[infant_g == "Day 1"] <- 1
infant_g[infant_g == "Day 7"] <- 7
infant_g[infant_g == "Day 28"] <- 28
infant_g[infant_g == "Day 91"] <- 91
infant_g[infant_g == "Day 182"] <- 182
infant_g[infant_g == "Day 365"] <- 365

infant_g$Age <- as.numeric(infant_g$Age)

# Select years to show
infant_g <- infant_g %>%
            filter(Year %in% c(1921, 1931, 1941, 1951, 1961, 1971, 1981, 1991, 2001, 2011))

# Change to factor so the legend shows them separately
infant_g$Year <- as.factor(infant_g$Year)

# Get number of time periods shown for colour scale
n_colours <- nrow(count(infant_g, Year))

# Set colour scale - n colours from red to blue
cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=n_colours))

# log-log plot of daily infant mortality by age
ggplot(data=infant_g, aes(x=Age, y=Mortality, color=Year)) +
      geom_line(aes(group=Year)) +
      #geom_smooth(se=F, aes(group=Year)) +
      geom_point(aes(fill=Year), size=1) +
      scale_y_continuous(trans='log10', breaks = c(0,1,2,5,10,20,50,100,200,500)) +
      scale_x_continuous(trans='log10') +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_manual(values=cc) +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Infant mortality rates decline after birth, with the shape of a power law", 
       subtitle = "The chances of dying are highest during the first few days of an infant's life.\nOver the following days, weeks and months, their chances of dying decrease sharply. \nThe straight line on the log-log plot indicates a steady, predictable decline as they age.\nOver time, the mortality rate has declined across the entire first year of an infant's life.", 
       y = "Daily mortality rate (per 100,000)", 
       x = "Age (days)",
       caption = "Source: Office for National Statistics, UK") 

