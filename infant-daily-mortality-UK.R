# Open libraries
library(readxl)
library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)

print(sessionInfo())

# Data source: https://cy.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales/2013

# Filepath
file_path <- ""

###########
# 2021 data
infant_2021 <- read.xlsx(xlsxFile = paste0(file_path, "cim2021deathcohortworkbook.xlsx"), sheet = 5, startRow=10)

infant_2021 <- data.frame(lapply(infant_2021, as.numeric))

# Create variables for daily mortality -- divide by number of days in the period
infant_2021 <- infant_2021 %>%
  mutate("Day 1" = Early.neonatal.under.1.day.mortality.rate) %>%
  mutate("Day 7" = Early.neonatal.1.day.and.under.1.week.mortality.rate / 6) %>%
  mutate("Day 28" = Late.neonatal.1.week.and.under.4.weeks.mortality.rate / 21) %>%
  mutate("Day 91" = Postneonatal.4.weeks.and.under.3.months.mortality.rate / 63) %>%
  mutate("Day 182" = Postneonatal.3.months.and.under.6.months.mortality.rate / 91) %>%
  mutate("Day 365" = Postneonatal.6.months.and.under.1.year.mortality.rate / 182)

infant_g_2021 <- infant_2021 %>%
  dplyr::select(Year, "Day 1":"Day 365") %>%
  gather("Age", "Mortality", 2:7)


##############3
# 1921 to 2013 data
# Data source: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales
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


# Prefer 2021 data for data past 1980, so delete them here
infant_g <- infant_g %>%
  filter(Year < 1980)


#############
# Join datasets
infant_g_2021 <- bind_rows(infant_g, infant_g_2021)


# Replace with numerics for plotting       
infant_g_2021[infant_g_2021 == "Day 1"] <- 1
infant_g_2021[infant_g_2021 == "Day 7"] <- 7
infant_g_2021[infant_g_2021 == "Day 28"] <- 28
infant_g_2021[infant_g_2021 == "Day 91"] <- 91
infant_g_2021[infant_g_2021 == "Day 182"] <- 182
infant_g_2021[infant_g_2021 == "Day 365"] <- 365

infant_g_2021$Age <- as.numeric(infant_g_2021$Age)

# Select years to show
infant_g_2021 <- infant_g_2021 %>%
            filter(Year %in% c(1921, 1931, 1941, 1951, 1961, 1971, 1981, 1991, 2001, 2011, 2021))

# Change to factor so the legend shows them separately
infant_g_2021$Year <- as.factor(infant_g_2021$Year)

# Get number of time periods shown for colour scale
n_colours <- nrow(count(infant_g_2021, Year))

# Set colour scale - n colours from red to blue
cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=n_colours))

# Interpolate mortality rate for days not given to show trendline
interp_fun_by_year <- function(Year) {
  subset_df <- infant_g_2021[infant_g_2021$Year == Year,]
  log_x <- log(subset_df$Age)
  log_y <- log(subset_df$Mortality)
  log_interp_fun <- stats::approxfun(log_x, log_y)
  return(function(x) exp(log_interp_fun(log(x))))
}

# GGPLOT
# Daily infant mortality by age
ggplot(data=infant_g_2021, aes(x=Age, y=Mortality, color=Year)) +
geom_point(aes(color=Year), size=1) +
  lapply(unique(infant_g_2021$Year), function(Year) {
    stat_function(fun = interp_fun_by_year(Year), color="black")
  }) +
      scale_y_continuous(trans='log10', breaks = c(0,1,2,5,10,20,50,100,200,500)) +
      #scale_x_continuous(trans='log10') +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_colour_manual(values=cc) +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Infant mortality rates decline after birth, with the shape of a power law", 
       subtitle = "The chances of dying are highest during the first few days of an infant's life.\nOver the following days, weeks and months, their chances of dying decrease sharply. \nThe straight line on the log-log plot indicates a steady, predictable decline as they age.\nOver time, the mortality rate has declined across the entire first year of an infant's life.", 
       y = "Daily mortality rate (per 100,000)", 
       x = "Age (days)",
       caption = "Source: Office for National Statistics, UK") 
