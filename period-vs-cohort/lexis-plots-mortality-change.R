library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)
library(RColorBrewer)
library(latticeExtra)
library(assertthat)

# Data source: Human Mortality Database, on mortality.org
# Download and save to folder below, as `Mx_1x1_[country].txt`
file_path <- "/Github/misc/period-vs-cohort/"

countries <- c("ITA")
mortality <- list()

### Try using populations and deaths instead

population <- list()
deaths <- list()

# Import data
for (country in countries) {
  
  # Import and rename cols
  population[[country]] <- read_table(paste0(file_path, "Population_", country, ".txt"), skip=2)
  colnames(population[[country]]) <- c("Year", "Age", "Female", "Male", "Total")
  
  population[[country]] <- population[[country]] %>%
    mutate(Country = country)
  
}

# Import data
for (country in countries) {
  
  # Import and rename cols
  deaths[[country]] <- read_table(paste0(file_path, "Deaths_", country, ".txt"), skip=2)
  colnames(deaths[[country]]) <- c("Year", "Age", "Female", "Male", "Total")
  
  deaths[[country]] <- deaths[[country]] %>%
    mutate(Country = country)
  
}

# Join into single df
population <- do.call(rbind.data.frame, population)
deaths <- do.call(rbind.data.frame, deaths)

# Gather
population <- population %>%
  gather(key = Sex, value = Population, Female:Total)

deaths <- deaths %>%
  gather(key = Sex, value = Deaths, Female:Total)

joined <- merge(population, deaths, by=c("Country", "Year", "Age", "Sex"))
joined <- joined %>% mutate(Mx = Population / Deaths)

joined$Year <- as.numeric(joined$Year)
joined$Age <- as.numeric(joined$Age)


######## Use either `mortality` or `joined` here

# Create vars for log10 mortality rate and ln mortality rate
mort_hmd <- joined %>%
  mutate(
    birth_year = Year - Age
  ) %>%
  mutate(
    log10_cmr  = log(Mx, base = 10),
    ln_mr      = log(Mx, base = exp(1))
  )

mort_hmd <- mort_hmd %>%
  arrange(Country, Age, Sex) %>%
  group_by(Country, Sex) %>%
  mutate(ch_mr = Mx - lag(Mx)) %>%  # must be done separately by group
  ungroup()

mort_hmd <- mort_hmd %>%
  arrange(Country, Age, Sex) %>%
  group_by(Sex, Age) %>%
  mutate(change_log10_cmr = log10_cmr - lag(log10_cmr)) %>%
  ungroup()

mort_hmd <- mort_hmd %>%
  mutate(
    change_log10_cmr = ifelse(change_log10_cmr < -0.1, -0.1, change_log10_cmr),
    change_log10_cmr = ifelse(change_log10_cmr > 0.1, 0.1, change_log10_cmr)
  )

# Data above age 90 is pretty bad
mort_hmd <- mort_hmd %>%
  #dplyr::filter(Age  < 91) %>%
  dplyr::filter(Year > 1899)


cores = parallel::detectCores() - 1L

# Lexis surface (contour) plots
parallel::mclapply(as.list(unique(mort_hmd$Country)), function(Country) {
  
  mort_hmd <- mort_hmd[mort_hmd$Country == Country, ]
  
  mort_plot <- mort_hmd %>%
    lattice::levelplot(
      change_log10_cmr ~ Year * Age | Sex,
      data = .,
      par.strip.text = list(cex = 1.4, fontface = "bold"),
      xlab = list(label = "Year", cex = 1.4),
      ylab = list(label = "Age in years", cex = 1.4),
      par.settings = list(strip.background = list(col = "lightgrey")),
      scales = list(
        x = list(cex = 1.2, at = seq(1900, 2010, by = 10), rot = 90),
        y = list(cex = 1.2, at = seq(0, 90, by = 10)),
        alternating = TRUE
      ),
      aspect = "iso",
      col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
      at = seq(-0.1, 0.1, by = 0.025)
    )
  
  png(filename = paste0(file_path, "mort_", country, ".png"),
      width = 420, height = 148, units = "mm", res = 300)
  print(mort_plot)
  dev.off()
  
},
mc.cores = cores
)
