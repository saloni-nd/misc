# Open libraries
library(tidyverse)
library(scales)
library(viridis)
library(patchwork)

# Data source:
# CDC Wonder https://wonder.cdc.gov/
# 1. Underlying cause of death - group by single age - group by ICD chapter (By cause)
# 2. Underlying cause of death - group by single age (All causes)
# Download and replace this with path to folder
data_folder <- ""


#----- DATA IMPORT

### Import all cause age-specific mortality
all_causes <- read_tsv(paste0(data_folder, "underlying-cod-2018-2021-all-causes.txt"))

# Remove quotation marks
all_causes <- all_causes %>% 
  mutate(across(
    everything(),
    ~ map_chr(.x, ~ gsub("\"", "", .x))
  ))

# Rename col names & add dummy variables for all causes to join later
all_causes <- all_causes %>%
  rename(Age = `Single-Year Ages Code`) %>%
  rename(Crude_rate = `Crude Rate`) %>%
  mutate(ICD_chapter = "All causes") %>%
  mutate(ICD_chapter_code = "ALL")

# Change to numeric
all_causes$Age <- as.numeric(all_causes$Age)
all_causes$Deaths <- as.numeric(all_causes$Deaths)
all_causes$Population <- as.numeric(all_causes$Population)
all_causes$Crude_rate <- as.numeric(all_causes$Crude_rate)
all_causes$ICD_chapter <- as.factor(all_causes$ICD_chapter)

# Keep ages 0-100
all_causes <- all_causes %>%
  filter(Age >= 0) %>%
  filter(Age <= 100)

# Rearrange columns
all_causes <- all_causes %>%
              select(Notes, `Single-Year Ages`, Age, ICD_chapter, ICD_chapter_code, Deaths, Population, Crude_rate)

### Import age-specific mortality by ICD chapter
by_cause <- read_tsv(paste0(data_folder, "underlying-cod-2018-2021-by-cause.txt"))

# Remove quotation marks
by_cause <- by_cause %>% 
  mutate(across(
    everything(),
    ~ map_chr(.x, ~ gsub("\"", "", .x))
  ))

# Rename col names
by_cause <- by_cause %>%
  rename(Age = `Single-Year Ages Code`) %>%
  rename(Crude_rate = `Crude Rate`) %>%
  rename(ICD_chapter = `ICD Chapter`)

# Change to numeric
by_cause$Age <- as.numeric(by_cause$Age)
by_cause$Deaths <- as.numeric(by_cause$Deaths)
by_cause$Population <- as.numeric(by_cause$Population)
by_cause$Crude_rate <- as.numeric(by_cause$Crude_rate)
by_cause$ICD_chapter <- as.factor(by_cause$ICD_chapter)

# Keep ages 0-100
by_cause <- by_cause %>%
  filter(Age >= 0) %>%
  filter(Age <= 100)

#--- JOIN
mx_age <- bind_rows(all_causes, by_cause)

# Select cause categories to retain from plot
mx_age <- mx_age %>%
            filter(ICD_chapter %in% c("All causes",
                                      "Certain infectious and parasitic diseases",
                                      "Diseases of the circulatory system",
                                      "Diseases of the respiratory system",
                                      "External causes of morbidity and mortality",
                                      "Neoplasms")) 

  # Rename ICD chapters to human-readable names
mx_age$ICD_chapter <- recode(mx_age$ICD_chapter, "Certain infectious and parasitic diseases" = "Infectious & parasitic diseases",
                 "Diseases of the circulatory system" = "Circulatory diseases",
                 "Diseases of the respiratory system" = "Respiratory disorders",
                 "Neoplasms" = "Cancers",
                 "External causes of morbidity and mortality" = "External causes")


#--- PLOT

# Plot for the "All causes" facet
plot_focus <- ggplot(mx_age[mx_age$ICD_chapter == "All causes",], 
                     aes(x=Age, y=Crude_rate)) +
  geom_line(color="#883039") +
  theme_classic() +
  labs(title = "All causes") +
  ylab("") +
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by=20)) +
  scale_y_continuous(labels = comma, trans='log2', breaks = c(0.1,1,10,100,1000,10000,100000)) 

# Plot for the remaining categories
plot_rest <- ggplot(mx_age[mx_age$ICD_chapter != "All causes",], 
                    aes(x=Age, y=Crude_rate)) +
  geom_line(color="#4C6A9C") +
  theme_classic() +
  facet_wrap(~ ICD_chapter, ncol = 1) +
  ylab("") +
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by=20)) +
  scale_y_continuous(labels = comma, trans='log2', breaks = c(0.1,1,10,100,1000,10000,100000)) 

# Combine the two plots
combined_plot <- (plot_focus | plot_rest) & 
  plot_annotation(title = "How do mortality rates vary by age?",
                  subtitle = "Annual mortality rates, per 100,000 people",
                  caption = "Period death rates between 2018-2021. Source: CDC Wonder database.") &
  plot_layout(ncol = 2, widths = c(3, 1), guides = "collect")

print(combined_plot)


