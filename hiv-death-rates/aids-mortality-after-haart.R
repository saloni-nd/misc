library(tidyverse)

file_path <- "" # Update

# Import data
icd9_df <- read_tsv(paste0(file_path, "Compressed Mortality, 1979-1998.txt"))
icd10_df <- read_tsv(paste0(file_path, "Compressed Mortality, 1999-2016.txt"))

# Remove NAs with text comments
icd9_df$`Crude Rate` <- as.numeric(icd9_df$`Crude Rate`)
icd9_df$Population <- as.numeric(icd9_df$Population)

icd10_df$`Crude Rate` <- as.numeric(icd10_df$`Crude Rate`)
icd10_df$Population <- as.numeric(icd10_df$Population)

# Add column with ICD version
icd9_df <- icd9_df %>%
  mutate(ICD_version = "ICD-9")

icd10_df <- icd10_df %>%
  mutate(ICD_version = "ICD-10")

# Use bind_rows to merge them
combined_df <- bind_rows(icd9_df, icd10_df)

# Keep only age groups between 15 and 85
filtered_df <- combined_df %>% 
  filter(`Age Group Code` %in% c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84"))

# Modify the factor levels of ICD_version to ensure correct order in the legend
filtered_df$ICD_version <- factor(filtered_df$ICD_version, levels = c("ICD-9", "ICD-10"))

# Plot
ggplot(data=filtered_df, aes(x=Year, y=`Crude Rate`, color=`Age Group Code`, linetype=ICD_version)) +
  geom_line() +
  geom_vline(xintercept = 1995, linetype = "dashed", color = "black") + # Dashed line
  annotate("text", x = 1995, y = max(combined_df$`Crude Rate`, na.rm = TRUE), 
           label = "HAART introduced", vjust = -0.5, hjust = -0.1) + # Annotated with HAART introduced
  scale_linetype_manual(values = c("ICD-9" = "solid", "ICD-10" = "dashed")) + # Specify linetypes for ICD versions
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) + # X-axis breaks each 5 years between 1985 and 2015
  coord_cartesian(xlim = c(1985, 2015)) + # Limit timeline to 1985 to 2015
  theme_classic() +
  labs(title="HIV/AIDS death rates shrank after HAART was introduced", 
       subtitle="Crude HIV/AIDS death rate per 100,000 people", 
       y="",
       color="Age group",
       linetype="ICD version",
       size="ICD version",
       caption="Chart by Saloni Dattani\nSource: NCHS, via Compressed Mortality File Archives") +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = 16)) +
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2)) # Specify order of legend categories
                          
                          
                          
