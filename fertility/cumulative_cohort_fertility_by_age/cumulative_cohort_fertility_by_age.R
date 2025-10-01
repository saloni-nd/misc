library(tidyverse)

## Download the data:
## https://www.humanfertility.org/
## Choose a country
## Under `Cumulative fertility rates`, download the table `All birth orders combined` as a txt file


file_path <- "/misc/fertility/cumulative_cohort_fertility_by_age/"

cfr_df <- read_table(
  file = paste0(file_path, "SWE_cumulative_fert.txt"),
  skip = 2   # skips first 2 lines, 3rd line becomes header
)

cfr_50 <- cfr_df %>%
            filter(Age == 50) %>%
            select(-Age)
colnames(cfr_50) <- c("Cohort", "CCFR_50")


cfr_45 <- cfr_df %>%
            filter(Age == 45) %>%
            select(-Age)
colnames(cfr_45) <- c("Cohort", "CCFR_45")

cfr_40 <- cfr_df %>%
            filter(Age == 40) %>%
            select(-Age)
colnames(cfr_40) <- c("Cohort", "CCFR_40")


cfr_all <- cfr_40 %>%
  full_join(cfr_45, by = "Cohort") %>%
  full_join(cfr_50, by = "Cohort") %>%
  mutate(across(-Cohort, as.numeric))

write_csv(cfr_all, paste0(file_path, "SWE_cfr_40_45_50.csv"))

