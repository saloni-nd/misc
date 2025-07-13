library(tidyverse)
library(lubridate)

# Data source:
# https://www.accessdata.fda.gov/scripts/opdlisting/oopd/listResult.cfm

data_folder <- ""

# Read your CSV file
orphan_df <- read_csv(paste0(data_folder, "Orphan drugs - Sheet2.csv"))

# Process the data
orphan_df_cleaned <- orphan_df %>%
  mutate(generic_lower = tolower(`Generic Name`)) %>%  # create a lowercase version
  distinct(generic_lower, .keep_all = TRUE) %>%
  mutate(approval_year = year(mdy(`Marketing Approval Date`))) %>%  # extract year from approval date
  filter(!is.na(approval_year)) # remove rows with missing approval year

orphan_summary <- orphan_df_cleaned %>%
  count(approval_year, name = "num_unique_drugs")  # count unique drugs by year

# View the summary
print(orphan_summary)

write_csv(orphan_summary, paste0(data_folder, "orphan_approvals_2025.csv"))

