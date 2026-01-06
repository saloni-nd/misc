library(tidyverse)

# Import all csvs in the folder
csvs <- list.files("", pattern = "-immunization-schedule\\.csv$", full.names = TRUE) %>%
  set_names(~ str_remove(basename(.x), "-immunization-schedule\\.csv$")) %>%
  map(read_csv)

# Join into one df
all_schedules <- imap_dfr(csvs, ~{ names(.x)[4] <- "Schedule"; mutate(.x, Vaccine = .y) })

# Clean name of vaccine
all_schedules <- all_schedules %>%
  mutate(
    Vaccine = Vaccine %>%
      str_replace_all("-", " ") %>%     # replace dashes with spaces
      str_remove(regex("\\bvaccine\\b", ignore_case = TRUE)) %>%  # remove the word "vaccine"
      str_squish() %>%                  # clean up extra spaces
      str_to_sentence()                 # capitalise first letter only
  )

write_csv(all_schedules, "all_schedules.csv")