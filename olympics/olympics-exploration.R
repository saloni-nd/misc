library(tidyverse)

data_folder <- ""

olympics_results <- read_csv(paste0(data_folder, "olympic_results.csv"))

## Categories
olympics_results$discipline_title <- as.factor(olympics_results$discipline_title)
olympics_results$event_title <- as.factor(olympics_results$event_title)
olympics_results$medal_type <- as.factor(olympics_results$medal_type)
olympics_results$rank_position <- as.integer(olympics_results$rank_position)

olympics_athletes <- read_csv(paste0(data_folder, "olympic_athletes.csv"))

# Calculate age at first game
olympics_athletes <- olympics_athletes %>%
  mutate(
    first_game_year = as.numeric(sub(".*\\s", "", first_game)),
    age_at_first_game = first_game_year - athlete_year_birth
  )

# Calculate average age at first game by year
average_age <- olympics_athletes %>%
  group_by(first_game_year) %>%
  summarise(average_age = mean(age_at_first_game, na.rm = TRUE))

# Plot the average age at first game
ggplot(average_age, aes(x = first_game_year, y = average_age)) +
  geom_line() +
  geom_point() +
  labs(title = "Average age of athletes at their first Olympic game",
       #subtitle = "Average age of athletes",
       x = "Year of first game",
       y = "") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) 
  
