library(tidyverse)

data_folder <- "olympics/1896-2022/"

olympics_results <- read_csv(paste0(data_folder, "olympic_results.csv"))
olympics_athletes <- read_csv(paste0(data_folder, "olympic_athletes.csv"))
olympics_host <- read_csv(paste0(data_folder, "olympic_hosts.csv"))

## Categories
olympics_results$discipline_title <- as.factor(olympics_results$discipline_title)
olympics_results$event_title <- as.factor(olympics_results$event_title)
olympics_results$medal_type <- as.factor(olympics_results$medal_type)
olympics_results$rank_position <- as.integer(olympics_results$rank_position)

#### 1. Average age of athletes at their first Olympics game over time

# Join datasets on game_name/first_game and calculate age
athlete_with_season <- olympics_athletes %>%
  inner_join(olympics_host, by = c("first_game" = "game_name")) %>%
  mutate(
    age_at_first_game = game_year - athlete_year_birth
  )


# Calculate average age at first game by year and season
average_age_by_season <- athlete_with_season %>%
  group_by(game_year, game_season) %>%
  summarise(average_age = mean(age_at_first_game, na.rm = TRUE))

# Plot the average age by year and season with different colors
ggplot(average_age_by_season, aes(x = game_year, y = average_age, color = game_season)) +
  geom_line() +
  geom_point() +
  labs(title = "Average age of athletes at their first Olympics",
       x = "Year",
       y = "",
       color = "Olympics season") +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size =12))

write_csv(average_age_by_season, paste0(data_folder, "age_first_olympics_by_season.csv"))

average_age_by_season_wide <- average_age_by_season %>%
  pivot_wider(
    names_from = game_season,
    values_from = average_age
  )

write_csv(average_age_by_season_wide, paste0(data_folder, "age_first_olympics_by_season_wide.csv"))


#### 2. Average medals per country over time

olympics_medals <- read_csv(paste0(data_folder, "olympic_medals.csv"))

# Define the country name dictionary for replacement
country_dict <- c(
  "German Democratic Republic (Germany)" = 'GDR', 
  "Federal Republic of Germany" = 'FRG',
  "Democratic People's Republic of Korea" = 'North Korea',
  "Republic of Korea" = "South Korea",
  "People's Republic of China" = 'China',
  "Islamic Republic of Iran" = 'Iran',
  "United States of America" = 'USA',
  "ROC" = 'Russia',
  "Russian Federation" = "Russia"
)

# Replace long country names with short analogs
olympics_medals$country_name <- recode(olympics_medals$country_name, !!!country_dict)

# Extract year from slug_game and add as a new column
olympics_medals$year <- as.numeric(sub(".*-(\\d+).*", "\\1", olympics_medals$slug_game))

# Summarize total medals per country per year
medals_summary <- olympics_medals %>%
  group_by(country_name, year) %>%
  summarize(total_medals = n(), .groups = 'drop')

# Define specific colors for selected countries
country_colors <- c(#"USA" = "blue", 
                   # "China" = "red", 
                   # "France" = "green", 
                   "GDR" = "#B22222",
                   "FRG" = "orange",
                   "Germany" = "blue")
                   # "Great Britain" = "green",
                   # "Russia" = "#B57EDC",
                   # "Soviet Union" = "#6A0DAD")
default_color <- "grey"
default_alpha <- 0.3

# Create a new column for color and alpha
medals_summary$color <- ifelse(medals_summary$country_name %in% names(country_colors), 
                               medals_summary$country_name, 
                               default_color)

# Create the plot
ggplot(medals_summary, aes(x = year, y = total_medals)) +
  geom_point(aes(color = color, alpha = country_name %in% names(country_colors)), size = 2) +
  scale_color_manual(values = c(country_colors, default_color)) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = default_alpha), guide = 'none') +
  theme_minimal() +
  labs(title = "Total Olympic medals per country",
       x = "Year",
       y = "",
       color = "Country",
       alpha = "Highlighted") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold")) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) 

write_csv(medals_summary, paste0(data_folder, "medals-summary.csv"))
