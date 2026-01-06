library(tidyverse)

filepath <- "misc/vaccination_schedules/"

all_schedules <- read_csv(paste0(filepath, "all_schedules.csv"))

# Rename Entire country with Universal
all_schedules <- all_schedules %>%
  mutate(
    Schedule = str_replace_all(Schedule, "Entire country", "Universal")
  )

# Set colours
schedule_cols <- c(
  "Not introduced" = "grey85",
  "Not routinely administered" = "#F6C27A",  # pastel orange
  "Specific risk groups" = "#C7B3E5",         # pastel purple
  "Universal" = "#A8D8F0"                     # pastel light blue
)

year_range <- range(all_schedules$Year, na.rm = TRUE)

# Get Denmarks schedule, and 
denmark_sch <- all_schedules %>%
  filter(Entity == "Denmark") %>% 
  filter(Vaccine != "Hepatitis b") %>% # Remove hepatitis b since its already in Hepatitis b birth dose
  complete(
    Vaccine,
    Year = seq(year_range[1], year_range[2]),
    fill = list(Schedule = "Not introduced")
  ) # Fill grid with all years, so blank years say not introduced
  
# Plot heatmap
ggplot(denmark_sch, aes(x = Year, y = Vaccine, fill = Schedule)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = schedule_cols,drop = FALSE) +
  labs(
    title = "What's in the vaccination schedule in Denmark?",
    x = "Year",
    y = "",
    fill = "",
    caption = "Data source: WHO via OWID (2025)\nChart by Saloni Dattani"
  ) +
  scale_y_discrete(expand = c(0, 0)) + 
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # horizontal labels
    axis.ticks.x = element_line(),                       # show tick marks
    legend.position = "top",
    axis.text.y = element_text(margin = margin(r = 0)),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.margin = margin(5, 5, 5, 5)
  )

