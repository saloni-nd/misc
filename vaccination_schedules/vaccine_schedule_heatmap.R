library(tidyverse)
library(readxl)

filepath <- ""

# Import all spreadsheets in the folder
who_schedules <- list.files(
  paste0(filepath, "Data/"),
  pattern = "vaccine-schedule\\.xlsx$",
  full.names = TRUE
) %>%
  map(~ read_excel(.x, sheet = 1)) %>%
  map(~ { names(.x)[7] <- "Schedule"; .x }) %>%
  bind_rows()

who_schedules <- who_schedules %>%
                  select(-c(WHO_REGION, DESCRIPTION))

# Recode vars
who_schedules <- who_schedules %>%
  mutate(
    Schedule = recode(
      Schedule,
      "No"      = "Not routinely administered",
      "Yes"     = "Universal",
      "Yes (R)" = "Specific risk groups",
      "Yes (O)" = "Outbreaks",
      "Yes (P)" = "Partial"
    )
  ) %>%
  mutate(
    ANTIGEN = ANTIGEN %>%
      str_replace_all("_", " ") %>%  # replace underscores with spaces
      str_to_sentence()              # sentence case (first letter capitalised)
  )

who_schedules <- who_schedules %>%
  rename(
    Code    = ISO_3_CODE,
    Entity  = COUNTRYNAME,
    Year    = YEAR,
    Vaccine = ANTIGEN
  )

# Recode names
who_schedules <- who_schedules %>%
  mutate(
    Vaccine = recode(
      Vaccine,
      "Varicella"   = "Chickenpox",
      "Rubella"     = "Rubella",
      "Rotavirus"   = "Rotavirus",
      "Pneumo conj" = "Pneumococcal (conjugate)",
      "Mumps"       = "Mumps",
      "Mmcv"        = "Meningococcal",
      "Mcv2"        = "Measles (2nd dose)",
      "Ipv" = "Polio (IPV)",
      "Influenza"   = "Influenza",
      "Hpv"         = "HPV",
      "Hib"         = "Haemophilus influenzae b",
      "Hepb bd" = "Hep B birth dose",
      "Hepa"        = "Hep A",
      "Ap"          = "Pertussis (acellular)"
    )
  )


write_csv(who_schedules, paste0(filepath, "combined_vaccine_introduction.csv"))

# Set colours
schedule_cols <- c(
  "Not introduced" = "grey85",
  "Not routinely administered" = "#F6C27A",  # pastel orange
  "Specific risk groups" = "#C7B3E5",         # pastel purple
  "Universal" = "#A8D8F0"                     # pastel light blue
)

year_range <- range(who_schedules$Year, na.rm = TRUE)

# USA

# Get USA schedule, and apply fills
usa_sch <- who_schedules %>%
  filter(Code == "USA") %>% 
  complete( # Fill grid with all years, so blank years say not introduced
    Vaccine,
    Year = seq(year_range[1], year_range[2]),
    fill = list(Schedule = "Not introduced")
  ) 

# Plot heatmap
ggplot(usa_sch, aes(x = Year, y = factor(Vaccine, levels = rev(sort(unique(Vaccine)))), fill = Schedule)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = schedule_cols,drop = FALSE) +
  labs(
    title = "What's in the vaccination schedule in the United States?",
    x = "Year",
    y = "",
    fill = "",
    caption = "Data source: WHO (2025)\nChart by Saloni Dattani"
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

ggsave(paste0(filepath, "usa-schedule-heatmap.png"), height=5, width=8, bg = "white")


# Get Denmark schedule, and apply fills
denmark_sch <- who_schedules %>%
  filter(Entity == "Denmark") %>% 
  complete( # Fill grid with all years, so blank years say not introduced
    Vaccine,
    Year = seq(year_range[1], year_range[2]),
    fill = list(Schedule = "Not introduced")
  ) 

# Plot heatmap
ggplot(denmark_sch, aes(x = Year, y = factor(Vaccine, levels = rev(sort(unique(Vaccine)))), fill = Schedule)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = schedule_cols,drop = FALSE) +
  labs(
    title = "What's in the vaccination schedule in Denmark?",
    x = "Year",
    y = "",
    fill = "",
    caption = "Data source: WHO (2025)\nChart by Saloni Dattani"
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

ggsave(paste0(filepath, "denmark-schedule-heatmap.png"), height=5, width=8, bg = "white")



