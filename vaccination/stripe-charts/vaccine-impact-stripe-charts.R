library(tidyverse)
library(scales)
library(extrafont)
library(ggnewscale)

# Import fonts
font_import(pattern = c("Lato", "Playfair Display"))
loadfonts(device = "mac") # use "win" for Windows, "mac" for macOS

# Load dataset
vpd_raw <- read.csv("https://raw.githubusercontent.com/saloni-nd/misc/refs/heads/main/vaccination/stripe-charts/vpd-cases-deaths.csv")

# Rename columns
vpd_df <- vpd_raw %>%
  dplyr::select(Entity, Year, Pertussis.cases, Pertussis.deaths, Mumps.cases, Mumps.deaths, Number.of.measles.cases, Measles.deaths, Diphtheria.cases, Diphtheria.deaths, Reported.polio.cases, Polio.deaths, Rubella.cases) %>%
  rename(Measles.cases = Number.of.measles.cases) %>%
  rename(Polio.cases = Reported.polio.cases)


full_df <- expand.grid(
  Entity = unique(vpd_df$Entity),
  Year = 1910:2024
)
# Ensuring there is a row for each year even if there isn't any data for that year
vpd_df <- merge(vpd_df, full_df, by = c("Entity", "Year"), all = TRUE) 

# Reshape the dataset
vpd_long <- vpd_df %>%
  pivot_longer(
    cols = -c(Entity, Year),
    names_to = "PanelTitleRaw",
    values_to = "Value"
  ) %>%
  arrange(Year) %>%
  # Split into Disease and Metric (Cases or Deaths)
  mutate(
    Disease = str_remove(PanelTitleRaw, "\\.(cases|deaths)$") %>%
      str_replace_all("\\.", " ") %>%
      str_to_title(),
    Metric = case_when(
      str_detect(PanelTitleRaw, "\\.cases$") ~ "Cases",
      str_detect(PanelTitleRaw, "\\.deaths$") ~ "Deaths"
    )
  ) %>%
  select(Entity, Year, Disease, Metric, PanelTitleRaw, Value) %>%
  # Fill in the dataset so there is a row for each year between 1910 and 2024, and the values are set to NA if there's nothing there
  complete(
    Year = unique(Year),
    Disease = unique(Disease),
    Metric = c("Cases", "Deaths"),
    fill = list(Value = NA)
  ) %>%
  group_by(Disease, Metric) %>%
  # Add a column for Normalized Value: this scales the colour gradient so each disease-metric has a min and max
  mutate(
    NormalizedValue = if (all(is.na(Value))) NA_real_ else
      (Value - min(Value, na.rm = TRUE)) /
      (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  # Remove rows where PanelTitleRaw is NA
  filter(!is.na(PanelTitleRaw))

# Preview
vpd_long %>% print(n = 10)

# Create an annotation dataframe for the labels for when vaccines were introduced
annotations <- expand_grid(
  Disease = c("Diphtheria", "Measles", "Mumps", "Pertussis", "Polio", "Rubella"),
  Metric = c("cases", "deaths")
) %>%
  mutate(
    PanelTitleRaw = paste0(Disease, ".", Metric),
    AnnotationYear = case_when(
      Disease == "Diphtheria" ~ 1914,
      Disease == "Measles" ~ 1963,
      Disease == "Mumps" ~ 1967,
      Disease == "Pertussis" ~ 1914,
      Disease == "Polio" ~ 1955,
      Disease == "Rubella" ~ 1969
    ),
    AnnotationLabel = paste(Disease, "vaccine introduced")
  ) %>%
  select(PanelTitleRaw, AnnotationYear, AnnotationLabel) %>% 
  filter(PanelTitleRaw != "Rubella.deaths")


# Create combined plot
combined_plot <- ggplot() +
  # For cases
  geom_tile(
    data = vpd_long %>% filter(Metric == "Cases"), 
    aes(x = Year, y = 1, fill = NormalizedValue),
    color = "white", size = 0.2
  ) +
  scale_fill_gradient(low = "white", high = "purple", na.value = "lightgrey") +
  new_scale_fill() + # Allow a new fill scale
  # For deaths
  geom_tile(
    data = vpd_long %>% filter(Metric == "Deaths"), 
    aes(x = Year, y = 1, fill = NormalizedValue),
    color = "white", size = 0.2
  ) +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey") +
  # X axis breaks
  scale_x_continuous(
    breaks = seq(floor(min(vpd_long$Year) / 10) * 10, 
                 ceiling(max(vpd_long$Year) / 10) * 10, by = 10),
    expand = c(0, 0)
  ) +
  # Add line for year of vaccine introduction
  geom_vline(data = annotations, aes(xintercept = AnnotationYear), color = "black", size = 0.8) +
  #geom_text(data = annotations, aes(x = AnnotationYear + 0.5, y = 1.2, label = AnnotationLabel), 
  #         inherit.aes = FALSE, color = "black", hjust = 0, size = 4, family = "Lato") +
  facet_wrap(~PanelTitleRaw, 
             ncol = 1, # Each panel on one line
             strip.position = "left") +
  # scales = "free_x") +  # Each panel has its own timeline
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.border = element_rect(color = "grey50", size = 0.5, fill = NA), 
    strip.text = element_text(size = 12, face = "bold", color = "grey30", hjust = 0), 
    text = element_text(family = "Lato", color = "grey50"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"), 
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, color = "grey50"),
    axis.title = element_text(size = 12, color = "grey50"),
    plot.title = element_text(size = 25, family = "Playfair Display SemiBold", color = "grey10"),
    plot.subtitle = element_text(size = 15, face = "plain", family = "Lato", color = "grey30"),
    plot.caption = element_text(size = 10, color = "grey50", hjust = 0),
    plot.caption.position = "plot",   
    legend.position = "none",  
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  ) +
  labs(
    title = "Impact of Vaccines in the United States",
    x = "Year", 
    y = NULL, 
    fill = NULL
  )

combined_plot

# Save plot
ggsave("combined-vaccine-impact-US-stripe.svg", combined_plot, width = 12, height = 12)