library(tidyverse)
library(extrafont)

# Import Lato font
font_import(pattern = c("Lato", "Playfair Display"))
loadfonts(device = "mac") # use "win" for Windows, "mac" for macOS

data_folder <- ""

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/long-run-birth-rate.csv?v=1&csvType=full&useColumnShortNames=true")

# Filter for countries with non-NA birth rate data in 1935
countries_1935 <- unique(df$Entity[!is.na(df$birth_rate_hist) & df$Year == 1939])

# Filter the dataset to include only those countries
filtered_data <- df %>%
  filter(Entity %in% countries_1935)

# Define the range of WWII years
wwii_start <- 1939
wwii_end <- 1945

# Remove England and Wales, Scotland, Northern Ireland, which have limited data after 1940s
filtered_data <- filtered_data[!(filtered_data$Entity %in% c("England and Wales", "Scotland", "Northern Ireland")), ]

# Fill in missing years for each Country and Collection combination
filtered_data <- filtered_data %>%
  group_by(Entity) %>%
  complete(Year = full_seq(Year, 1), fill = list(birth_rate_hist = NA)) %>%
  ungroup()


# Create the faceted plot
ggplot(filtered_data, aes(x = Year, y = birth_rate_hist)) +
  # Highlight WWII period
  geom_rect(aes(xmin = wwii_start, 
                xmax = wwii_end, 
                ymin = -Inf, 
                ymax = Inf),
            fill = "lightpink", 
            alpha = 0.2, 
            inherit.aes = FALSE) +
  # Add line plot
  geom_line(na.rm = TRUE, color = "purple") +
  # Facet by Country
  facet_wrap(~ Entity, ncol = 3, nrow = 5, scales = "free_y") +
  # Labels and style
  labs(
    title = "Birth rate",
    subtitle = "A historical perspective of birth rates during WWII",
    x = "Year",
    y = "",
    caption = "Source: Human Mortality Database (2024)"
  ) +
  scale_x_continuous(limits = c(1910, 2024)) +
  theme_minimal() +
  theme(
    # Background
    panel.background = element_rect(fill = "white", color = NA), # White background
    plot.background = element_rect(fill = "white", color = NA),  # White overall background
    
    # Text styling
    strip.text = element_text(size = 12, face = "bold", color = "grey30"), 
    text = element_text(family = "Lato", color = "grey50"),
    axis.text = element_text(size = 10, color = "grey50"),
    axis.title = element_text(size = 12, color = "grey50"),
    plot.title = element_text(size = 25, family = "Playfair Display SemiBold", color = "grey10"),
    plot.subtitle = element_text(size = 15, face = "plain", family = "Lato", color = "grey30"),
    plot.caption = element_text(size = 10, color = "grey50", hjust = 0), # Left-align caption
    plot.caption.position = "plot",                                   # Position caption below the plot
    
    # Lines in grey
    panel.grid = element_blank(),                        # Remove grid lines
    axis.ticks = element_line(color = "grey70"),         # Axis ticks in grey
    axis.ticks.length = unit(0.2, "cm"),                 # Customize tick length
    panel.border = element_rect(color = "grey70", fill = NA) # Axis lines in grey
  )

# Save the plot with specified pixel dimensions
ggsave(
  filename = paste0(data_folder, "birth-rates-countries-wwii.svg"),
  plot = last_plot(),                # Saves the last plot
  width = 11.8,
  height = 15.2,
  units = "in",                      # Specify units as inches
  dpi = 300                          # Set resolution to 300 DPI
)
