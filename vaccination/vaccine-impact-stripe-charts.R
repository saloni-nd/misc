library(tidyverse)
library(scales)
library(extrafont)

# Import fonts
font_import(pattern = c("Lato", "Playfair Display"))
loadfonts(device = "mac") # use "win" for Windows, "mac" for macOS

# Define paths
data_folder <- ""

# Load datasets
polio_df <- read.csv("https://ourworldindata.org/grapher/reported-paralytic-polio-cases-and-deaths-in-the-united-states-since-1910.csv?v=1&csvType=full&useColumnShortNames=true")
colnames(polio_df) <- c("Entity", "Code", "Year", "Cases", "Deaths")

measles_df <- read.csv("https://ourworldindata.org/grapher/measles-cases-and-death.csv?v=1&csvType=full&useColumnShortNames=true")
colnames(measles_df) <- c("Entity", "Code", "Year", "Cases", "Deaths")

# Function to process data
process_data <- function(df) {
  df %>%
    pivot_longer(cols = c("Cases", "Deaths"), names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(NormalizedValue = (Value - min(Value, na.rm = TRUE)) / 
             (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))) %>%
    ungroup()
}

# Function to plot data
plot_data <- function(data, title, annotation_year, annotation_label, output_file) {
  # Calculate decade breaks
  start_decade <- floor(min(data$Year) / 10) * 10
  end_decade <- ceiling(max(data$Year) / 10) * 10
  decade_breaks <- seq(start_decade, end_decade, by = 10)
  
  # Create plot
  chart <- ggplot(data, aes(x = Year, y = 1, fill = NormalizedValue)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_gradient(
      low = "white", 
      high = "red", 
      na.value = "lightgrey"
    ) +
    scale_x_continuous(
      breaks = decade_breaks,  # Set x-axis labels at each decade
      expand = c(0, 0)
    ) +
    geom_vline(xintercept = annotation_year, color = "black", size = 0.8) +
    annotate("text", 
             x = annotation_year + 0.5, 
             y = 1.2, 
             label = annotation_label, 
             color = "black", 
             hjust = 0, 
             size = 4,
             family = "Lato") +
    facet_wrap(~Metric, ncol = 1, strip.position = "top") +  # Independent color ranges achieved via normalization
    theme_void() +
    theme(
      # Background
      panel.background = element_rect(fill = "white", color = NA), # White background
      plot.background = element_rect(fill = "white", color = NA),  # White overall background
      
      # Text styling
      strip.text.x = element_text(size = 12, face = "bold", color = "grey30", hjust = 0), # Align strip text to the left
      text = element_text(family = "Lato", color = "grey50"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.length = unit(4, "pt"), # Adjust the tick length here
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, color = "grey50"),
      axis.title = element_text(size = 12, color = "grey50"),
      plot.title = element_text(size = 25, family = "Playfair Display SemiBold", color = "grey10"),
      plot.subtitle = element_text(size = 15, face = "plain", family = "Lato", color = "grey30"),
      plot.caption = element_text(size = 10, color = "grey50", hjust = 0), # Left-align caption
      plot.caption.position = "plot",   # Position caption below the plot
      legend.position = "none",  # Remove the legend
      plot.margin = margin(t = 10, r = 20, b = 20, l = 10)  # Add space around the plot
    ) +
    labs(
      title = title,
      x = "Year", 
      y = NULL, 
      fill = NULL
    )
  
  # Save plot
  ggsave(output_file, chart, width = 10, height = 3)
}

# Process datasets
polio_long <- process_data(polio_df)
measles_long <- process_data(measles_df)

# Create plots
plot_data(
  polio_long, 
  "Polio in the United States", 
  1953, 
  "Salk vaccine introduced", 
  paste0(data_folder, "polio-vaccine-impact-US-stripe.svg")
)

plot_data(
  measles_long, 
  "Measles in the United States", 
  1963, 
  "Measles vaccine introduced", 
  paste0(data_folder, "measles-vaccine-impact-US-stripe.svg")
)
