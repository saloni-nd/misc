library(tidyverse)
library(scales)
library(extrafont)
library(ggnewscale)

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
process_data <- function(df, disease) {
  df %>%
    pivot_longer(cols = c("Cases", "Deaths"), names_to = "Metric", values_to = "Value") %>%
    group_by(Metric) %>%
    mutate(NormalizedValue = (Value - min(Value, na.rm = TRUE)) / 
             (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(PanelTitle = paste(disease, Metric, sep = " "))
}

# Process datasets
polio_long <- process_data(polio_df, "Polio")
measles_long <- process_data(measles_df, "Measles")

# Combine datasets
combined_data <- bind_rows(polio_long, measles_long)

# Annotation data
annotations <- tibble(
  PanelTitle = c("Polio Cases", "Polio Deaths", "Measles Cases", "Measles Deaths"),
  AnnotationYear = c(1953, 1953, 1963, 1963),
  AnnotationLabel = c("Salk vaccine introduced", "Salk vaccine introduced", 
                      "Measles vaccine introduced", "Measles vaccine introduced")
)

# Create combined plot
combined_plot <- ggplot() +
  geom_tile(
    data = combined_data %>% filter(Metric == "Cases"), 
    aes(x = Year, y = 1, fill = NormalizedValue),
    color = "white", size = 0.2
  ) +
  scale_fill_gradient(low = "white", high = "purple", na.value = "lightgrey") +
  new_scale_fill() + # Allow a new fill scale
  geom_tile(
    data = combined_data %>% filter(Metric == "Deaths"), 
    aes(x = Year, y = 1, fill = NormalizedValue),
    color = "white", size = 0.2
  ) +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey") +
  scale_x_continuous(
    breaks = seq(floor(min(combined_data$Year) / 10) * 10, 
                 ceiling(max(combined_data$Year) / 10) * 10, by = 10),
    expand = c(0, 0)
  ) +
  geom_vline(data = annotations, aes(xintercept = AnnotationYear), color = "black", size = 0.8) +
  geom_text(data = annotations, aes(x = AnnotationYear + 0.5, y = 1.2, label = AnnotationLabel), 
            inherit.aes = FALSE, color = "black", hjust = 0, size = 4, family = "Lato") +
  facet_wrap(~PanelTitle, ncol = 1, strip.position = "top") +  # Each panel on its own line
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
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
ggsave(paste0(data_folder, "combined-vaccine-impact-US-stripe.svg"), combined_plot, width = 12, height = 12)
