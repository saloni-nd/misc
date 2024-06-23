#########################################
#                 Mapping poverty with R
#                 via Milos Popovic
#                 2023/07/25
#                 Adapted by Saloni Dattani
#########################################
install.packages("remotes")
remotes::install_github(
  "dickoa/rgeoboundaries"
)
libs <- c(
  "tidyverse", "rgeoboundaries",
  "sf", "terra"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

# 1. GET INDIAN STATES

india_states <- rgeoboundaries::gb_adm1(
  "IND"
)

unzip("master.zip")

india_states <- sf::st_read(
  "India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp"
)

# 2. Global Gridded Relative Deprivation Index (GRDI), Version 1

unzip("povmap-grdi-v1-grdiv1-geotiff.zip")

grdi <- terra::rast("povmap-grdi-v1.tif")

# 3. CROP GRDI BY INDIAN SHAPEFILE

indian_states_4326 <- india_states |>
  sf::st_transform(
    4326
  )

india_grdi <- terra::crop(
  grdi,
  terra::vect(indian_states_4326),
  snap = "in",
  mask = T
)

# 4. AVERAGE GRDI WITH ZONAL STATISTICS

india_grdi_zs <- terra::zonal(
  india_grdi,
  terra::vect(
    indian_states_4326
  ),
  fun = "mean",
  na.rm = T
)

indian_states_grdi <- cbind(
  indian_states_4326,
  india_grdi_zs
)

names(indian_states_grdi)[2] <- "grdi"

# 5. MAP

indian_states_grdi <- indian_states_grdi %>%
  mutate(label = paste(State_Name, "\n", round(grdi, 1)))

# Create 10 equally spaced intervals from 0 to 100
bins <- seq(0, 100, by = 10)
# Generate labels for each interval
bin_labels <- paste(head(bins, -1), tail(bins, -1), sep = "-")

# Binning the GRDI data into specified equal intervals
indian_states_grdi$grdi_cat <- factor(cut(indian_states_grdi$grdi, breaks = bins, include.lowest = TRUE, labels = bin_labels), levels = bin_labels)


p1 <- ggplot() +
  geom_sf(
    data = indian_states_grdi,
    aes(fill = factor(grdi_cat)),  # Use factor to treat the GRDI categories as discrete
    color = "grey10",
    size = 0.25,
    alpha = 0.75
  ) +
  geom_sf_label(
    data = indian_states_grdi,
    aes(label = label),
    size = 2.5,
    color = "black",
    label.size = NA,
    alpha = 0.75
  ) +
  scale_fill_brewer(
    type = "seq",
    palette = "Blues",
    name = "GRDI (highest = most deprived)",  # Descriptive legend title
  ) +
  coord_sf(crs = 4326) +
  theme_void() +
  labs(
    title = "Poverty in India",
    subtitle = "Global Gridded Relative Deprivation Index (GRDI)",
    caption = "Data source: Global Gridded Relative Deprivation Index (GRDI), v1 (2010 - 2020)\nCenter for International Earth Science Information Network",
  ) +
  theme(
    legend.position = "right",
    plot.margin = unit(c(t = -3, r = 2, b = -3, l = 0.5), "lines"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0)
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)  # Reverse the order of the legend entries
  )

ggsave(
  "india-grdi.png", p1,
  width = 10, height = 12,
  units = "in", bg = "white"
)
