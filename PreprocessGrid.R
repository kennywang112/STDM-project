library(tidyverse)
library(sf)
library(tmap)
source('utils/boundaries.R')

accident <- read_csv('./Data/dft-road-casualty-statistics-collision-provisional-2025.csv') %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(27700)


england_sf_bng <- st_transform(england_sf, 27700)
unique(england_sf_bng$region)
london_bng <- england_sf_bng %>% filter(region == "Greater London")

accident_london <- st_filter(accident, london_bng)

tmap_mode("plot")
tm_shape(london_bng) +
  tm_borders() +
  tm_shape(accident_london) +
  tm_dots(size = 0.1, col = "red", alpha = 0.5)

# Create a 1km x 1km grid over London
grid_full <- st_make_grid(london_bng, cellsize = 300, square = TRUE) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(grid_id = row_number())

# Determine which grid cells intersect with London boundary
intersects_matrix <- st_intersects(grid_full, st_union(london_bng), sparse = FALSE)
grid_full$is_inside <- intersects_matrix[,1]

# Filter grid cells to keep only those that intersect with London
accident_in_grid <- st_join(accident_london, grid_full) %>%
  st_drop_geometry() %>%
  # count(grid_id, name = "accident_count")%>%
  group_by(grid_id) %>%
  summarise(accident_count = n())

unified_data <- grid_full %>%
  left_join(accident_in_grid, by = "grid_id") %>%
  mutate(accident_count = replace_na(accident_count, 0))

tm_shape(unified_data) +
  tm_polygons("accident_count", palette = "Reds", title = "Accident Count") +
  tm_shape(london_bng) +
  tm_borders()

spatial_data <- unified_data %>% 
  filter(is_inside == TRUE)
nb_list <- poly2nb(spatial_data)
