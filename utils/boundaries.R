library(sf)
library(purrr)
shp_path <- "./Data/LB_shp"
shp_files <- list.files(path = shp_path, pattern = "\\.shp$", full.names = TRUE)
london_lsoa <- map_dfr(shp_files, st_read, quiet = TRUE) 
  
london_msoa_geom <- london_lsoa %>%
  group_by(msoa21cd) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

london_lad_geom <- london_lsoa %>%
  group_by(lad22cd) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# This is the new on for testing england
# london_lad_geom <- st_read('./Data/Local_Authority_Districts_May_2024_Boundaries_UK_BFE_5988953988717086591.csv')
