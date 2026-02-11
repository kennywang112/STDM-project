library(sf)
library(purrr)
shp_path <- "/Users/wangqiqian/Downloads/LB_shp"
shp_path <- "./Data/LB_shp"
shp_files <- list.files(path = shp_path, pattern = "\\.shp$", full.names = TRUE)
london_lsoa <- map_dfr(shp_files, st_read, quiet = TRUE) 


london_msoa_geom <- london_lsoa %>%
  group_by(msoa21cd) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

london_lad_geom <- london_lsoa %>%
  group_by(lad22cd) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
