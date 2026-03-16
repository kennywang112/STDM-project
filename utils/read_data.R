library(tidyverse)
library(readxl)
source('./utils/boundaries.R')

# accident <- read_csv('./Data/dft-road-casualty-statistics-collision-1979-latest-published-year.csv') %>%
#   filter(collision_year >= 2010)%>%
#   filter(!is.na(longitude) & !is.na(latitude)) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# st_write(accident, "./Data/accident_2010.gpkg", delete_dsn = TRUE)
accident <- st_read("./Data/accident_2010.gpkg")

london_lsoa_4326 <- st_transform(london_lsoa, 4326)
accidents_joined <- st_join(accident, london_lsoa_4326, left = FALSE)%>%
  st_transform(27700)

lsoa_pop <- readRDS("./Data/london_population_2009_2024.rds")

lsoa_lookup <- st_drop_geometry(london_lsoa) %>% 
  select(lsoa21cd, msoa21cd, lad22cd) %>% 
  distinct()

read_final_data <- function(
    analysis_level = "MSOA", time_scale = "month"
    ) {

  if (analysis_level == "LAD") {
    
    lad_pop <- lsoa_pop %>%
      left_join(lsoa_lookup, by = "lsoa21cd") %>%
      group_by(lad22cd, Year) %>%
      summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      mutate(Year = year(time_date)) %>%
      count(lad22cd, time_date, Year, name = "accident_count")
    
    final_data <- london_lad_geom %>%
      left_join(accidents_agg, by = "lad22cd") %>%
      left_join(lad_pop, by = c("lad22cd", "Year")) %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return_data <- list(lad_pop, final_data)
    return(return_data)
    
  } else if (analysis_level == "MSOA") {
    
    msoa_pop <- lsoa_pop %>%
      left_join(lsoa_lookup, by = "lsoa21cd") %>%
      group_by(msoa21cd, Year) %>%
      summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
  
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      mutate(Year = year(time_date)) %>%
      count(msoa21cd, time_date, Year, name = "accident_count")
  
    final_data <- london_msoa_geom %>%
      left_join(accidents_agg, by = "msoa21cd") %>%
      left_join(msoa_pop, by = c("msoa21cd", "Year")) %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return_data <- list(msoa_pop, final_data)
    return(return_data)
    
  } else if (analysis_level == "LSOA") {
    
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      count(lsoa21cd, time_date, Year, name = "accident_count")
  
    final_data <- london_lsoa %>%
      select(lsoa21cd, msoa21cd, geometry) %>%
      left_join(accidents_agg, by = "lsoa21cd") %>%
      left_join(lsoa_pop, by = c("lsoa21cd", "Year")) %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return(final_data)
  }
}

# rt <- read_final_data('MSOA', time_scale)
# saveRDS(rt, file = "./Data/spatial_data.rds")

# library(sf)
# rainfall_data <- read_csv('./Data/daily_rainfall.csv')
# split_rainfall_data <- rainfall_data%>%
#   # splot ob_date to year, month from ex 2020-01-01
#   separate(ob_date, into = c("year", "month", "day"), sep = "-")%>%
#   mutate(
#     Year = as.numeric(year),
#     month = as.numeric(month),
#     day = as.numeric(day),
#     time_date = as.Date(paste(year, month, day, sep = "-")))%>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   st_transform(crs = st_crs(london_msoa_geom))
# 
# rain_msoa_agg <- st_join(split_rainfall_data, london_msoa_geom[,'msoa21cd']) %>%
#   st_drop_geometry() %>%
#   group_by(msoa21cd, time_date, Year) %>%
#   summarise(avg_rainfall = mean(rainfall_amount, na.rm = TRUE), .groups = "drop")
# 
# rt <- read_final_data('MSOA', 'week')
# rt[[2]] <- rt[[2]] %>%
#   left_join(rain_msoa_agg, by = c("msoa21cd", "time_date", "Year")) %>%
#   mutate(avg_rainfall = replace_na(avg_rainfall, 0))
# saveRDS(rt, file = "./Data/spatial_data_week.rds")

# code_list_df <- read_xlsx('./Data/dft-road-casualty-statistics-road-safety-open-dataset-data-guide-2024.xlsx', sheet = "2024_code_list")
# collision_codes <- code_list_df %>%
#   # filter(tolower(table) == 'collision') %>%
#   filter(!is.na(`code/format`) & !is.na(label))
# 
# 
# mapped_df <- accident
# 
# for (col_name in colnames(mapped_df)) {
# 
#   original_values <- as.character(mapped_df[[col_name]])
#   mapping <- collision_codes %>% filter(`field name` == col_name)
# 
#   # namevec: c('code' = 'label')
#   lookup_vec <- setNames(mapping$label, as.character(mapping$`code/format`))
#   mapped_values <- lookup_vec[original_values]
# 
#   matched_indices <- !is.na(mapped_values)
# 
#   if (any(matched_indices)) {
#     mapped_df[[col_name]] <- as.character(mapped_df[[col_name]])
#     mapped_df[[col_name]][matched_indices] <- mapped_values[matched_indices]
#   }
# }
# mapped_df