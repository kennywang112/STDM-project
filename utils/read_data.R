library(tidyverse)
library(readxl)
source('./utils/boundaries.R')

accident <- read_csv('./Data/dft-road-casualty-statistics-collision-1979-latest-published-year.csv') %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(27700)
accidents_joined <- st_join(accident, london_lsoa, left = FALSE)%>%
  filter(collision_year >= 2020)

pop_raw <- read_excel("Data/sapelsoasyoa20222024.xlsx", sheet = "Mid-2024 LSOA 2021", skip = 3) # Population in LSOA

lsoa_pop <- pop_raw %>% select(lsoa21cd = `LSOA 2021 Code`, population = Total)

lsoa_lookup <- st_drop_geometry(london_lsoa) %>% 
  select(lsoa21cd, msoa21cd, lad22cd) %>% 
  distinct()

read_final_data <- function(
    analysis_level = "MSOA", time_scale = "month"
    ) {

  if (analysis_level == "LAD") {
    
    lad_pop <- lsoa_pop %>%
      left_join(lsoa_lookup, by = "lsoa21cd") %>%
      group_by(lad22cd) %>%
      summarise(population = sum(population, na.rm = TRUE))
    
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      count(lad22cd, time_date, name = "accident_count")
    
    final_data <- london_lad_geom %>%
      left_join(accidents_agg, by = "lad22cd") %>%
      left_join(lad_pop, by = "lad22cd") %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return_data <- list(lad_pop, final_data)
    return(return_data)
    
  } else if (analysis_level == "MSOA") {
    
    msoa_pop <- lsoa_pop %>%
      left_join(lsoa_lookup, by = "lsoa21cd") %>%
      group_by(msoa21cd) %>%
      summarise(population = sum(population, na.rm = TRUE))
  
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      count(msoa21cd, time_date, name = "accident_count")
  
    final_data <- london_msoa_geom %>%
      left_join(accidents_agg, by = "msoa21cd") %>%
      left_join(msoa_pop, by = "msoa21cd") %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return_data <- list(msoa_pop, final_data)
    return(return_data)
    
  } else if (analysis_level == "LSOA") {
    
    accidents_agg <- accidents_joined %>%
      st_drop_geometry() %>%
      mutate(day_date = dmy(date)) %>%
      mutate(time_date = floor_date(day_date, unit = time_scale)) %>%
      count(lsoa21cd, time_date, name = "accident_count")
  
    final_data <- london_lsoa %>%
      select(lsoa21cd, msoa21cd, geometry) %>%
      left_join(accidents_agg, by = "lsoa21cd") %>%
      left_join(lsoa_pop, by = "lsoa21cd") %>%
      mutate(accident_count = replace_na(accident_count, 0))
    
    return(final_data)
  }

}

# code_list_df <- read_xlsx('./Data/dft-road-casualty-statistics-road-safety-open-dataset-data-guide-2024.xlsx', sheet = "2024_code_list")
# collision_codes <- code_list_df %>%
#   # filter(tolower(table) == 'collision') %>%
#   filter(!is.na(`code/format`) & !is.na(label))
# 
# 
# mapped_df <- accident

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