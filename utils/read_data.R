library(tidyverse)
library(readxl)

accident <- read_csv('./Data/dft-road-casualty-statistics-collision-provisional-2025.csv')
code_list_df <- read_xlsx('./Data/dft-road-casualty-statistics-road-safety-open-dataset-data-guide-2024.xlsx', sheet = "2024_code_list")
pop_raw <- read_excel("Data/sapelsoasyoa20222024.xlsx", sheet = "Mid-2024 LSOA 2021", skip = 3)

lsoa_pop <- pop_raw %>%
  select(
    lsoa21cd = `LSOA 2021 Code`,
    population = Total 
  )

collision_codes <- code_list_df %>%
  # filter(tolower(table) == 'collision') %>%
  filter(!is.na(`code/format`) & !is.na(label))


mapped_df <- accident

for (col_name in colnames(mapped_df)) {
  
  original_values <- as.character(mapped_df[[col_name]])
  mapping <- collision_codes %>% filter(`field name` == col_name)
  
  # namevec: c('code' = 'label')
  lookup_vec <- setNames(mapping$label, as.character(mapping$`code/format`))
  mapped_values <- lookup_vec[original_values]
  
  matched_indices <- !is.na(mapped_values)
  
  if (any(matched_indices)) {
    mapped_df[[col_name]] <- as.character(mapped_df[[col_name]])
    mapped_df[[col_name]][matched_indices] <- mapped_values[matched_indices]
  }
}
mapped_df
