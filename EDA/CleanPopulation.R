library(readxl)

pop_raw_09 <- read_excel("Data/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls", sheet = "Mid-2009")
pop_raw_10 <- read_excel("Data/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls", sheet = "Mid-2010")

pop_raw_11 <- read_excel("Data/sapelsoasyoa20112014.xlsx", sheet = "Mid-2011 LSOA 2021", skip = 3)
pop_raw_12 <- read_excel("Data/sapelsoasyoa20112014.xlsx", sheet = "Mid-2012 LSOA 2021", skip = 3)
pop_raw_13 <- read_excel("Data/sapelsoasyoa20112014.xlsx", sheet = "Mid-2013 LSOA 2021", skip = 3)
pop_raw_14 <- read_excel("Data/sapelsoasyoa20112014.xlsx", sheet = "Mid-2014 LSOA 2021", skip = 3)

pop_raw_15 <- read_excel("Data/sapelsoasyoa20152018.xlsx", sheet = "Mid-2015 LSOA 2021", skip = 3)
pop_raw_16 <- read_excel("Data/sapelsoasyoa20152018.xlsx", sheet = "Mid-2016 LSOA 2021", skip = 3)
pop_raw_17 <- read_excel("Data/sapelsoasyoa20152018.xlsx", sheet = "Mid-2017 LSOA 2021", skip = 3)
pop_raw_18 <- read_excel("Data/sapelsoasyoa20152018.xlsx", sheet = "Mid-2018 LSOA 2021", skip = 3)

pop_raw_19 <- read_excel("Data/sapelsoasyoa20192022.xlsx", sheet = "Mid-2019 LSOA 2021", skip = 3)
pop_raw_20 <- read_excel("Data/sapelsoasyoa20192022.xlsx", sheet = "Mid-2020 LSOA 2021", skip = 3)
pop_raw_21 <- read_excel("Data/sapelsoasyoa20192022.xlsx", sheet = "Mid-2021 LSOA 2021", skip = 3)
pop_raw_22 <- read_excel("Data/sapelsoasyoa20192022.xlsx", sheet = "Mid-2022 LSOA 2021", skip = 3)

pop_raw_23 <- read_excel("Data/sapelsoasyoa20222024.xlsx", sheet = "Mid-2023 LSOA 2021", skip = 3)
pop_raw_24 <- read_excel("Data/sapelsoasyoa20222024.xlsx", sheet = "Mid-2024 LSOA 2021", skip = 3)

clean_old <- function(df, year) {
  df %>%
    select(lsoa21cd = LSOA11CD, population = all_ages) %>%
    mutate(Year = year)
}

clean_new <- function(df, year) {
  df %>%
    select(lsoa21cd = `LSOA 2021 Code`, population = Total) %>%
    mutate(Year = year)
}

pop_09_10 <- list(
  clean_old(pop_raw_09, 2009),
  clean_old(pop_raw_10, 2010)
)

pop_11_24 <- list(
  clean_new(pop_raw_11, 2011), clean_new(pop_raw_12, 2012),
  clean_new(pop_raw_13, 2013), clean_new(pop_raw_14, 2014),
  clean_new(pop_raw_15, 2015), clean_new(pop_raw_16, 2016),
  clean_new(pop_raw_17, 2017), clean_new(pop_raw_18, 2018),
  clean_new(pop_raw_19, 2019), clean_new(pop_raw_20, 2020),
  clean_new(pop_raw_21, 2021), clean_new(pop_raw_22, 2022),
  clean_new(pop_raw_23, 2023), clean_new(pop_raw_24, 2024)
)

london_pop_2009_2024 <- bind_rows(c(pop_09_10, pop_11_24))

saveRDS(london_pop_2009_2024, file = "./Data/london_population_2009_2024.rds")
