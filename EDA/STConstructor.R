# This method is to reproduce Emerging Hotspot Analysis based on arcgis, but won't include all 17 features
# https://pro.arcgis.com/en/pro-app/latest/tool-reference/space-time-pattern-mining/emerginghotspots.htm
library(Kendall)
library(tmap)
library(spdep)
source('utils/read_data.R')
source('utils/map_func.R')

time_scale <- "month"
rt_year <- read_final_data('MSOA', time_scale)

final_data <- rt_year[[2]]

cscale <- 'msoa21cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- rt_year[[1]]
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- rt_year[[1]]
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- rt_year[[1]]
}

# The original data is based on

unique_geom <- final_data %>%
  group_by(msoa21cd) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(msoa21cd)

ordered_msoa <- unique_geom$msoa21cd

nb <- poly2nb(unique_geom, queen = TRUE)
nb_self <- include.self(nb) 
lw <- nb2listw(nb_self, style = "W", zero.policy = TRUE)

gi_data <- final_data %>%
  st_drop_geometry() %>%
  complete(msoa21cd = ordered_msoa, time_date, fill = list(accident_count = 0)) %>%
  arrange(time_date, match(msoa21cd, ordered_msoa)) %>%
  group_by(time_date) %>%
  mutate(
    Gi_star = as.numeric(localG(accident_count, listw = lw, zero.policy = TRUE))) %>%
  ungroup()


mk_results <- gi_data %>%
  arrange(msoa21cd, time_date) %>%
  group_by(msoa21cd) %>%
  summarise(
    mk_tau = MannKendall(Gi_star)$tau[1],
    mk_pvalue = MannKendall(Gi_star)$sl[1],
    .groups = 'drop') %>%
  mutate(
    trend_category = case_when(
      mk_pvalue < 0.05 & mk_tau > 0 ~ "Hotter (Significant Increase)",
      mk_pvalue < 0.05 & mk_tau < 0 ~ "Colder (Significant Decrease)",
      TRUE ~ "No Significant Trend"
    ))


final_trend_map <- unique_geom %>%
  select(msoa21cd, geometry) %>%
  left_join(mk_results, by = "msoa21cd")

final_trend_map$trend_category%>%table()

library(tmap)
tmap_mode("plot")
ehsa_map <- tm_shape(final_trend_map) +
  tm_polygons("trend_category", palette = c("red", "blue", "lightgrey"), title = "Trend Category") +
  tm_layout(main.title = "Emerging Hotspot Analysis (Mann-Kendall Trend)", legend.outside = TRUE) +
  add_map_decorations()

tmap_save(ehsa_map, filename = paste0("Data/Layout/EHSA_",cscale,".png", sep=''), width = 6, height = 6, dpi = 300)


final_trend_map