library(spdep)
library(lubridate)
source('utils/read_data.R')
source('utils/starima_package.R')

# to not make the data too much
# week + LAD
# month + msoa
time_scale <- "week"
rt <- read_final_data('MSOA', time_scale)
final_data <- rt[[2]]

cscale <- 'msoa21cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- rt[[1]]
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- rt[[1]]
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- rt[[1]]
}


nb_list_msoa <- poly2nb(londona_geom, queen = TRUE)
W_list <- nb2listw(nb_list_msoa, style = "W", zero.policy = TRUE)
ordered_msoa_ids <- londona_geom[, cscale][[1]]

# empty panel data
min_date <- floor_date(min(final_data$time_date, na.rm=TRUE), time_scale)
max_date <- floor_date(max(final_data$time_date, na.rm=TRUE), time_scale)
all_dates <- seq(min_date, max_date, by = time_scale)

grid_list <- setNames(list(ordered_msoa_ids), cscale)
panel_data <- expand.grid(c(grid_list, list(time_date = all_dates))) %>%
  left_join(st_drop_geometry(final_data), by = c(cscale, "time_date")) %>%
  mutate(accident_count = replace_na(accident_count, 0)) %>%
  arrange(time_date, match(.data[[cscale]], ordered_msoa_ids))

calc_spatial_daily <- function(df_subset, w_list) {
  return(lag.listw(w_list, df_subset$count, zero.policy = TRUE))
}
# spatial_lag_t = accidents in neighbors / neighbors
panel_data_spatial <- panel_data %>%
  group_by(time_date) %>%
  # this is the spatial lag at time t
  mutate(spatial_lag_t = lag.listw(W_list, accident_count, zero.policy = TRUE)) %>%
  ungroup()

panel_data_spatial%>%
  group_by(accident_count, spatial_lag_t)%>%
  summarise(ct = n())%>%
  arrange(desc(ct))

# Add both temporal lag and spatial lag
ready_data <- panel_data_spatial %>%
  arrange(.data[[cscale]], time_date) %>% 
  group_by(.data[[cscale]]) %>%
  mutate(
    t = accident_count,
    t_minus_1 = lag(accident_count, 1), 
    t_minus_7 = lag(accident_count, 7),
    # this is the spatial lag at time t-1
    spatial_lag_1 = lag(spatial_lag_t, 1) 
  ) %>%
  ungroup() %>%
  filter(!is.na(t_minus_1) & !is.na(t_minus_7) & !is.na(spatial_lag_1))

source('Model/STSVR.R')
source('Model/STARIMA.R')
