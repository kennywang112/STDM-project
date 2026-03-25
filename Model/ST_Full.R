library(spdep)
library(lubridate)
library(tidyverse)
source('utils/read_data.R')

time_scale <- "week"
rt <- readRDS("./Data/spatial_data_week.rds")
final_data <- rt[[2]]

cscale <- 'msoa21cd'
if (cscale == 'lad22cd') {
  london_geom <- london_lad_geom
  pop_ref <- rt[[1]]
} else if (cscale == 'msoa21cd') {
  london_geom <- london_msoa_geom
  pop_ref <- rt[[1]]
} else if (cscale == 'lsoa21cd') {
  london_geom <- london_lsoa
  pop_ref <- rt[[1]]
}

nb_list <- poly2nb(london_geom, queen = TRUE)
W_list <- nb2listw(nb_list, style = "W", zero.policy = TRUE)
ordered_ids <- london_geom[, cscale][[1]]

min_date <- floor_date(min(final_data$time_date, na.rm=TRUE), time_scale)
max_date <- floor_date(max(final_data$time_date, na.rm=TRUE), time_scale)
all_dates <- seq(min_date, max_date, by = time_scale)

grid_list <- setNames(list(ordered_ids), cscale)
panel_data <- expand.grid(c(grid_list, list(time_date = all_dates))) %>%
  left_join(st_drop_geometry(final_data) %>% 
              select(all_of(cscale), time_date, population, avg_rainfall, accident_count), 
            by = c(cscale, "time_date")) %>%
  mutate(
    accident_count = replace_na(accident_count, 0),
    population = replace_na(population, median(population, na.rm = TRUE)),
    avg_rainfall = replace_na(avg_rainfall, 0)
  )

panel_data_spatial <- panel_data %>%
  group_by(time_date) %>%
  mutate(spatial_lag_t = lag.listw(W_list, accident_count, zero.policy = TRUE)) %>%
  ungroup()

ready_data <- panel_data_spatial %>%
  arrange(.data[[cscale]], time_date) %>% 
  group_by(.data[[cscale]]) %>%
  mutate(
    t = accident_count,
    t_minus_1  = lag(accident_count, 1), 
    t_minus_2  = lag(accident_count, 2),
    t_minus_3  = lag(accident_count, 3),
    t_minus_52 = lag(accident_count, 52),
    spatial_lag_1 = lag(spatial_lag_t, 1),
    pop_t = population,
    rain_t = avg_rainfall
  ) %>%
  ungroup()

test_start_date <- as.Date("2023-01-01")
val_start_date <- as.Date("2021-01-01")

train_raw_ref <- ready_data %>% filter(time_date < test_start_date)

acc_min <- min(train_raw_ref$accident_count, na.rm = TRUE)
acc_max <- max(train_raw_ref$accident_count, na.rm = TRUE)
pop_min <- min(train_raw_ref$pop_t, na.rm = TRUE)
pop_max <- max(train_raw_ref$pop_t, na.rm = TRUE)
rain_min <- min(train_raw_ref$rain_t, na.rm = TRUE)
rain_max <- max(train_raw_ref$rain_t, na.rm = TRUE)

ready_data_scaled <- ready_data %>%
  mutate(
    across(c(accident_count, t_minus_1, t_minus_2, t_minus_3, t_minus_52, spatial_lag_1), 
           ~ (. - acc_min) / (acc_max - acc_min)),
    pop_t  = (pop_t - pop_min) / (pop_max - pop_min),
    rain_t = (rain_t - rain_min) / (rain_max - rain_min)
  )%>%
  mutate(across(everything(), ~replace_na(., 0)))

test <- ready_data_scaled %>% filter(time_date >= test_start_date)
train_trad <- ready_data_scaled %>% filter(time_date < test_start_date)
train_dl <- ready_data_scaled %>% filter(time_date < val_start_date)
val_dl <- ready_data_scaled %>% filter(time_date >= val_start_date & time_date < test_start_date)

feature_cols <- c("t_minus_1", "t_minus_2", "t_minus_3", "t_minus_52", "spatial_lag_1", "pop_t", "rain_t")

train_X <- as.matrix(train_trad %>% select(all_of(feature_cols)))
test_X <- as.matrix(test %>% select(all_of(feature_cols)))
train_X_t <- as.matrix(train_trad %>% select(all_of(feature_cols))%>%select(-spatial_lag_1))
test_X_t <- as.matrix(test %>% select(all_of(feature_cols))%>%select(-spatial_lag_1))

train_y <- train_trad$accident_count
test_y <- test$accident_count

source('Model/STSVR.R')
source('Model/STARIMA.R')
gc()
source('Model/TGCN.R')
gc()
source('Model/LSTMGNN.R')
gc()
source('Model/STGCN.R')

test_results_all <- test %>%
  mutate(accident_count = accident_count * (acc_max - acc_min) + acc_min) %>%
  mutate(time_date = as.character(time_date)) %>%
  left_join(
    test_results_svr %>%
      mutate(time_date = as.character(time_date)) %>%
      select(time_date, msoa21cd, Predicted_stsvr, Predicted_tsvr, mse_stsvr, mse_tsvr),
    by = c("time_date", "msoa21cd")
  ) %>%
  left_join(
    test_results_starima %>%
      mutate(time_date = as.character(time_date)) %>%
      select(time_date, msoa21cd, Predicted_starima, Predicted_arima, mse_starima, mse_arima),
    by = c("time_date", "msoa21cd")
  ) %>%
  left_join(
    test_results_tgcn %>%
      mutate(time_date = as.character(time_date)) %>%
      select(time_date, msoa21cd, Predicted_ann, Predicted_tgcn, mse_ann, mse_tgcn),
    by = c("time_date", "msoa21cd")
  ) %>%
  left_join(
    test_results_gcnlstm %>%
      mutate(time_date = as.character(time_date)) %>%
      select(time_date, msoa21cd, Predicted_gcn_lstm, mse_gcn_lstm),
    by = c("time_date", "msoa21cd")
  ) %>%
  left_join(
    test_results_stgcn %>%
      mutate(time_date = as.character(time_date)) %>%
      select(time_date, msoa21cd, Predicted_stgcn, mse_stgcn),
    by = c("time_date", "msoa21cd")
  ) %>%
  mutate(time_date = as.Date(time_date))%>%
  filter(!is.na(Predicted_starima))%>% # add geometry
  left_join(
    london_msoa_geom %>% select(msoa21cd, geometry),
    by = "msoa21cd"
  )

saveRDS(test_results_all, file = "./Data/CalculatedData/test_results_ALL_MODELS.rds")
test_results_all <- readRDS("./Data/CalculatedData/test_results_ALL_MODELS.rds")


