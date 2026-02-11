library(spdep)
library(lubridate)
source('utils/read_data.R')

cscale <- 'lad22cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- lad_pop
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- msoa_pop
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- lsoa_pop
}

nb_list_msoa <- poly2nb(londona_geom, queen = TRUE)
W_list <- nb2listw(nb_list_msoa, style = "W", zero.policy = TRUE)
ordered_msoa_ids <- londona_geom[, cscale][[1]]

time_scale <- "week"
final_data <- read_final_data('LAD', time_scale)
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
svr_ready_data <- panel_data_spatial %>%
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

X_sorted <- svr_ready_data %>% arrange(time_date)
n_total <- nrow(X_sorted)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$accident_count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$accident_count


ggplot(data.frame(accidents = train_y), aes(x = accidents)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:max(train_y)) 

### ksvm
library(kernlab)
model_svr_st <- ksvm(x = train_X, 
                     y = train_y, 
                     type = "eps-svr", 
                     kernel = "rbfdot",
                     C = 1, 
                     epsilon = 0.1)

predictions <- predict(model_svr_st, test_X)

test_results <- test %>%
  mutate(
    Predicted = predictions, 
    Actual = accident_count
  )

top_msoa <- test_results %>%
  group_by(.data[[cscale]]) %>%
  summarise(total = sum(Actual)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(.data[[cscale]])

plot_subset <- test_results %>%
  filter(.data[[cscale]] %in% top_msoa)

ggplot(plot_subset, aes(x = time_date)) +
  geom_line(aes(y = Actual, group = .data[[cscale]], color = .data[[cscale]]), size = 0.5) +
  geom_line(aes(y = Predicted, group = .data[[cscale]], color = .data[[cscale]]), linetype = "dashed", size = 0.8) +
  facet_wrap(~.data[[cscale]], scales = "free_y", ncol = 1) +
  theme_minimal()
################### result analyse
analyse_final <- test_results%>%
  group_by(.data[[cscale]])%>%
  summarise(total_actual = sum(Actual), total_predicted = sum(Predicted))
analyse_final_all <- test_results%>%
  mutate(
    mse = (Actual - Predicted)^2,
    mae = abs(Actual - Predicted))%>%
  left_join(londona_geom, by = cscale)%>%
  st_as_sf()

predicted_line <- test_results%>%
  group_by(time_date)%>%
  summarise(total_actual = sum(Actual), total_predicted = sum(Predicted))%>%
  ggplot(aes(x = time_date)) +
  geom_line(aes(y = total_actual, color = "Actual"), size = 0.5) +
  geom_line(aes(y = total_predicted, color = "Predicted"), linetype = "dashed", size = 0.8) +
  theme_minimal() +
  labs(title = "Total Actual vs Predicted Accidents Over Time", y = "Total Accidents", color = "Legend")
ggsave(file = "Data/Layout/ST_SVR_result.png", predicted_line, width = 15, height = 6, dpi = 300)


mse_plot <- analyse_final_all%>%
  ggplot()+
  geom_line(aes(time_date, mse)) +
  theme_minimal() +
  labs(title = "MSE of SVR with Spatial Lag Over Time", x = "Time", y = "Mean Squared Error")
ggsave(file = "Data/Layout/ST_SVR_mse.png", mse_plot, width = 15, height = 6, dpi = 300)

### Residual Map
library(tmap)
spatial_error <- analyse_final_all %>%
  group_by(lad22cd) %>%
  summarise(
    avg_residual = mean(Actual - Predicted, na.rm = TRUE),
    geometry = st_union(geometry)) %>%
  st_as_sf()

error_map <- tm_basemap("CartoDB.Positron") +
  tm_shape(spatial_error) +
  tm_polygons(
    fill = "avg_residual",
    # fill.scale = tm_scale_intervals(
    #   values = "RdBu",
    #   midpoint = 0,
    #   style = "fixed",
    #   breaks = c(-Inf, -10, -5, -2, 2, 5, 10, Inf)),
    fill.legend = tm_legend(title = "Avg Residual (Actual - Predicted)")) +
  add_map_decorations()
tmap_save(error_map, filename = paste0("Data/Layout/residual_map_",cscale,".png", sep=''), width = 12, height = 6, dpi = 300)
