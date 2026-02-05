library(lubridate)
library(sf)
library(spdep)

# ST
nb_list <- poly2nb(london_lsoa)
W_list <- nb2listw(nb_list, style = "W", zero.policy = TRUE)

ordered_lsoa_ids <- london_lsoa$lsoa21cd

# filter data after 2000
lsoa_daily_counts <- accidents_lsoa %>%
  st_drop_geometry() %>%
  mutate(date_parsed = dmy(date), day_date = floor_date(date_parsed, "month")) %>%
  group_by(lsoa21cd, day_date) %>%
  summarise(count = n(), .groups = "drop")%>%
  filter(day_date >= as.Date("2020-01-01"))

final_data <- lsoa_daily_counts
# empty panel data
all_dates <- seq(min(final_data$day_date), max(final_data$day_date), by="month")
panel_data <- expand.grid(lsoa21cd = ordered_lsoa_ids, day_date = all_dates) %>%
  left_join(final_data, by = c("lsoa21cd", "day_date")) %>%
  mutate(count = replace_na(count, 0)) %>%
  arrange(day_date, match(lsoa21cd, ordered_lsoa_ids))

calc_spatial_daily <- function(df_subset, w_list) {
  return(lag.listw(w_list, df_subset$count, zero.policy = TRUE))
}
# spatial_lag_t = accidents in neighbors / neighbors
panel_data_spatial <- panel_data %>%
  group_by(day_date) %>%
  mutate(spatial_lag_t = calc_spatial_daily(cur_data(), W_list)) %>%
  ungroup()

panel_data_spatial%>%
  group_by(count, spatial_lag_t)%>%
  summarise(ct = n())%>%
  arrange(desc(ct))

svr_ready_data <- panel_data_spatial %>%
  arrange(lsoa21cd, day_date) %>% 
  group_by(lsoa21cd) %>%
  mutate(
    t = count,
    t_minus_1 = lag(count, 1), 
    t_minus_7 = lag(count, 7),
    spatial_lag_1 = lag(spatial_lag_t, 1) 
  ) %>%
  ungroup() %>%
  filter(!is.na(t_minus_1) & !is.na(t_minus_7) & !is.na(spatial_lag_1))


X <- svr_ready_data
svr_ready_data%>%select(t_minus_1, t_minus_7, spatial_lag_1)%>%dim()
y <- svr_ready_data$t

set.seed(123)
smp_size <- floor(0.8 * nrow(X))
sample_idx <- sample(seq_len(nrow(X)), size = smp_size)

train <- X[sample_idx[1:100000], ]
test <- X[-sample_idx, ]
train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$count

library(kernlab)
model_svr_st <- ksvm(x = train_X, 
                     y = train_y, 
                     type = "eps-svr", 
                     kernel = "rbfdot",
                     C = 1, 
                     epsilon = 0.1)

predictions <- predict(model_svr_st, test_X)

plot_data <- data.frame(
  Actual = test_y[1:100],
  Predicted = predictions[1:100]
)

ggplot(plot_data, aes(x = 1:100)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "SVR with Spatial Lag (Sampled Test)", x = "Index", y = "Accidents") +
  theme_minimal()
