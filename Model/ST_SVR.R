library(spdep)
library(lubridate)
source('utils/read_data.R')

nb_list_msoa <- poly2nb(london_msoa_geom, queen = TRUE)
W_list <- nb2listw(nb_list_msoa, style = "W", zero.policy = TRUE)

ordered_msoa_ids <- london_msoa_geom$msoa21cd

time_scale <- "month"
final_data <- read_final_data('MSOA', time_scale)
final_data <- final_data %>% filter(time_date >= as.Date("2020-01-01"))
# empty panel data
min_date <- floor_date(min(final_data$time_date, na.rm=TRUE), time_scale)
max_date <- floor_date(max(final_data$time_date, na.rm=TRUE), time_scale)
all_dates <- seq(min_date, max_date, by = time_scale)

panel_data <- expand.grid(msoa21cd = ordered_msoa_ids, time_date = all_dates) %>%
  left_join(st_drop_geometry(final_data), by = c("msoa21cd", "time_date")) %>%
  mutate(accident_count = replace_na(accident_count, 0)) %>%
  arrange(time_date, match(msoa21cd, ordered_msoa_ids))

calc_spatial_daily <- function(df_subset, w_list) {
  return(lag.listw(w_list, df_subset$count, zero.policy = TRUE))
}
# Add spatial lag
# spatial_lag_t = accidents in neighbors / neighbors
panel_data_spatial <- panel_data %>%
  group_by(time_date) %>%
  mutate(spatial_lag_t = lag.listw(W_list, accident_count, zero.policy = TRUE)) %>%
  ungroup()

panel_data_spatial%>%
  group_by(accident_count, spatial_lag_t)%>%
  summarise(ct = n())%>%
  arrange(desc(ct))

# Add temporal lag
svr_ready_data <- panel_data_spatial %>%
  arrange(msoa21cd, time_date) %>% 
  group_by(msoa21cd) %>%
  mutate(
    t = accident_count,
    t_minus_1 = lag(accident_count, 1), 
    t_minus_7 = lag(accident_count, 7),
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

train <- X[sample_idx, ]
test <- X[-sample_idx, ]
train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$accident_count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$accident_count

train%>%dim()
test_X%>%dim()

ggplot(data.frame(accidents = train_y), aes(x = accidents)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:max(train_y)) 

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
  group_by(msoa21cd) %>%
  summarise(total = sum(Actual)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(msoa21cd)

plot_subset <- test_results %>%
  filter(msoa21cd %in% top_msoa)

ggplot(plot_subset, aes(x = time_date)) +
  geom_line(aes(y = Actual, group = msoa21cd, color = msoa21cd), size = 0.5) +
  geom_line(aes(y = Predicted, group = msoa21cd, color = msoa21cd), linetype = "dashed", size = 0.8) +
  facet_wrap(~msoa21cd, scales = "free_y", ncol = 1) +
  theme_minimal()
