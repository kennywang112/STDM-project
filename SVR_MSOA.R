library(lubridate)
library(sf)
library(spdep)

london_msoa_geom <- london_lsoa %>%
  group_by(msoa21cd) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
nb_list_msoa <- poly2nb(london_msoa_geom, queen = TRUE)
W_list <- nb2listw(nb_list_msoa, style = "W", zero.policy = TRUE)

ordered_msoa_ids <- london_msoa_geom$msoa21cd

accidents_with_msoa <- accidents_lsoa %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(london_lsoa) %>% select(lsoa21cd, msoa21cd), by = "msoa21cd") %>%
  filter(!is.na(msoa21cd))

msoa_daily_counts <- accidents_with_msoa %>%
  mutate(day_date = dmy(date)) %>%
  group_by(msoa21cd, day_date) %>%
  summarise(count = n(), .groups = "drop")

# ============== this is for checking
# msoa_total_check <- msoa_daily_counts %>%
#   group_by(msoa21cd) %>%
#   summarise(total_count = sum(count), .groups = "drop")
# 
# map_for_verification <- london_lsoa %>%
#   select(lsoa21cd, msoa21cd) %>%
#   left_join(msoa_total_check, by = "msoa21cd") %>%
#   mutate(total_count = replace_na(total_count, 0))
# 
# tm_shape(map_for_verification) +
#   tm_fill("total_count", palette = "Reds", title = "Total Accidents") +
#   tm_borders() +
#   tm_layout(title = "Total Accidents by MSOA (Joined Back to LSOA)")
# 
# 
# map_for_verification%>%dim()
# map_for_verification%>%
#   group_by(msoa21cd)%>%
#   summarise(n())%>%
#   dim()
# ============== 

final_data <- msoa_daily_counts
# empty panel data
all_dates <- seq(min(final_data$day_date), max(final_data$day_date), by="month")
panel_data <- expand.grid(msoa21cd = ordered_msoa_ids, day_date = all_dates) %>%
  left_join(final_data, by = c("msoa21cd", "day_date")) %>%
  mutate(count = replace_na(count, 0)) %>%
  arrange(day_date, match(msoa21cd, ordered_msoa_ids))

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
  arrange(msoa21cd, day_date) %>% 
  group_by(msoa21cd) %>%
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

train <- X[sample_idx, ]
test <- X[-sample_idx, ]
train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$count

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
    Actual = count
  )

top_msoa <- test_results %>%
  group_by(msoa21cd) %>%
  summarise(total = sum(Actual)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(msoa21cd)

plot_subset <- test_results %>%
  filter(msoa21cd %in% top_msoa)

ggplot(plot_subset, aes(x = day_date)) +
  geom_line(aes(y = Actual, group = msoa21cd, color = msoa21cd), size = 0.5) +
  geom_line(aes(y = Predicted, group = msoa21cd, color = msoa21cd), linetype = "dashed", size = 0.8) +
  facet_wrap(~msoa21cd, scales = "free_y", ncol = 1) +
  theme_minimal()
