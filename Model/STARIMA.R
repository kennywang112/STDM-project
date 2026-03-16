library(forecast)
source('utils/starima_package.R')

Z_matrix_df <- ready_data %>%
  select(time_date, !!sym(cscale), accident_count) %>%
  pivot_wider(names_from = !!sym(cscale), values_from = accident_count, values_fill = 0) %>%
  arrange(time_date)

time_dates <- Z_matrix_df$time_date
Z_matrix <- as.matrix(Z_matrix_df %>% select(-time_date))

train_idx <- which(time_dates < test_start_date)
test_idx <- which(time_dates >= test_start_date)

train_Z <- Z_matrix[train_idx, ]
test_Z <- Z_matrix[test_idx, ]

W_mat <- list(listw2mat(W_list))

train_total_series <- rowSums(train_Z)
test_total_series  <- rowSums(test_Z)
n_ahead <- length(test_total_series)

train_ts <- ts(train_total_series, frequency = 12)
fit_total <- auto.arima(train_ts, stepwise = TRUE, approximation = TRUE)
print(summary(fit_total))

best_orders <- arimaorder(fit_total)
best_p <- unname(best_orders["p"])
best_d <- unname(best_orders["d"])
best_q <- unname(best_orders["q"])

############################# STARIMA
fit.star <- starima_fit(Z = train_Z, W = W_mat, p = best_p, d = best_d, q = best_q)
pre.star <- starima_pre(test_Z, model = fit.star)

pred_mat_starima <- as.matrix(pre.star$PRE)
actual_mat <- as.matrix(test_Z)
n_rows <- min(nrow(actual_mat), nrow(pred_mat_starima))

############################## ARIMA
N_nodes <- ncol(train_Z)
pred_arima_mat_full <- matrix(NA, nrow = n_ahead, ncol = N_nodes)
colnames(pred_arima_mat_full) <- colnames(train_Z)

for (i in 1:N_nodes) {
  region_train <- train_Z[, i]
  region_ts <- ts(region_train, frequency = 12)
  tryCatch({
    fit_region <- Arima(region_ts, order = c(best_p, best_d, best_q))
    pre_region <- forecast(fit_region, h = n_ahead)
    pred_arima_mat_full[, i] <- as.numeric(pre_region$mean)
  }, error = function(e) {
    fit_naive <- naive(region_ts, h = n_ahead)
    pred_arima_mat_full[, i] <- as.numeric(fit_naive$mean)
  })
}

pred_arima_mat <- tail(pred_arima_mat_full, n_rows)
pred_starima_mat_aligned <- tail(pred_mat_starima, n_rows)
actual_mat_aligned <- tail(actual_mat, n_rows)

############################# manual ARIMA
# train_total_series <- rowSums(train_Z)
# test_total_series  <- rowSums(test_Z)
# fit_total <- arima(train_total_series, order = c(1, 0, 0))
# n_ahead <- length(test_total_series)
# pre.ar <- predict(fit_total, n.ahead = n_ahead)
# 
# N_nodes <- ncol(train_Z)
# 
# predict_arima_total <- as.numeric(pre.ar$pred)
# predict_arima_per_region <- predict_arima_total / N_nodes
# 
# pred_arima_mat_full <- matrix(
#   rep(predict_arima_per_region, times = N_nodes),
#   nrow = n_ahead,
#   ncol = N_nodes
# )
# colnames(pred_arima_mat_full) <- colnames(train_Z)
# 
# pred_arima_mat <- tail(pred_arima_mat_full, n_rows)
# pred_starima_mat_aligned <- tail(pred_mat_starima, n_rows)
# actual_mat_aligned <- tail(actual_mat, n_rows)

############### Calculate MSE for both models
diff_arima_mat <- actual_mat_aligned - pred_arima_mat
arima_mse_vec <- colMeans(diff_arima_mat^2, na.rm = TRUE)

diff_starima_mat <- actual_mat_aligned - pred_starima_mat_aligned
star_mse_vec <- colMeans(diff_starima_mat^2, na.rm = TRUE)

all_dates <- ready_data %>% arrange(time_date) %>% pull(time_date) %>% unique()
test_dates <- tail(all_dates, n_rows)
region_ids <- colnames(test_Z)

pred_starima_df <- as.data.frame(pred_starima_mat_aligned)
colnames(pred_starima_df) <- region_ids
pred_starima_df$time_date <- as.character(test_dates)

################ create msoa predict
pred_starima_long <- pred_starima_df %>%
  pivot_longer(cols = -time_date, names_to = "msoa21cd", values_to = "Predicted_starima") %>%
  mutate(msoa21cd = as.character(msoa21cd))

pred_arima_df <- as.data.frame(pred_arima_mat)
colnames(pred_arima_df) <- region_ids
pred_arima_df$time_date <- as.character(test_dates)

pred_arima_long <- pred_arima_df %>%
  pivot_longer(cols = -time_date, names_to = "msoa21cd", values_to = "Predicted_arima") %>%
  mutate(msoa21cd = as.character(msoa21cd))

test_results_starima <- test %>%
  mutate(
    time_date_char = as.character(time_date),
    msoa21cd_char = as.character(msoa21cd)
  ) %>%
  left_join(pred_starima_long, by = c("time_date_char" = "time_date", "msoa21cd_char" = "msoa21cd")) %>%
  left_join(pred_arima_long, by = c("time_date_char" = "time_date", "msoa21cd_char" = "msoa21cd")) %>%
  select(-time_date_char, -msoa21cd_char) %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    mse_starima = (real_actual - Predicted_starima)^2,
    mse_arima = (real_actual - Predicted_arima)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

test_results_starima%>%filter(!is.na(mse_arima))

test_results_starima %>% write.csv("./Data/CalculatedData/test_results_starima.csv", row.names = FALSE)
test_results_starima <- read.csv("./Data/CalculatedData/test_results_starima.csv")

