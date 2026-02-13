# This is extension in ST_SVR
Z_matrix <- ready_data %>%
  select(time_date, !!sym(cscale), accident_count) %>%
  pivot_wider(names_from = !!sym(cscale), values_from = accident_count, values_fill = 0) %>%
  arrange(time_date) %>%
  select(-time_date) %>%
  as.matrix()

n_time <- nrow(Z_matrix)
split_idx <- floor(0.8 * n_time)

train_Z <- Z_matrix[1:split_idx, ]
test_Z  <- Z_matrix[(split_idx + 1):n_time, ]
test_Z%>%dim()

W_mat <- list(listw2mat(W_list))

# This param need to be optimise
fit.star <- starima_fit(Z=train_Z, W=W_mat, p=1, d=0, q=0)
pre.star <- starima_pre(test_Z, model=fit.star)

actual_mat <- pre.star$OBS
predict_mat <- pre.star$PRE

predict_starima <- rowSums(pre.star$PRE)
actual_starima <- rowSums(test_Z)
mse_per_time <- rowMeans((actual_mat - predict_mat)^2, na.rm = TRUE)

