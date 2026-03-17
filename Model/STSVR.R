library(kernlab)
library(parallel)

train_ksvm <- function(x_mat, y_vec, test_mat, msoa_idx_train, msoa_idx_test) {
  
  unique_msoas <- unique(msoa_idx_test)
  all_predictions <- numeric(nrow(test_mat))
  
  for (m in unique_msoas) {
    train_rows <- which(msoa_idx_train == m)
    test_rows  <- which(msoa_idx_test == m)
    
    model <- try(ksvm(x = x_mat[train_rows, ], 
                      y = y_vec[train_rows], 
                      type = "nu-svr", 
                      kernel = "rbfdot",
                      C = 1, 
                      nu = 0.5,
                      scaled = FALSE), silent = TRUE)
    
    if (!inherits(model, "try-error")) {
      preds <- predict(model, test_mat[test_rows, , drop = FALSE])
      all_predictions[test_rows] <- as.numeric(preds)
    }
  }
  return(all_predictions)
}

train_msoa <- train_trad$msoa21cd
test_msoa <- test$msoa21cd

# STSVR
train_X_5 <- as.matrix(train_trad %>% select(all_of(feature_cols)))
test_X_5 <- as.matrix(test %>% select(all_of(feature_cols)))

# TSVR 
train_X_t_4 <- as.matrix(train_trad %>% select(all_of(feature_cols))%>%select(-spatial_lag_1))
test_X_t_4 <- as.matrix(test %>% select(all_of(feature_cols))%>%select(-spatial_lag_1))

results <- mclapply(
  list(
    list(x = train_X_5,   y = train_y, test = test_X_5),   # STSVR
    list(x = train_X_t_4, y = train_y, test = test_X_t_4)  # TSVR
  ),
  function(data) {
    train_ksvm(data$x, data$y, data$test, train_msoa, test_msoa)
  },
  mc.cores = 2
)

predictions_st <- results[[1]]
predictions_t <- results[[2]]

test_results_svr <- test %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    Predicted_stsvr = predictions_st * (acc_max - acc_min) + acc_min,
    Predicted_tsvr  = predictions_t * (acc_max - acc_min) + acc_min,
    mse_stsvr = (real_actual - Predicted_stsvr)^2,
    mse_tsvr  = (real_actual - Predicted_tsvr)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

write.csv(test_results_svr, "./Data/CalculatedData/test_results_stsvr.csv", row.names = FALSE)
test_results_svr <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")

## This is for permutation importance
# library(DALEX)
# set.seed(123)
# sample_idx <- sample(1:nrow(train_trad), 20000)
# train_X_sample <- as.matrix(train_trad[sample_idx, ] %>% select(all_of(feature_cols)))
# train_y_sample <- train_trad$accident_count[sample_idx]
# 
# explainer_model <- ksvm(x = train_X_sample, 
#                         y = train_y_sample, 
#                         type = "nu-svr", 
#                         kernel = "rbfdot",
#                         C = 1, 
#                         nu = 0.5,
#                         scaled = FALSE)
# 
# train_df_sample <- as.data.frame(train_trad[sample_idx, ] %>% select(all_of(feature_cols)))
# 
# pred_func <- function(model, newdata) {
#   newdata_mat <- as.matrix(newdata)
#   return(as.numeric(predict(model, newdata_mat)))
# }
# 
# explainer_svr <- explain(
#   model = explainer_model,
#   data = train_df_sample,
#   y = train_trad$accident_count[sample_idx],
#   predict_function = pred_func,
#   label = "Spatio-Temporal SVR"
# )
# 
# vip_svr <- model_parts(explainer_svr, loss_function = loss_root_mean_square)
# 
# fi <- plot(vip_svr) + 
#   ggtitle("Feature Importance: STSVR Model", "Measured by Dropout Loss (RMSE)")
# ggsave(filename = "./Data/Layout/Feature_Importance_STSVR.png", plot = fi, width = 6, height = 4, dpi = 300)
# 
# colnames(train_df_sample)
# pdp_rain <- model_profile(explainer_svr, variables = 'pop_t')
# plot(pdp_rain) + ggtitle("Partial Dependence Plot: Impact of Rainfall")
