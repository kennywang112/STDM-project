### ksvm
library(kernlab)
library(parallel)

train_ksvm <- function(
    x, y, test
    ) {
  model <- ksvm(x = x, 
                y = y, 
                type = "eps-svr", 
                kernel = "rbfdot",
                C = 1, 
                epsilon = 0.1)
  
  predictions <- predict(model, test)
  
  return(predictions)
}

feature_cols <- c("t_minus_1", "t_minus_12", "spatial_lag_1", "pop_lag_1", "rain_lag_1")

# STSVR
train_X_5 <- as.matrix(train_trad %>% select(all_of(feature_cols)))
test_X_5 <- as.matrix(test %>% select(all_of(feature_cols)))
# TSVR
train_X_t_4 <- as.matrix(train_trad %>% select(t_minus_1, t_minus_12, pop_lag_1, rain_lag_1))
test_X_t_4 <- as.matrix(test %>% select(t_minus_1, t_minus_12, pop_lag_1, rain_lag_1))

results <- mclapply(
  list(
    list(x = train_X_5, y = train_y, test = test_X_5),   # STSVR
    list(x = train_X_t_4, y = train_y, test = test_X_t_4) # TSVR
  ),
  function(data) {
    train_ksvm(data$x, data$y, data$test)
  },
  mc.cores = 8
)

predictions_st <- results[[1]]
predictions_t <- results[[2]]

test_results_svr <- test %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    Predicted_stsvr = predictions_st * (acc_max - acc_min) + acc_min,
    Predicted_tsvr = predictions_t * (acc_max - acc_min) + acc_min,
    mse_stsvr = (real_actual - Predicted_stsvr)^2,
    mse_tsvr = (real_actual - Predicted_tsvr)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

test_results_svr%>%write.csv("./Data/CalculatedData/test_results_stsvr.csv")
test_results_svr <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")
