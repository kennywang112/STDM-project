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


results <- mclapply(
  list(
    list(x = train_X, y = train_y, test = test_X),
    list(x = train_X_t, y = train_y, test = test_X_t)
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
    Predicted_stsvr = predictions_st,
    Predicted_tsvr = predictions_t,
    mse_stsvr = (accident_count - predictions_st)^2,
    mse_tsvr = (accident_count - predictions_t)^2
    )

test_results_svr%>%write.csv("./Data/CalculatedData/test_results_stsvr.csv")
test_results_svr <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")
