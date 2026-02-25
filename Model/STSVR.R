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
  mc.cores = 6
)

predictions_st <- results[[1]]
predictions_t <- results[[2]]

test_results <- test %>%
  mutate(
    Predicted = predictions_st,
    Predicted_t = predictions_t,
    Actual = accident_count)

test_results%>%write.csv("./Data/CalculatedData/test_results_stsvr.csv")
test_results <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")
