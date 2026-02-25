### ksvm
library(kernlab)

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

predictions_st <- train_ksvm(train_X, train_y, test_X)
predictions_t <- train_ksvm(train_X_t, train_y, test_X_t)

test_results <- test %>%
  mutate(
    Predicted = predictions_st,
    Predicted_t = predictions_t,
    Actual = accident_count)

test_results%>%write.csv("./Data/CalculatedData/test_results_stsvr.csv")
test_results <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")
