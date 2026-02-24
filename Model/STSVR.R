X_sorted <- ready_data %>% arrange(time_date)
n_total <- nrow(X_sorted)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$accident_count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$accident_count

train_X_t <- as.matrix(train%>%select(t_minus_1, t_minus_7))
test_X_t <- as.matrix(test%>%select(t_minus_1, t_minus_7))

ggplot(data.frame(accidents = train_y), aes(x = accidents)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:max(train_y)) 

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

# test_results%>%write.csv("./Data/CalculatedData/test_results_stsvr.csv")
# test_results <- read.csv("./Data/CalculatedData/test_results_stsvr.csv")
