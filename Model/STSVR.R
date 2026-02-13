X_sorted <- ready_data %>% arrange(time_date)
n_total <- nrow(X_sorted)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7, spatial_lag_1))
train_y <- train$accident_count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7, spatial_lag_1))
test_y <- test$accident_count


ggplot(data.frame(accidents = train_y), aes(x = accidents)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 0:max(train_y)) 

### ksvm
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
    Actual = accident_count)

