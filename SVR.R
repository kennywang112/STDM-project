# Extended EDA for temporal patterns, this need to change because it only consider temporal

library(kernlab)

temp_data_lag <- temp_data %>%
  st_drop_geometry()%>%
  arrange(month_year) %>% 
  mutate(t = count, 
         t_minus_1 = lag(count, n = 1),
         t_minus_7 = lag(count, n = 7))%>%
  filter(!is.na(t_minus_1) & !is.na(t_minus_7))

X <- as.matrix(temp_data_lag %>% select(t_minus_1, t_minus_7))
y <- temp_data_lag$count
C <- 1
epsilon <- 0.1
model_svr <- ksvm(X, y, type="eps-svr", kernel="vanilladot", C=C, epsilon=epsilon)

prediction <- predict(model_svr, X)

temp_data_lag%>%
  ggplot(aes(x=month_year)) +
  geom_line(aes(y=count), color="blue", size=1) +
  geom_line(aes(y=prediction), color="red", size=1) +
  labs(title="Kernlab ksvm Prediction", y="Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Train SVR
smp_size <- floor(0.8 * nrow(temp_data_lag))
set.seed(123)
train_ind <- sample(seq_len(nrow(temp_data_lag)), size = smp_size)

train <- temp_data_lag[train_ind, ]
test <- temp_data_lag[-train_ind, ]
train_X <- as.matrix(train %>% select(t_minus_1, t_minus_7))
train_y <- train$count
test_X <- as.matrix(test %>% select(t_minus_1, t_minus_7))
test_y <- test$count

library(caret)
ctrl <- trainControl(method = "cv", number=5) 
SVRGrid <- expand.grid(.sigma=c(0.001, 0.005, 0.01, 0.02), .C=c(10,100,1000))
SVRFit <- caret::train(train_X, train_y, method="svmRadial", tuneGrid=SVRGrid, trControl=ctrl, type="eps-svr")
SVRFit
plot(SVRFit)
SVRFit$finalModel

pred_y <- predict(SVRFit, test_X)
plot(test_y, type="l", xaxt="n", xlab="Date", ylab="UKTemp")
points(pred_y, col="blue", pch=21, bg="blue")

svr_res <- test_y - pred_y
plot(svr_res)
acf(svr_res)