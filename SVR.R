library(kernlab) 

source('utils/read_data.R')

# Extended EDA for temporal patterns, this need to change because it only consider temporal
temp_data

X <- temp_data$
y <- gauss1[,1]

svr_data <- temp_data %>%
  st_drop_geometry() %>%      
  arrange(month_year) %>%     
  mutate(
    lag_1 = lag(count, 1),     
    lag_2 = lag(count, 2),     
    day_num = as.numeric(month_year)
  ) %>%
  na.omit()

X <- as.matrix(svr_data %>% select(lag_1, lag_2, day_num))
y <- svr_data$count
C <- 1
epsilon <- 0.1
model_svr <- ksvm(X, y, type="eps-svr", kernel="vanilladot", C=C, epsilon=epsilon)

prediction <- predict(model_svr, X)

svr_data%>%
  ggplot(aes(x=month_year)) +
  geom_line(aes(y=count), color="blue", size=1) +
  geom_line(aes(y=prediction), color="red", size=1) +
  labs(title="Kernlab ksvm Prediction", y="Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
