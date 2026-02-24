source('utils/gcn_model.R')

X_sorted <- ready_data %>% arrange(time_date)
n_total <- nrow(ready_data)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

# This only include accident count
train_X <- train%>%select(accident_count)
test_X <- test%>%select(accident_count)

x_tensor <- torch_tensor(as.matrix(train_X), dtype = torch_float())$to(device = device)
y_tensor <- torch_tensor(as.matrix(train_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)

x_test_tensor <- torch_tensor(as.matrix(test_X), dtype = torch_float())$to(device = device)
y_test_tensor <- torch_tensor(as.matrix(test_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)

# This include both accident count and temporal lag
train_X_t <- as.matrix(train%>%select(accident_count, t_minus_1, t_minus_7))
test_X_t <- as.matrix(test%>%select(accident_count, t_minus_1, t_minus_7))

x_t_tensor <- torch_tensor(train_X_t, dtype = torch_float())$to(device = device)
x_test_t_tensor <- torch_tensor(test_X_t, dtype = torch_float())$to(device = device)

A <- torch_eye(nrow(train_X))$to(device = device)
A_test <- torch_eye(nrow(test_X))$to(device = device)

criterion <- nn_mse_loss()
num_epochs <- 100
min_delta <- 0.0001
patience <- 5

model_sgcn <- gcn_net(1, 16, 1)
model_sgcn$to(device = device)
hist_sgcn <- train_model(model_sgcn, x_tensor, y_tensor, x_test_tensor, y_test_tensor, "./Data/CalculatedData/best_model_sgcn.pt")

model_sgcn$load_state_dict(torch_load("./Data/CalculatedData/best_model.pt"))
model_sgcn$eval()
with_no_grad({
  pred_sgcn <- as.array(model_sgcn(x_test_tensor, A_test)$to(device = "cpu"))[,1]
})

model_stgcn <- gcn_net(3, 16, 1)
model_stgcn$to(device = device)
hist_t <- train_model(model_stgcn, x_t_tensor, y_tensor, x_test_t_tensor, y_test_tensor, "./Data/CalculatedData/best_model_stgcn.pt")

model_stgcn$load_state_dict(torch_load("./Data/CalculatedData/best_model_stgcn.pt"))
model_stgcn$eval()
with_no_grad({
  pred_stgcn <- as.array(model_stgcn(x_test_t_tensor, A_test)$to(device = "cpu"))[,1]
})

test_results_stgcn <- test %>%
  mutate(
    Predicted_sgcn = pred_sgcn,
    Predicted_stgcn = pred_stgcn,
    Actual = accident_count
  )

test_results_stgcn%>%write.csv("./Data/CalculatedData/test_results_stgcn.csv")
test_results_stgcn <- read.csv("./Data/CalculatedData/test_results_stgcn.csv")
