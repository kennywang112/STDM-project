source('utils/gcn_model.R')

X_sorted <- ready_data %>% arrange(time_date)
n_total <- nrow(ready_data)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

train_X <- train%>%select(accident_count)
test_X <- test%>%select(accident_count)

x_tensor <- torch_tensor(as.matrix(train_X), dtype = torch_float())$to(device = device)
y_tensor <- torch_tensor(as.matrix(train_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)

x_test_tensor <- torch_tensor(as.matrix(test_X), dtype = torch_float())$to(device = device)
y_test_tensor <- torch_tensor(as.matrix(test_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)

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

model_gcn <- gcn_net(1, 16, 1)
model_gcn$to(device = device)
hist_gcn <- train_model(model_gcn, x_tensor, y_tensor, x_test_tensor, y_test_tensor, "./Data/CalculatedData/best_model.pt")

model_t <- gcn_net(3, 16, 1)
model_t$to(device = device)
hist_t <- train_model(model_t, x_t_tensor, y_tensor, x_test_t_tensor, y_test_tensor, "./Data/CalculatedData/best_model_t.pt")

model_gcn$load_state_dict(torch_load("./Data/CalculatedData/best_model.pt"))
model_gcn$eval()
with_no_grad({
  pred_gcn <- as.array(model_gcn(x_test_tensor, A_test)$to(device = "cpu"))[,1]
})

model_t$load_state_dict(torch_load("./Data/CalculatedData/best_model_t.pt"))
model_t$eval()
with_no_grad({
  pred_t <- as.array(model_t(x_test_t_tensor, A_test)$to(device = "cpu"))[,1]
})

test_results_stgcn <- test %>%
  mutate(
    Predicted = pred_gcn,
    Predicted_t = pred_t,
    Actual = accident_count
  )

