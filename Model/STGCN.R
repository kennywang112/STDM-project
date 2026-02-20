library(torch)
device <- torch_device("mps")

X_sorted <- ready_data %>% arrange(time_date)
n_total <- nrow(X_sorted)
split_point <- floor(0.8 * n_total)

train <- X_sorted[1:split_point, ]
test <- X_sorted[(split_point + 1):n_total, ]

gcn_layer <- nn_module(
  "GCN_Layer",
  initialize = function(in_features, out_features) {
    self$weight <- nn_parameter(torch_randn(in_features, out_features))
    self$bias <- nn_parameter(torch_zeros(out_features))
  },
  forward = function(x, adj) {
    x <- x$to(device = self$weight$device)
    adj <- adj$to(device = self$weight$device)
    support <- torch_mm(x, self$weight)
    output <- torch_mm(adj, support)
    output + self$bias
  }
)

gcn_net <- nn_module(
  "GCN_Net",
  initialize = function(n_feat, n_hid, n_out) {
    self$gcn1 <- gcn_layer(n_feat, n_hid)
    self$gcn2 <- gcn_layer(n_hid, n_out)
  },
  forward = function(x, adj) {
    x <- self$gcn1(x, adj)
    x <- torch_relu(x)
    x <- self$gcn2(x, adj)
    x
  }
)

x_tensor <- torch_tensor(as.matrix(train_X), dtype = torch_float())$to(device = device)
y_tensor <- torch_tensor(as.matrix(train_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)
x_test_tensor <- torch_tensor(as.matrix(test_X), dtype = torch_float())$to(device = device)
y_test_tensor <- torch_tensor(as.matrix(test_y), dtype = torch_float())$view(c(-1, 1))$to(device = device)
A <- torch_eye(nrow(train_X))$to(device = device)
A_test <- torch_eye(nrow(test_X))$to(device = device)

x_t_tensor <- torch_tensor(as.matrix(train_X_t), dtype = torch_float())$to(device = device)
x_test_t_tensor <- torch_tensor(as.matrix(test_X_t), dtype = torch_float())$to(device = device)

criterion <- nn_mse_loss()
num_epochs <- 500
min_delta <- 0.0001
patience <- 5

train_model <- function(model_obj, train_x, train_y, test_x, test_y, save_path) {
  target_device <- model_obj$gcn1$weight$device
  train_x <- train_x$to(device = target_device)
  train_y <- train_y$to(device = target_device)
  test_x <- test_x$to(device = target_device)
  test_y <- test_y$to(device = target_device)
  
  optimizer <- optim_adam(model_obj$parameters, lr = 0.01)
  best_loss <- Inf
  early_stop_counter <- 0
  train_hist <- numeric(num_epochs)
  test_hist <- numeric(num_epochs)
  
  for (epoch in 1:num_epochs) {
    model_obj$train()
    optimizer$zero_grad()
    output <- model_obj(train_x, A)
    loss <- criterion(output, train_y)
    loss$backward()
    optimizer$step()
    train_hist[epoch] <- loss$item()
    
    model_obj$eval()
    with_no_grad({
      test_output <- model_obj(test_x, A_test)
      test_loss <- criterion(test_output, test_y)
      test_hist[epoch] <- test_loss$item()
    })
    
    if (test_loss$item() < (best_loss - min_delta)) {
      best_loss <- test_loss$item()
      early_stop_counter <- 0
      torch_save(model_obj$state_dict(), save_path)
    } else {
      early_stop_counter <- early_stop_counter + 1
    }
    
    if (early_stop_counter >= patience) break
  }
  return(list(train_hist = train_hist[1:epoch], test_hist = test_hist[1:epoch], final_epoch = epoch))
}

model_gcn <- gcn_net(3, 16, 1)
model_gcn$to(device = device)
hist_gcn <- train_model(model_gcn, x_tensor, y_tensor, x_test_tensor, y_test_tensor, "./Data/CalculatedData/best_model.pt")

model_t <- gcn_net(2, 16, 1)
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

