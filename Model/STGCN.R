source('utils/gcn_model.R')

N_nodes <- length(W_list$neighbours)
A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)

ready_data <- ready_data %>% arrange(time_date, .data[[cscale]])

unique_dates <- sort(unique(ready_data$time_date))
total_time_steps <- length(unique_dates)

split_index <- floor(0.8 * total_time_steps)
split_date <- unique_dates[split_index]

train_index <- floor(0.8 * split_index)
train_date <- unique_dates[train_index]

train_data <- ready_data %>% filter(time_date <= train_date)
val_data <- ready_data %>% filter(time_date > train_date & time_date <= split_date)
test_data <- ready_data %>% filter(time_date > split_date)

x_ann_train <- as.matrix(train_data %>% select(t_minus_1, t_minus_7, spatial_lag_1))
x_ann_val <- as.matrix(val_data %>% select(t_minus_1, t_minus_7, spatial_lag_1))
x_ann_test <- as.matrix(test_data %>% select(t_minus_1, t_minus_7, spatial_lag_1))

x_tensor <- torch_tensor(x_ann_train, dtype = torch_float())
x_val_tensor <- torch_tensor(x_ann_val, dtype = torch_float())
x_test_tensor <- torch_tensor(x_ann_test, dtype = torch_float())

y_tensor <- torch_tensor(train_data$accident_count, dtype = torch_float())$view(c(-1, 1))
y_val_tensor <- torch_tensor(val_data$accident_count, dtype = torch_float())$view(c(-1, 1))
y_test_tensor <- torch_tensor(test_data$accident_count, dtype = torch_float())$view(c(-1, 1))

x_gcn_train <- as.matrix(train_data %>% select(t_minus_1, t_minus_7))
x_gcn_val <- as.matrix(val_data %>% select(t_minus_1, t_minus_7))
x_gcn_test <- as.matrix(test_data %>% select(t_minus_1, t_minus_7))

T_train <- nrow(train_data) / N_nodes
T_val <- nrow(val_data) / N_nodes
T_test <- nrow(test_data) / N_nodes

x_t_tensor_3d <- torch_tensor(x_gcn_train, dtype = torch_float())$view(c(T_train, N_nodes, -1))
x_t_val_tensor_3d <- torch_tensor(x_gcn_val, dtype = torch_float())$view(c(T_val, N_nodes, -1))
x_test_t_tensor_3d <- torch_tensor(x_gcn_test, dtype = torch_float())$view(c(T_test, N_nodes, -1))

y_t_tensor_3d <- torch_tensor(train_data$accident_count, dtype = torch_float())$view(c(T_train, N_nodes, 1))
y_t_val_tensor_3d <- torch_tensor(val_data$accident_count, dtype = torch_float())$view(c(T_val, N_nodes, 1))
y_test_t_tensor_3d <- torch_tensor(test_data$accident_count, dtype = torch_float())$view(c(T_test, N_nodes, 1))

criterion <- nn_mse_loss()
num_epochs <- 100
min_delta <- 0.0001
patience <- 5

model_ann <- ann_net(n_feat = 3, n_hid = 16, n_out = 1) 

cat('ANN')
hist_ann <- train_model_val(
  model_obj = model_ann, 
  train_x = x_tensor, train_y = y_tensor, 
  val_x = x_val_tensor, val_y = y_val_tensor,
  test_x = x_test_tensor, test_y = y_test_tensor,
  save_path = "./Data/CalculatedData/best_model_ann.pt",
  A_mat = NULL, num_epochs = num_epochs, patience = patience
)

model_ann$load_state_dict(torch_load("./Data/CalculatedData/best_model_ann.pt"))
model_ann$to(device = device) 
model_ann$eval()
with_no_grad({
  pred_ann <- as.numeric(model_ann(x_test_tensor$to(device = device))$to(device = "cpu"))
})

model_stgcn <- gcn_net(n_feat = 2, n_hid = 16, n_out = 1)

cat('STGCN')
hist_stgcn <- train_model_val(
  model_obj = model_stgcn, 
  train_x = x_t_tensor_3d,      train_y = y_t_tensor_3d,
  val_x   = x_t_val_tensor_3d,  val_y   = y_t_val_tensor_3d,
  test_x  = x_test_t_tensor_3d, test_y  = y_test_t_tensor_3d,
  save_path = "./Data/CalculatedData/best_model_stgcn.pt",
  A_mat = A_dense, num_epochs = num_epochs, patience = patience
)

model_stgcn$load_state_dict(torch_load("./Data/CalculatedData/best_model_stgcn.pt"))
model_stgcn$to(device = device)
model_stgcn$eval()
with_no_grad({
  pred_stgcn <- as.numeric(model_stgcn(x_test_t_tensor_3d$to(device = device), A_dense)$to(device = "cpu"))
})


test_results_stgcn <- test %>%
  mutate(
    Predicted_ann = pred_ann,
    Predicted_stgcn = pred_stgcn,
    Actual = accident_count
  )

test_results_stgcn %>% write.csv("./Data/CalculatedData/test_results_stgcn.csv", row.names = FALSE)
test_results_stgcn <- read.csv("./Data/CalculatedData/test_results_stgcn.csv")
