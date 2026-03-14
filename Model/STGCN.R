source('utils/gcn_model.R')
source('utils/gcn_lstm.R') # new version of train_model_val is here

torch_manual_seed(123)

N_nodes <- length(W_list$neighbours)
A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)

train_data <- train_dl
val_data <- val_dl
test_data <- test

feature_cols <- c("t_minus_1", "t_minus_12", "spatial_lag_1", "pop_lag_1", "rain_lag_1")

x_ann_train <- as.matrix(train_data %>% select(all_of(feature_cols)))
x_ann_val <- as.matrix(val_data %>% select(all_of(feature_cols)))
x_ann_test <- as.matrix(test_data %>% select(all_of(feature_cols)))

x_tensor <- torch_tensor(x_ann_train, dtype = torch_float())
x_val_tensor <- torch_tensor(x_ann_val, dtype = torch_float())
x_test_tensor <- torch_tensor(x_ann_test, dtype = torch_float())

y_tensor <- torch_tensor(train_data$accident_count, dtype = torch_float())$view(c(-1, 1))
y_val_tensor <- torch_tensor(val_data$accident_count, dtype = torch_float())$view(c(-1, 1))
y_test_tensor <- torch_tensor(test_data$accident_count, dtype = torch_float())$view(c(-1, 1))

x_gcn_train <- as.matrix(train_data %>% select(all_of(feature_cols)))
x_gcn_val <- as.matrix(val_data %>% select(all_of(feature_cols)))
x_gcn_test <- as.matrix(test_data %>% select(all_of(feature_cols)))

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
min_delta <- 0.00001
lr <- 1e-3
patience <- 10
batch_size <- 16

model_ann <- ann_net(n_feat = 5, n_hid = 16, n_out = 1) 

cat('ANN')
hist_ann <- train_model_val(
  model_obj = model_ann, 
  train_x = x_tensor, train_y = y_tensor, 
  val_x = x_val_tensor, val_y = y_val_tensor,
  test_x = x_test_tensor, test_y = y_test_tensor,
  save_path = "./Data/CalculatedData/best_model_ann.pt",
  A_mat = NULL, num_epochs = num_epochs, patience = patience, min_delta = min_delta, lr = lr,
  batch_size = batch_size
)

model_ann$load_state_dict(torch_load("./Data/CalculatedData/best_model_ann.pt"))
model_ann$to(device = device) 
model_ann$eval()
with_no_grad({
  pred_ann <- as.numeric(model_ann(x_test_tensor$to(device = device))$to(device = "cpu"))
})


criterion <- nn_mse_loss()
num_epochs <- 100
min_delta <- 0.00001
lr <- 1e-2
patience <- 10
batch_size <- 16

model_stgcn <- gcn_net(n_feat = 5, n_hid = 32, n_out = 1)

cat('STGCN')
hist_stgcn <- train_model_val(
  model_obj = model_stgcn, 
  train_x = x_t_tensor_3d, train_y = y_t_tensor_3d,
  val_x = x_t_val_tensor_3d,  val_y = y_t_val_tensor_3d,
  test_x = x_test_t_tensor_3d, test_y = y_test_t_tensor_3d,
  save_path = "./Data/CalculatedData/best_model_stgcn.pt",
  A_mat = A_dense, num_epochs = num_epochs, patience = patience, min_delta = min_delta, lr = lr,
  batch_size = batch_size
)

saveRDS(list(ann = hist_ann, stgcn = hist_stgcn), file = "./Data/CalculatedData/history_stgcn.rds")

model_stgcn$load_state_dict(torch_load("./Data/CalculatedData/best_model_stgcn.pt"))
model_stgcn$to(device = device)
model_stgcn$eval()
with_no_grad({
  pred_stgcn <- as.numeric(model_stgcn(x_test_t_tensor_3d$to(device = device), A_dense)$to(device = "cpu"))
})

test_results_stgcn <- test %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    Predicted_ann = pred_ann * (acc_max - acc_min) + acc_min,
    Predicted_stgcn = pred_stgcn * (acc_max - acc_min) + acc_min,
    mse_ann = (real_actual - Predicted_ann)^2,
    mse_stgcn = (real_actual - Predicted_stgcn)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

test_results_stgcn %>% write.csv("./Data/CalculatedData/test_results_stgcn.csv", row.names = FALSE)
test_results_stgcn <- read.csv("./Data/CalculatedData/test_results_stgcn.csv")
