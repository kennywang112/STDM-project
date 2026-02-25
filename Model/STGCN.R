source('utils/gcn_model.R')

N_nodes <- length(W_list$neighbours)

A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)
x_tensor <- torch_tensor(as.matrix(train_X), dtype = torch_float())
y_tensor <- torch_tensor(as.matrix(train_y), dtype = torch_float())$view(c(-1, 1))
x_test_tensor <- torch_tensor(as.matrix(test_X), dtype = torch_float())
y_test_tensor <- torch_tensor(as.matrix(test_y), dtype = torch_float())$view(c(-1, 1))

T_train <- nrow(train_X_t) / N_nodes
T_test <- nrow(test_X_t) / N_nodes

x_t_tensor_3d <- torch_tensor(train_X_t, dtype = torch_float())$view(c(T_train, N_nodes, -1))
x_test_t_tensor_3d <- torch_tensor(test_X_t, dtype = torch_float())$view(c(T_test, N_nodes, -1))
y_t_tensor_3d <- torch_tensor(as.matrix(train_y), dtype = torch_float())$view(c(T_train, N_nodes, 1))
y_test_t_tensor_3d <- torch_tensor(as.matrix(test_y), dtype = torch_float())$view(c(T_test, N_nodes, 1))

criterion <- nn_mse_loss()
num_epochs <- 100
min_delta <- 0.0001
patience <- 5

model_ann <- ann_net(n_feat = 3, n_hid = 16, n_out = 1) 

cat('ANN')
hist_ann <- train_model(
  model_obj = model_ann, 
  train_x = x_tensor, train_y = y_tensor, 
  test_x = x_test_tensor, test_y = y_test_tensor,
  min_delta = min_delta, patience = patience, num_epochs = num_epochs,
  save_path = "./Data/CalculatedData/best_model_ann.pt",
  A_train = NULL, A_test = NULL
)

model_ann$load_state_dict(torch_load("./Data/CalculatedData/best_model_ann.pt"))
model_ann$to(device = device) 
model_ann$eval()
with_no_grad({
  pred_ann <- as.numeric(model_ann(x_test_tensor$to(device = device))$to(device = "cpu"))
})

model_stgcn <- gcn_net(n_feat = 2, n_hid = 16, n_out = 1)

cat('STGCN')
hist_stgcn <- train_model(
  model_obj = model_stgcn, 
  train_x = x_t_tensor_3d, train_y = y_t_tensor_3d,
  test_x = x_test_t_tensor_3d, test_y = y_test_t_tensor_3d,
  save_path = "./Data/CalculatedData/best_model_stgcn.pt",
  A_train = A_dense, A_test = A_dense
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
