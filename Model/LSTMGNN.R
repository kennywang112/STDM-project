source('./utils/gcn_lstm.R')

torch_manual_seed(123)

window_len <- 12
A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)

scaled_full_data <- bind_rows(train_trad, test) %>% arrange(time_date, .data[[cscale]])

st_data <- create_stgcn_data(
  long_data = scaled_full_data, 
  cscale_col = cscale, 
  time_col = "time_date", 
  value_col = "accident_count",
  feat_cols = feature_cols,
  window_size = window_len
)

train_end <- max(which(st_data$times < val_start_date))
val_end <- max(which(st_data$times < test_start_date))
total_samples <- length(st_data$times)

train_X_array <- st_data$X[1:train_end, , , , drop = FALSE]
val_X_array <- st_data$X[(train_end + 1):val_end, , , , drop = FALSE]
test_X_array <- st_data$X[(val_end + 1):total_samples, , , , drop = FALSE]

train_y_array <- st_data$Y[1:train_end, , , drop = FALSE]
val_y_array <- st_data$Y[(train_end + 1):val_end, , , drop = FALSE]
test_y_array  <- st_data$Y[(val_end + 1):total_samples, , , drop = FALSE]

train_x_tensor <- torch_tensor(train_X_array, dtype = torch_float())
train_y_tensor <- torch_tensor(train_y_array, dtype = torch_float())

val_x_tensor <- torch_tensor(val_X_array, dtype = torch_float())
val_y_tensor <- torch_tensor(val_y_array, dtype = torch_float())

test_x_tensor <- torch_tensor(test_X_array, dtype = torch_float())
test_y_tensor <- torch_tensor(test_y_array, dtype = torch_float())

cat(sprintf("Train: %d, Val: %d, Test: %d\n", dim(train_x_tensor)[1], dim(val_x_tensor)[1], dim(test_x_tensor)[1]))

num_epochs <- 200
min_delta <- 0.00001
lr <- 1e-3
patience <- 10
batch_size <- 16

stgcn_model <- gcnlstm_net(n_feat = 7, n_hid = 16, n_out = 1, lstm_layers = 1)

time_gcnlstm <- system.time({
  history <- train_model_val(
    model_obj = stgcn_model,
    train_x = train_x_tensor, train_y = train_y_tensor,
    val_x = val_x_tensor, val_y = val_y_tensor,
    test_x = test_x_tensor, test_y = test_y_tensor,
    A_mat = A_dense,
    save_path = "./Data/CalculatedData/best_gcnlstm_model.pt",
    num_epochs = num_epochs, patience = patience, min_delta = min_delta, lr = lr,
    batch_size = batch_size
  )
})
print(time_gcnlstm)

saveRDS(history, file = "./Data/CalculatedData/history_gcnlstm.rds")

stgcn_model$load_state_dict(torch_load("./Data/CalculatedData/best_gcnlstm_model.pt"))
stgcn_model$to(device = device)
stgcn_model$eval()
with_no_grad({
  pred_tensor <- stgcn_model(test_x_tensor$to(device = device), A_dense)
  pred_gcn_lstm <- as.numeric(pred_tensor$to(device = "cpu"))
})


test_results_gcnlstm <- test %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    Predicted_gcn_lstm = pred_gcn_lstm * (acc_max - acc_min) + acc_min,
    mse_gcn_lstm = (Predicted_gcn_lstm - real_actual)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

test_results_gcnlstm%>%write.csv("./Data/CalculatedData/test_results_gcnlstm.csv")
test_results_gcnlstm <- read.csv("./Data/CalculatedData/test_results_gcnlstm.csv")

