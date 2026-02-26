source('./utils/gcn_lstm.R')

window_len <- 7
A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)

st_data <- create_stgcn_data(
  long_data = ready_data, 
  cscale_col = cscale, 
  time_col = "time_date", 
  value_col = "accident_count", 
  window_size = window_len
)

unique_dates <- sort(unique(ready_data$time_date))
split_index <- floor(0.8 * length(unique_dates))
split_date <- unique_dates[split_index]

val_end <- max(which(st_data$times <= split_date))
train_end <- floor(0.8 * val_end)
total_samples <- length(st_data$times)

train_X_array <- st_data$X[1:train_end, , , , drop = FALSE]
val_X_array   <- st_data$X[(train_end + 1):val_end, , , , drop = FALSE]
test_X_array  <- st_data$X[(val_end + 1):total_samples, , , , drop = FALSE]

train_y_array <- st_data$Y[1:train_end, , , drop = FALSE]
val_y_array   <- st_data$Y[(train_end + 1):val_end, , , drop = FALSE]
test_y_array  <- st_data$Y[(val_end + 1):total_samples, , , drop = FALSE]

train_x_tensor <- torch_tensor(train_X_array, dtype = torch_float())
train_y_tensor <- torch_tensor(train_y_array, dtype = torch_float())

val_x_tensor   <- torch_tensor(val_X_array, dtype = torch_float())
val_y_tensor   <- torch_tensor(val_y_array, dtype = torch_float())

test_x_tensor  <- torch_tensor(test_X_array, dtype = torch_float())
test_y_tensor  <- torch_tensor(test_y_array, dtype = torch_float())

cat(sprintf("Train: %d, Val: %d, Test: %d\n", dim(train_x_tensor)[1], dim(val_x_tensor)[1], dim(test_x_tensor)[1]))

stgcn_model <- gcnlstm_net(n_feat = 1, n_hid = 16, n_out = 1, lstm_layers = 1)

history <- train_model_val(
  model_obj = stgcn_model,
  train_x = train_x_tensor, train_y = train_y_tensor,
  val_x   = val_x_tensor,   val_y   = val_y_tensor,
  test_x  = test_x_tensor,  test_y  = test_y_tensor,
  A_mat   = A_dense, 
  save_path = "./Data/CalculatedData/best_gcnlstm_model.pt",
  num_epochs = 150,
  patience = 10  
)

stgcn_model$load_state_dict(torch_load("./Data/CalculatedData/best_gcnlstm_model.pt"))
stgcn_model$to(device = device)
stgcn_model$eval()
with_no_grad({
  pred_gcn_lstm <- as.numeric(model_stgcn(x_test_t_tensor_3d$to(device = device), A_dense)$to(device = "cpu"))
})


test_results_final <- test %>%
  mutate(
    Predicted_ann = pred_ann,
    Predicted_stgcn = pred_stgcn,
    Predicted_gcn_lstm = pred_gcn_lstm,
    Actual = accident_count
  )%>%
  mutate(
    mse_ann = (Predicted_ann - Actual)^2,
    mse_stgcn = (Predicted_stgcn - Actual)^2,
    mse_gcn_lstm = (Predicted_gcn_lstm - Actual)^2
  )

test_results_final%>%
  summarise(
    mse_ann = mean(mse_ann),
    mse_stgcn = mean(mse_stgcn),
    mse_gcn_lstm = mean(mse_gcn_lstm)
  )

test_results_final%>%
  group_by(time_date)%>%
  summarise(
    mean_pred_ann = mean(Predicted_ann),
    mean_pred_stgcn = mean(Predicted_stgcn),
    mean_pred_gcn_lstm = mean(Predicted_gcn_lstm),
    mean_actual = mean(Actual)
  )%>%
  ggplot()+
  geom_line(aes(x=time_date, y=mean_pred_ann, color="STGCN"), linetype = "dashed")+
  geom_line(aes(x=time_date, y=mean_pred_stgcn, color="GCN-LSTM"), linetype = "dashed")+
  geom_line(aes(x=time_date, y=mean_pred_gcn_lstm, color="ANN"), linetype = "dashed")+
  geom_line(aes(x=time_date, y=mean_actual, color="Actual"))


