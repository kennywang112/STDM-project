library(torch)
device <- torch_device("mps")

temporal_conv_layer <- nn_module(
  "TemporalConvLayer",
  initialize = function(in_channels, out_channels, kt = 3) {
    self$conv <- nn_conv2d(in_channels, 2 * out_channels, kernel_size = c(1, kt))
  },
  forward = function(x) {
    x_conv <- self$conv(x)
    out_split <- torch_split(x_conv, x_conv$shape[2] / 2, dim = 2)
    return(out_split[[1]] * torch_sigmoid(out_split[[2]]))
  }
)

st_conv_block <- nn_module(
  "ST_Conv_Block",
  initialize = function(in_channels, hidden_channels, out_channels, n_nodes, kt = 3) {
    self$tmp_conv1 <- temporal_conv_layer(in_channels, hidden_channels, kt)
    self$gcn <- gcn_layer_for_lstm(hidden_channels, hidden_channels)
    self$tmp_conv2 <- temporal_conv_layer(hidden_channels, out_channels, kt)
    self$ln <- nn_layer_norm(c(n_nodes, out_channels))
  },
  forward = function(x, adj) {
    x <- self$tmp_conv1(x)

    b <- x$shape[1]; c <- x$shape[2]; n <- x$shape[3]; t <- x$shape[4]
    x_gcn <- x$permute(c(1, 4, 3, 2))$reshape(c(-1, n, c)) # [B*T, N, C]
    x_gcn <- self$gcn(x_gcn, adj)
    x_gcn <- torch_relu(x_gcn)

    x <- x_gcn$reshape(c(b, t, n, -1))$permute(c(1, 4, 3, 2)) # [B, C, N, T]
    x <- self$tmp_conv2(x)

    x <- x$permute(c(1, 4, 3, 2)) # [B, T, N, C]
    x <- self$ln(x)
    x <- x$permute(c(1, 4, 3, 2)) # [B, C, N, T]
    return(nnf_dropout(x, p = 0.3, training = self$training))
  }
)

stgcn_net_final <- nn_module(
  "STGCN_Net",
  initialize = function(n_feat, n_hid, n_out, n_nodes, kt = 3, window_size = 12) {
    self$block1 <- st_conv_block(n_feat, n_hid, n_hid, n_nodes, kt)
    self$block2 <- st_conv_block(n_hid, n_hid, n_hid, n_nodes, kt)

    # T -> (T - 2*(kt-1)) -> (T - 4*(kt-1))
    reduced_time <- window_size - 4 * (kt - 1)
    self$output_tmp_conv <- temporal_conv_layer(n_hid, n_hid, reduced_time)
    self$fc <- nn_linear(n_hid, n_out)
  },
  forward = function(x, adj) {
    # [Batch, Nodes, Time, Features] -> [Batch, Features, Nodes, Time]
    x <- x$permute(c(1, 4, 2, 3))

    x <- self$block1(x, adj)
    x <- self$block2(x, adj)
    x <- self$output_tmp_conv(x)

    # [Batch, Nodes, 1]
    x <- x$squeeze(4)$permute(c(1, 3, 2))
    return(self$fc(x))
  }
)

source('./utils/gcn_lstm.R')

torch_manual_seed(123)
window_len <- 12
kt <- 3

A_dense <- torch_tensor(listw2mat(W_list), dtype = torch_float32())$to(device = device)
scaled_full_data <- bind_rows(train_trad, test) %>% arrange(time_date, .data[[cscale]])

# (Samples x Nodes x Window x Feats)
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

train_x_tensor <- torch_tensor(st_data$X[1:train_end, , , , drop = FALSE], dtype = torch_float())
train_y_tensor <- torch_tensor(st_data$Y[1:train_end, , , drop = FALSE], dtype = torch_float())

val_x_tensor <- torch_tensor(st_data$X[(train_end + 1):val_end, , , , drop = FALSE], dtype = torch_float())
val_y_tensor <- torch_tensor(st_data$Y[(train_end + 1):val_end, , , drop = FALSE], dtype = torch_float())

test_x_tensor <- torch_tensor(st_data$X[(val_end + 1):total_samples, , , , drop = FALSE], dtype = torch_float())
test_y_tensor <- torch_tensor(st_data$Y[(val_end + 1):total_samples, , , drop = FALSE], dtype = torch_float())

stgcn_final_model <- stgcn_net_final(
  n_feat = length(feature_cols), n_hid = 32, n_out = 1, 
  n_nodes = dim(train_x_tensor)[2], kt = kt, window_size = window_len
)

history <- train_model_val(
  model_obj = stgcn_final_model,
  train_x = train_x_tensor, train_y = train_y_tensor,
  val_x = val_x_tensor, val_y = val_y_tensor,
  test_x = test_x_tensor, test_y = test_y_tensor,
  A_mat = A_dense,
  save_path = "./Data/CalculatedData/best_stgcn_real_model.pt",
  num_epochs = 200, patience = 15, lr = 1e-3, batch_size = 16
)

stgcn_final_model$load_state_dict(torch_load("./Data/CalculatedData/best_stgcn_real_model.pt"))
stgcn_final_model$to(device = device)
stgcn_final_model$eval()

with_no_grad({
  pred_tensor <- stgcn_final_model(test_x_tensor$to(device = device), A_dense)
  pred_stgcn <- as.numeric(pred_tensor$to(device = "cpu"))
})

test_results_stgcn <- test %>%
  mutate(
    real_actual = accident_count * (acc_max - acc_min) + acc_min,
    Predicted_stgcn = pred_stgcn * (acc_max - acc_min) + acc_min,
    mse_stgcn = (Predicted_stgcn - real_actual)^2
  ) %>%
  mutate(accident_count = real_actual) %>%
  select(-real_actual)

test_results_stgcn %>% write.csv("./Data/CalculatedData/test_results_stgcn_real.csv", row.names = FALSE)