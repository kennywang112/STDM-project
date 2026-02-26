create_stgcn_data <- function(
  long_data, cscale_col, time_col, value_col, window_size = 7
) {
  # Row: time, Col: MSOA
  wide_data <- long_data %>%
    select(all_of(c(time_col, cscale_col, value_col))) %>%
    pivot_wider(names_from = all_of(cscale_col), values_from = all_of(value_col), values_fill = 0) %>%
    arrange(!!sym(time_col))
  
  time_labels <- wide_data[[time_col]]
  Y_mat <- as.matrix(wide_data %>% select(-all_of(time_col)))
  
  N_nodes <- ncol(Y_mat)
  T_total <- nrow(Y_mat)
  S_samples <- T_total - window_size
  
  # [Samples, Nodes, Window_Size, Features = 1 (only accident)]

  X_array <- array(0, dim = c(S_samples, N_nodes, window_size, 1))
  
  # [Samples, Nodes, 1]
  Y_array <- array(0, dim = c(S_samples, N_nodes, 1))
  
  for (i in 1:S_samples) {
    window <- Y_mat[i:(i + window_size - 1), ]
    X_array[i, , , 1] <- t(window)
    Y_array[i, , 1] <- Y_mat[i + window_size, ]
  }
  
  target_times <- time_labels[(window_size + 1):T_total]
  
  return(list(X = X_array, Y = Y_array, times = target_times, N = N_nodes))
}

gcn_layer_for_lstm <- nn_module(
  "GCN_Layer",
  initialize = function(in_features, out_features) {
    self$weight <- nn_parameter(torch_randn(in_features, out_features))
    self$bias <- nn_parameter(torch_zeros(out_features))
  },
  forward = function(x, adj) {
    # [Batch, Nodes, n_hid] @ [n_hid, 1]
    support <- torch_matmul(x, self$weight)
    # [1, Nodes, Nodes] @ [batch, Nodes, 1] -> [batch, Nodes, 1]
    output <- torch_matmul(adj$unsqueeze(1), support)
    
    return(output + self$bias)
  }
)

gcnlstm_net <- nn_module(
  "GCN_LSTM_Net",
  initialize = function(n_feat, n_hid, n_out, lstm_layers = 1) {
    # input [Batch, Nodes, n_hid]
    self$lstm <- nn_lstm(input_size = n_feat, 
                         hidden_size = n_hid, 
                         num_layers = lstm_layers, 
                         batch_first = TRUE)
    self$gcn <- gcn_layer_for_lstm(n_hid, n_out)
  },

  forward = function(x, adj) {

    B <- x$shape[1] # Batch 
    N <- x$shape[2] # Nodes 
    T_steps <- x$shape[3] # Time
    F_feat <- x$shape[4] # Features

    # [Batch, Nodes, Time, Features] -> [Batch * Nodes, Time, Features]
    x_lstm <- x$view(c(B * N, T_steps, F_feat))

    # [Batch * Nodes, Time, Features] -> [Batch * Nodes, Time, n_hid]
    lstm_out <- self$lstm(x_lstm)
    # Transform back to [Batch, Nodes, n_hid]
    lstm_last_step <- lstm_out[[1]][, -1, ]
    gcn_input <- lstm_last_step$view(c(B, N, -1))

    out <- self$gcn(gcn_input, adj)
    
    return(out)
  }
)

train_model_val <- function(
    model_obj, 
    train_x, train_y, 
    val_x, val_y, 
    test_x, test_y, 
    save_path,
    A_mat = NULL,
    num_epochs = 100, min_delta = 0.0001, patience = 5,
    target_device = device
) {
  model_obj$to(device = target_device)
  train_x <- train_x$to(device = target_device)
  train_y <- train_y$to(device = target_device)
  val_x <- val_x$to(device = target_device)
  val_y <- val_y$to(device = target_device)
  test_x <- test_x$to(device = target_device)
  test_y <- test_y$to(device = target_device)
  
  if (!is.null(A_mat)) A_mat <- A_mat$to(device = target_device)

  optimizer <- optim_adam(model_obj$parameters, lr = 0.001)
  criterion <- nn_mse_loss()
  
  best_val_loss <- Inf
  early_stop_counter <- 0
  train_hist <- numeric(num_epochs)
  val_hist <- numeric(num_epochs)
  
  for (epoch in 1:num_epochs) {
    model_obj$train()
    optimizer$zero_grad()
    
    if (!is.null(A_mat)) {
      output <- model_obj(train_x, A_mat)
    } else {
      output <- model_obj(train_x)
    }
    
    loss <- criterion(output, train_y)
    loss$backward()
    optimizer$step()
    train_hist[epoch] <- loss$item()
  
    model_obj$eval()
    with_no_grad({
      if (!is.null(A_mat)) {
        val_output <- model_obj(val_x, A_mat)
      } else {
        val_output <- model_obj(val_x)
      }
      val_loss <- criterion(val_output, val_y)
      val_hist[epoch] <- val_loss$item()
    })

    if (val_loss$item() < (best_val_loss - min_delta)) {
      best_val_loss <- val_loss$item()
      early_stop_counter <- 0
      torch_save(model_obj$state_dict(), save_path)
    } else {
      early_stop_counter <- early_stop_counter + 1
    }
    
    if (early_stop_counter >= patience) {
      cat(sprintf("-> Early stopping in Epoch %d. Val Loss: %.5f\n", epoch, best_val_loss))
      break
    }
  
    if (epoch %% 10 == 0 || epoch == 1) {
      cat(sprintf("Epoch %3d | Train Loss: %.5f | Val Loss: %.5f\n", epoch, loss$item(), val_loss$item()))
    }
  }

  model_obj$load_state_dict(torch_load(save_path))
  model_obj$eval()
  with_no_grad({
    if (!is.null(A_mat)) {
      test_output <- model_obj(test_x, A_mat)
    } else {
      test_output <- model_obj(test_x)
    }
    final_test_loss <- criterion(test_output, test_y)
  })
  
  cat(sprintf("\n=== Finish Training ===\n Test Loss: %.5f\n", final_test_loss$item()))

  return(list(
    train_hist = train_hist[1:epoch], 
    val_hist = val_hist[1:epoch], 
    final_epoch = epoch,
    test_loss = final_test_loss$item(),
    predictions = test_output
  ))
}
