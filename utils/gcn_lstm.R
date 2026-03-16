library(torch)
device <- torch_device("mps")

create_stgcn_data <- function(
    long_data, cscale_col, time_col, value_col, feat_cols, window_size = 12
) {
  # 1. Initialize metadata and ensure spatial consistency
  time_labels <- sort(unique(long_data[[time_col]]))
  # CRITICAL: Use ordered_ids to ensure node sequence matches W_list
  node_labels <- ordered_ids 
  N_nodes <- length(node_labels)
  T_total <- length(time_labels)
  num_feats <- length(feat_cols)
  
  # 2. Extract Features into a 3D Array [Time x Nodes x Features]
  # We use pivot_wider for each feature to ensure correct spatial alignment
  feat_3d <- array(0, dim = c(T_total, N_nodes, num_feats))
  
  for (f in 1:num_feats) {
    feat_name <- feat_cols[f]
    wide_feat <- long_data %>%
      select(all_of(c(time_col, cscale_col, feat_name))) %>%
      pivot_wider(names_from = all_of(cscale_col), values_from = all_of(feat_name)) %>%
      arrange(!!sym(time_col)) %>%
      # Force columns to follow the exact order of the adjacency matrix
      select(all_of(node_labels)) 
    
    feat_3d[,,f] <- as.matrix(wide_feat)
  }
  
  # 3. Extract Target variable (Y) into a Matrix [Time x Nodes]
  Y_mat <- long_data %>%
    select(all_of(c(time_col, cscale_col, value_col))) %>%
    pivot_wider(names_from = all_of(cscale_col), values_from = all_of(value_col)) %>%
    arrange(!!sym(time_col)) %>%
    select(all_of(node_labels)) %>%
    as.matrix()
  
  S_samples <- T_total - window_size
  
  # 4. Initialize Output Tensors
  # X: [Samples, Nodes, Window, Features] - Standard GCN-LSTM input format
  X_array <- array(0, dim = c(S_samples, N_nodes, window_size, num_feats))
  # Y: [Samples, Nodes, 1] - Predicting next time step for all nodes
  Y_array <- array(0, dim = c(S_samples, N_nodes, 1))
  
  # 5. Sliding Window Generation
  for (i in 1:S_samples) {
    # Extract window slice: [Window_size x Nodes x Features]
    window_data <- feat_3d[i:(i + window_size - 1), , ]
    
    # Use aperm to transpose dimensions to [Nodes x Window x Features]
    # This aligns with the 'n_feat' and 'window_len' logic in gcnlstm_net
    X_array[i, , , ] <- aperm(window_data, c(2, 1, 3))
    
    # Set the target as the observation immediately following the window
    Y_array[i, , 1] <- Y_mat[i + window_size, ]
  }
  
  # Return target time labels for evaluation mapping
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
    
    out <- output + self$bias
    return(out)
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
    lstm_last_step <- nnf_dropout(lstm_last_step, p = 0.3, training = self$training)

    gcn_input <- lstm_last_step$view(c(B, N, -1))
    out <- self$gcn(gcn_input, adj)
    # out <- torch_sigmoid(out)
    
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
    num_epochs = 100, min_delta = 0.0001, patience = 5, lr = 0.00001,
    target_device = device,
    batch_size = 128
) {
  model_obj$to(device = target_device)
  
  train_ds <- tensor_dataset(train_x, train_y)
  train_dl <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)

  val_x <- val_x$to(device = target_device)
  val_y <- val_y$to(device = target_device)
  test_x <- test_x$to(device = target_device)
  test_y <- test_y$to(device = target_device)
  
  if (!is.null(A_mat)) A_mat <- A_mat$to(device = target_device)

  optimizer <- optim_adam(model_obj$parameters, lr = lr)
  
  scheduler <- lr_step(optimizer, step_size = 30, gamma = 0.9)
  criterion <- nn_mse_loss()
  
  best_val_loss <- Inf
  early_stop_counter <- 0
  train_hist <- numeric(num_epochs)
  val_hist <- numeric(num_epochs)
  
  for (epoch in 1:num_epochs) {
    model_obj$train()
    
    epoch_loss <- 0
    num_batches <- 0
    
    coro::loop(for (batch in train_dl) {
      optimizer$zero_grad()
      
      b_x <- batch[[1]]$to(device = target_device)
      b_y <- batch[[2]]$to(device = target_device)
      
      if (!is.null(A_mat)) {
        output <- model_obj(b_x, A_mat)
      } else {
        output <- model_obj(b_x)
      }
      
      loss <- criterion(output, b_y)
      loss$backward()
      optimizer$step()
      
      epoch_loss <- epoch_loss + loss$item()
      num_batches <- num_batches + 1
    })
    
    train_hist[epoch] <- epoch_loss / num_batches
    
    scheduler$step()
    
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
    
    if (epoch %% 5 == 0 || epoch == 1) {
      cat(sprintf("Epoch %3d | Train Loss: %.5f | Val Loss: %.5f\n", epoch, train_hist[epoch], val_hist[epoch]))
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
