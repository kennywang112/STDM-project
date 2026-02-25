library(torch)
device <- torch_device("mps")

gcn_layer <- nn_module(
  "GCN_Layer",
  initialize = function(in_features, out_features) {
    self$weight <- nn_parameter(torch_randn(in_features, out_features))
    self$bias <- nn_parameter(torch_zeros(out_features))
  },
forward = function(x, adj) {
    support <- torch_matmul(x, self$weight)
    output <- torch_matmul(adj, support) 
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
    return(x)
  }
)

ann_net <- nn_module(
  "ANN_Net",
  initialize = function(n_feat, n_hid, n_out) {
    self$fc1 <- nn_linear(n_feat, n_hid)
    self$fc2 <- nn_linear(n_hid, n_out)
  },
  forward = function(x) {
    x <- self$fc1(x)
    x <- torch_relu(x)
    x <- self$fc2(x)
    return(x)
  }
)

train_model <- function(
    model_obj, train_x, train_y, test_x, test_y, save_path,
    A_train = NULL, A_test = NULL,
    num_epochs = 100, min_delta = 0.001, patience = 5,
    target_device = device
) {
  model_obj$to(device = target_device)
  train_x <- train_x$to(device = target_device)
  train_y <- train_y$to(device = target_device)
  test_x <- test_x$to(device = target_device)
  test_y <- test_y$to(device = target_device)
  
  if (!is.null(A_train)) A_train <- A_train$to(device = target_device)
  if (!is.null(A_test)) A_test <- A_test$to(device = target_device)
  
  optimizer <- optim_adam(model_obj$parameters, lr = 0.001)
  best_loss <- Inf
  early_stop_counter <- 0
  train_hist <- numeric(num_epochs)
  test_hist <- numeric(num_epochs)
  
  for (epoch in 1:num_epochs) {
    model_obj$train()
    optimizer$zero_grad()
  
    if (!is.null(A_train)) {
      output <- model_obj(train_x, A_train)
    } else {
      output <- model_obj(train_x)
    }
    
    loss <- criterion(output, train_y)
    loss$backward()
    optimizer$step()
    train_hist[epoch] <- loss$item()
    
    model_obj$eval()
    with_no_grad({
      if (!is.null(A_test)) {
        test_output <- model_obj(test_x, A_test)
      } else {
        test_output <- model_obj(test_x)
      }
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
    
    if (early_stop_counter >= patience) {
      cat(sprintf("Early stopping at epoch %d\n", epoch))
      break
    }
  }
  return(list(train_hist = train_hist[1:epoch], test_hist = test_hist[1:epoch], final_epoch = epoch))
}