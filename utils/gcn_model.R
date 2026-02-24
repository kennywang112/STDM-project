library(torch)
device <- torch_device("mps")

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

train_model <- function(
    model_obj, train_x, train_y, test_x, test_y, save_path
) {
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
    cat("Epoch:", epoch, "\n")
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
