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
