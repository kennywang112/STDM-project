library(tidyverse)
library(ggplot2)

hist_tgcn_file <- readRDS("./Data/CalculatedData/history_tgcn.rds")
hist_ann <- hist_tgcn_file$ann
hist_tgcn <- hist_tgcn_file$tgcn
hist_gcnlstm <- readRDS("./Data/CalculatedData/history_gcnlstm.rds")
hist_stgcn <- readRDS("./Data/CalculatedData/history_stgcn.rds")

create_loss_df <- function(hist_obj, model_name) {
  data.frame(
    Epoch = 1:length(hist_obj$train_hist),
    Train_Loss = hist_obj$train_hist,
    Val_Loss = hist_obj$val_hist,
    Model = model_name
  )
}

loss_df_val <- bind_rows(
  create_loss_df(hist_ann, "ANN"),
  create_loss_df(hist_tgcn, "TGCN"),
  create_loss_df(hist_gcnlstm, "GCN+LSTM"),
  create_loss_df(hist_stgcn, "STGCN")
) %>%
  select(Epoch, Val_Loss, Model) %>%
  mutate(Model = factor(Model, levels = c("ANN", "TGCN", "GCN+LSTM", "STGCN")))

loss_plot_val <- ggplot(loss_df_val, aes(x = Epoch, y = Val_Loss, color = Model)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c("ANN" = "#d465d6", "TGCN" = "#9b819c", "GCN+LSTM" = "#ff7f00", "STGCN" = "#1f78b4")
  ) +
  scale_y_log10() +
  labs(
    title = "Validation Loss Comparison",
    x = "Epoch",
    y = "Validation MSE Loss",
    color = "Model"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold") 
  )

loss_plot_val
ggsave(filename = "./Data/Layout/Validation_Loss_Comparison.png", plot = loss_plot_val, width = 6, height = 4, dpi = 300)
