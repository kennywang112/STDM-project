library(spdep)

W_list <- nb2listw(nb_list, style = "W", zero.policy = TRUE)

moran_test <- moran.test(spatial_data$accident_count, W_list, zero.policy = TRUE)
# moran.mc(x=spatial_data$accident_count, listw=W_list, nsim=9999)
print(moran_test)

lmoran <- localmoran(spatial_data$accident_count, W_list, zero.policy = TRUE)

spatial_data$Ii <- lmoran[, 1] # Local Moran statistics
spatial_data$E.Ii<- lmoran[, 2] # Expectation of local moran statistic
spatial_data$Var.Ii<- lmoran[, 3] # Variance of local moran statistic
spatial_data$Z.Ii<- lmoran[, 4] # Standard deviate of local moran statistic
spatial_data$P.Ii <- lmoran[, 5] # P-value

spatial_data$scaled_obs <- scale(spatial_data$accident_count) %>% as.vector()
spatial_data$lag_obs <- lag.listw(W_list, spatial_data$scaled_obs)

alpha <- 0.05
spatial_data <- spatial_data %>% 
  mutate(
    type = case_when(
      scaled_obs > 0 & lag_obs > 0 ~ "High-High",
      scaled_obs < 0 & lag_obs < 0 ~ "Low-Low",
      scaled_obs < 0 & lag_obs > 0 ~ "Low-High",
      scaled_obs > 0 & lag_obs < 0 ~ "High-Low",
      TRUE ~ "Undefined"
    ),
    cluster_sig = ifelse(P.Ii < alpha, type, "Not Significant")
  )

lisa_colors <- c(
  "High-High" = "red", 
  "Low-Low" = "blue", 
  "Low-High" = "lightblue", 
  "High-Low" = "pink", 
  "Not Significant" = "grey90"
)

tmap_mode("plot")
tm_shape(spatial_data) + 
  tm_polygons(col = "cluster_sig", 
              palette = lisa_colors,
              title = "Local Moran's I Clusters",
              border.alpha = 0.3) +
  tm_layout(main.title = "Accident Hotspots in London (LISA)",
            legend.outside = TRUE)

moran_plot_data <- spatial_data %>%
  st_drop_geometry() %>%
  mutate(
    z_score = scaled_obs,
    lag_z = lag_obs,
    quadrant = case_when(
      cluster_sig == "Not Significant" ~ "Not Significant",
      TRUE ~ cluster_sig
    )
  )

moran_i_value <- round(moran_test$estimate[1], 3)
label_text <- paste("Regression Line\nSlope =", moran_i_value)

ggplot(moran_plot_data, aes(x = z_score, y = lag_z)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  geom_point(aes(color = quadrant), alpha = 0.8, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 1) +
  annotate("text", x = min(moran_plot_data$z_score), y = max(moran_plot_data$lag_z), 
           label = label_text, hjust = 0, vjust = 1, 
           color = "black", size = 4, fontface = "bold") +
  scale_color_manual(values = lisa_colors) +
  labs(title = "Moran Scatter Plot (Accidents)",
       x = "Standardized Num of Accidents (Z-score)",
       y = "Spatial Lag of Accidents (Z-score)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")
