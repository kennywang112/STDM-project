library(spdep)

accident <- read_csv('./Data/dft-road-casualty-statistics-collision-provisional-2025.csv') %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(27700)
accidents_lsoa <- st_join(accident, london_lsoa)

lsoa_counts <- accidents_with_lsoa %>%
  st_drop_geometry() %>%   
  filter(!is.na(lsoa21cd)) %>%
  group_by(lsoa21cd) %>%
  summarise(accident_count = n())

london_lsoa_stats <- london_lsoa %>%
  left_join(lsoa_counts, by = "lsoa21cd") %>%
  mutate(accident_count = replace_na(accident_count, 0))%>%
  left_join(lsoa_pop, by=c("lsoa21cd"="lsoa21cd"))%>%
  mutate(accidents_per_1000 = (accident_count / population) * 1000)

tmap_mode("plot")
map1 <- tm_shape(london_lsoa_stats) +
  tm_polygons(col = "accident_count", 
              style = "jenks",
              palette = "Reds", 
              title = "Accidents per LSOA",
              border.alpha = 0.1)

map2 <- tm_shape(london_lsoa_stats) +
  tm_polygons(col = "accidents_per_1000", 
              style = "jenks",
              palette = "Reds",
              border.alpha = 0.1)

tmap_arrange(map1, map2, ncol = 2)

# Correlation plot
library(ggpubr)
ggplot(london_lsoa_stats, aes(x = population, y = accident_count)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  stat_cor(method = "pearson", label.x = 1000, label.y = 50)

# plot neighbor
nb_list <- poly2nb(london_lsoa)
W_list <- nb2listw(nb_list, style = "W", zero.policy = TRUE)

tmap_mode("plot")
coords <- st_coordinates(st_centroid(london_lsoa))
nb_lines <- nb2lines(neighbours, coords = coords, proj4string = st_crs(london_lsoa)$proj4string)
nb_lines_sf <- st_as_sf(nb_lines)

tm_shape(london_lsoa) +
  tm_borders(col = "lightgrey") +
  tm_shape(nb_lines_sf) +
  tm_lines(col = "red", alpha = 0.5)


## morans
moran_test <- moran.test(london_lsoa_stats$accident_count, W_list, zero.policy = TRUE)
# moran.mc(x=london_lsoa_stats$accident_count, listw=W_list, nsim=9999)
print(moran_test)

lmoran <- localmoran(london_lsoa_stats$accident_count, W_list, zero.policy = TRUE)

london_lsoa_stats$Ii <- lmoran[, 1] # Local Moran statistics
london_lsoa_stats$E.Ii<- lmoran[, 2] # Expectation of local moran statistic
london_lsoa_stats$Var.Ii<- lmoran[, 3] # Variance of local moran statistic
london_lsoa_stats$Z.Ii<- lmoran[, 4] # Standard deviate of local moran statistic
london_lsoa_stats$P.Ii <- lmoran[, 5] # P-value

london_lsoa_stats$scaled_obs <- scale(london_lsoa_stats$accident_count) %>% as.vector()
london_lsoa_stats$lag_obs <- lag.listw(W_list, london_lsoa_stats$scaled_obs)

alpha <- 0.05
london_lsoa_stats <- london_lsoa_stats %>% 
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
tm_shape(london_lsoa_stats) + 
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

moran <- moran.plot(london_lsoa_stats$accident_count, listw = W_list)
# ggplot(moran_plot_data, aes(x = z_score, y = lag_z)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
#   geom_point(aes(color = quadrant), alpha = 0.8, size = 1.5) +
#   geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 1) +
#   annotate("text", x = min(moran_plot_data$z_score), y = max(moran_plot_data$lag_z), 
#            label = label_text, hjust = 0, vjust = 1, 
#            color = "black", size = 4, fontface = "bold") +
#   scale_color_manual(values = lisa_colors) +
#   labs(title = "Moran Scatter Plot (Accidents)",
#        x = "Standardized Num of Accidents (Z-score)",
#        y = "Spatial Lag of Accidents (Z-score)") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"),
#         legend.position = "bottom")
