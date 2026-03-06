# This is a further analysis based on STConstructor.R, the result is bad

library(plotly)
library(ks)

cube_data <- gi_data %>%
  left_join(msoa_centroids, by = "msoa21cd") %>%
  filter(!is.na(X) & !is.na(Y))

hotspots_3d <- cube_data %>%
  filter(Gi_star >= 1.96) %>%
  mutate(time_num = as.numeric(time_date)) %>%
  select(X, Y, time_num, Gi_star)


coords <- hotspots_3d %>% select(X, Y, time_num) %>% as.matrix()
weights <- hotspots_3d$Gi_star

bw <- diag(c(3000^2, 3000^2, 365^2))
kd_res <- kde(x = coords, H = bw, w = weights, gridsize = c(300, 300, 300))
grid_x <- kd_res$eval.points[[1]]
grid_y <- kd_res$eval.points[[2]]
grid_z <- kd_res$eval.points[[3]]
volume_data <- expand.grid(X = grid_x, Y = grid_y, Z = grid_z)
volume_data$Density <- as.vector(kd_res$estimate)
min_density_threshold <- quantile(volume_data$Density, 0.75)
max_density <- max(volume_data$Density)

london_sf <- unique_geom 
cube_data_sig$time_date_num <- as.numeric(cube_data_sig$time_date)
z_base_num <- min(cube_data_sig$time_date_num, na.rm = TRUE)


get_plotly_boundaries_df <- function(sf_geom, z_level) {
  lines <- st_cast(sf_geom, "MULTILINESTRING")
  coords <- as.data.frame(st_coordinates(lines))
  coords$unique_path_id <- cumsum(!duplicated(coords[, c("L1", "L2")]))
  
  plot_data <- data.frame()
  for (id in unique(coords$unique_path_id)) {
    path_coords <- coords[coords$unique_path_id == id, c("X", "Y")]
    path_coords[nrow(path_coords) + 1, ] <- NA
    plot_data <- rbind(plot_data, path_coords)
  }
  plot_data$Z <- z_level
  return(plot_data)
}

london_boundary_coords <- get_plotly_boundaries_df(london_sf, z_base_num)
max_z <- max(abs(cube_data_sig$Gi_star), na.rm = TRUE)

fig <- plot_ly() %>%
  add_paths(
    data = london_boundary_coords,
    x = ~X, y = ~Y, z = ~Z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "black", width = 1.5),
    name = "London Boundaries",
    showlegend = FALSE,
    hoverinfo = "none") %>%
  add_trace(
    data = volume_data,
    x = ~X, y = ~Y, z = ~Z,
    value = ~Density,
    type = 'volume',
    isomin = min_density_threshold,
    isomax = max_density,
    opacity = 0.3,
    surface = list(count = 7),
    colorscale = 'Reds',
    caps = list(
      x = list(show = FALSE), 
      y = list(show = FALSE), 
      z = list(show = FALSE)),
    name = "Hotspot Density",
    showscale = TRUE,
    colorbar = list(title = "Density")) %>%
  plotly::layout(
    title = list(text = "3D Space-Time Density Cloud of London Traffic Hotspots", y = 0.95),
    scene = list(
      xaxis = list(title = "Easting (X)", showgrid = TRUE),
      yaxis = list(title = "Northing (Y)", showgrid = TRUE),
      zaxis = list(
        title = "Time",
        tickvals = as.numeric(as.Date(c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2025-01-01"))),
        ticktext = c("2000", "2005", "2010", "2015", "2020", "2025")
      ),
      camera = list(eye = list(x = 1.3, y = -1.3, z = 1.0)),
      aspectmode = "manual", 
      aspectratio = list(x = 1, y = 1, z = 1.2)
    )
  )

fig
