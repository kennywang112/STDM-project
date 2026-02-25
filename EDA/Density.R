# https://bookdown.org/nicohahn/making_maps_with_r5/docs/ggplot2.html#using-ggplot2-to-create-maps
library(scico)
library(tidyverse)

ggplot() +
  geom_sf(
    data = londona_geom,
    fill = NA,
    color = "white",
    size = 0.1) +
  coord_sf() +
  theme_void() +
  stat_density_2d(
    data = accidents_joined,
    aes(
      x = location_easting_osgr, 
      y = location_northing_osgr, 
      fill = after_stat(density),
      alpha = after_stat(density)),
    geom = "raster",
    contour = FALSE,
    n = 800) +
  scale_alpha(name = "", range = c(0.1, 0.7), guide = "none") + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    text = element_text(color = "white")) +
  labs(x = NULL, y = NULL, title = "Traffic Collision Density") + 
  scale_fill_scico(
    palette = "davos", 
    direction = -1,
    name = "Collision Density",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(50, "mm"),
      barheight = unit(2, "mm"),
      title.position = 'top',
      title.hjust = 0.5
      )
  )
