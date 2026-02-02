add_map_decorations <- function() {
  tm_compass(position = c("right", "top")) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_grid(labels.size = 0.7, n.x = 5, n.y = 5,
            lwd = 0.1,
            alpha = 0.5,
            labels.inside.frame = FALSE,
            projection = 27700) +
    tm_layout(
      main.title.size = 1,
      legend.outside = FALSE,
      legend.position = c("right", "bottom"),
      legend.bg.color = "white",
      legend.bg.alpha = 0.5,
      legend.frame = TRUE
    )
}
