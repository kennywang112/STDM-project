source('utils/read_data.R')
source('utils/starima_package.R')

time_scale <- "day"
# rt_day <- read_final_data('MSOA', time_scale)
# saveRDS(rt_day, file = "./Data/spatial_data_day.rds")
rt_day <- readRDS("./Data/spatial_data_day.rds")

final_data <- rt_day[[2]]

cscale <- 'msoa21cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- rt_day[[1]]
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- rt_day[[1]]
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- rt_day[[1]]
}

daily_acc <- final_data%>%
  st_drop_geometry() %>%
  filter(time_date >= as.Date("2010-01-01"))%>%
  group_by(time_date)%>%
  summarise(accident_count = sum(accident_count))
  
daily_acc%>%
  ggplot(aes(x=time_date, y=accident_count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")


# same as above
library(forecast)
library(gridExtra)
p_acf <- ggAcf(daily_acc$accident_count, lag.max = 30) + 
  ggtitle("Accident Counts ACF") +
  theme_bw()

p_pacf <- ggPacf(daily_acc$accident_count, lag.max = 30) + 
  ggtitle("Accident Counts PACF") + 
  theme_bw()

acf_pacf <- grid.arrange(p_acf, p_pacf, ncol = 2)
ggsave("Data/Layout/ACF_PACF_ggplot.png", acf_pacf, width = 10, height = 5)
