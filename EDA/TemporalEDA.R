source('utils/read_data.R')
source('utils/starima_package.R')

# time_scale <- "week"
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

weekly_acc <- daily_acc%>%
  # group by month
  group_by(month = floor_date(time_date, "week"))%>%
  summarise(monthly_accidents = sum(accident_count))%>%
  ggplot(aes(x=month, y=monthly_accidents)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # draw curve
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
  theme_bw() +
  ggtitle("Weekly Accident Counts with LOESS Smoothing") +
  xlab("Week") +
  ylab("Total Accidents")

ggsave("Data/Layout/weekly_acc.png", weekly_acc, width = 10, height = 5)

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

# accumulate by month
weekly_data <- daily_acc%>%
  group_by(month = floor_date(time_date, "week"))%>%
  summarise(monthly_accidents = sum(accident_count))

p_acf_weekly <- ggAcf(weekly_data$monthly_accidents, lag.max = 60) + 
  ggtitle("Weekly Accident Counts ACF") +
  theme_bw()

p_pacf_weekly <- ggPacf(weekly_data$monthly_accidents, lag.max = 60) + 
  ggtitle("Weekly Accident Counts PACF") + 
  theme_bw()

acf_pacf_weekly <- grid.arrange(p_acf_weekly, p_pacf_weekly, ncol = 2)
ggsave("Data/Layout/ACF_PACF_ggplot_weekly.png", acf_pacf_weekly, width = 10, height = 5)
