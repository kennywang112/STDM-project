source('utils/boundaries.R')
source('utils/read_data.R')
source('utils/starima_package.R')

cscale <- 'lad22cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- lad_pop
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- msoa_pop
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- lsoa_pop
}
time_scale <- "week"
plot_data <- accidents_joined %>%
  mutate(date_parsed = dmy(date), month_year = floor_date(date_parsed, time_scale)) %>%
  group_by(month_year) %>%
  summarise(count = n(), .groups = "drop")

ggplot(plot_data, aes(x = month_year, y = count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = time_scale) +
  labs(title = "2025 Weekly Accident Counts by Road Type",
       x = "Month",
       y = "Number of Accidents",
       color = "Road Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
accidents_joined%>%
  # mutate(day_of_week = factor(day_of_week, levels = day_levels))%>%
  group_by(day_of_week, collision_severity)%>%
  summarise(count=n())%>%
  ggplot()+
  geom_bar(aes(x=day_of_week, y=count, fill=collision_severity), stat='identity', position='dodge')

temp_data <- accidents_lsoa %>%
  mutate(date_parsed = dmy(date), month_year = floor_date(date_parsed, time_scale)) %>%
  group_by(month_year) %>%
  summarise(count = n(), .groups = "drop")

temp_1 <- ggplot(temp_data, aes(x = month_year, y = count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(title = "2025 Weekly Accident Counts by Road Type",
       x = "Data",
       y = "Number of Accidents",
       color = "Road Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

temp_data_lag <- temp_data %>%
  arrange(month_year) %>% 
  mutate(t = count, 
         t_minus_1 = lag(count, n = 1),
         t_minus_7 = lag(count, n = 7),
         t_minus_28 = lag(count, n = 28))%>%
  filter(!is.na(t_minus_1) & !is.na(t_minus_7) & !is.na(t_minus_28))

temp_2 <- temp_data_lag%>%
  ggplot(aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+
  ggplot2::annotate("text", 
                    x = min(temp_data_lag$t), 
                    y = max(temp_data_lag$t_minus_1),
                    hjust = 0, vjust = 1,
                    label = paste("r =", round(cor(temp_data_lag$t, temp_data_lag$t_minus_1), 3)))
library(gridExtra)
grid.arrange(temp_1, temp_2, nrow=1)
ggsave(file = "Data/Layout/Temporal.png", arrangeGrob(grobs = c(temp_1, temp_2), ncol = 2), width = 15, height = 6, dpi = 300)


# Temporal analysis
acf_plot <- acf(temp_data$count, lag.max = 30, main = "Accident Counts ACF")
pacf_plot <- pacf(temp_data$count, lag.max = 30, main = "Accident Counts PACF")

# same as above
library(forecast)
p_acf <- ggAcf(temp_data$count, lag.max = 30) + 
  ggtitle("Accident Counts ACF") +
  theme_bw()

p_pacf <- ggPacf(temp_data$count, lag.max = 30) + 
  ggtitle("Accident Counts PACF") + 
  theme_bw()

acf_pacf <- grid.arrange(p_acf, p_pacf, ncol = 2)
ggsave("Data/Layout/ACF_PACF_ggplot.png", acf_pacf, width = 10, height = 5)
