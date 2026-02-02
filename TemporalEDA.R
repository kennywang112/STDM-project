source('utils/boundaries.R')
source('utils/read_data.R')

mapped_df
mapped_df%>%colnames()

plot_data <- mapped_df %>%
  mutate(date_parsed = dmy(date), month_year = floor_date(date_parsed, "week")) %>%
  group_by(month_year, road_type) %>%
  summarise(count = n(), .groups = "drop")

ggplot(plot_data, aes(x = month_year, y = count, color = road_type, group = road_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(title = "2025 Weekly Accident Counts by Road Type",
       x = "Month",
       y = "Number of Accidents",
       color = "Road Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mapped_df%>%
  mutate(day_of_week = factor(day_of_week, levels = day_levels))%>%
  group_by(day_of_week, collision_severity)%>%
  summarise(count=n())%>%
  ggplot()+
  geom_bar(aes(x=day_of_week, y=count, fill=collision_severity), stat='identity', position='dodge')


temp_data <- mapped_df %>%
  mutate(date_parsed = dmy(date), month_year = floor_date(date_parsed, "day")) %>%
  group_by(month_year) %>%
  summarise(count = n(), .groups = "drop")

temp_1 <- ggplot(temp_data, aes(x = month_year, y = count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(title = "2025 Weekly Accident Counts by Road Type",
       x = "Month",
       y = "Number of Accidents",
       color = "Road Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

temp_data_lag <- temp_data %>%
  arrange(month_year) %>% 
  mutate(t = count, t_minus_1 = lag(count, n = 1))
filtered_temp_data <- temp_data_lag%>%filter(!is.na(t_minus_1))
temp_2 <- filtered_temp_data%>%
  ggplot(aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+
  ggplot2::annotate("text", 
                    x = min(filtered_temp_data$t), 
                    y = max(filtered_temp_data$t_minus_1),
                    hjust = 0, vjust = 1,
                    label = paste("r =", round(cor(filtered_temp_data$t, filtered_temp_data$t_minus_1), 3)))
library(gridExtra)
grid.arrange(temp_1, temp_2, nrow=1)
ggsave(file = "Data/Layout/Temporal.png", arrangeGrob(grobs = c(temp_1, temp_2), ncol = 2))


# Temporal analysis
acf(temp_data$count, lag.max = 30, main = "Accident Counts ACF")
pacf(temp_data$count, lag.max = 30, main = "Accident Counts PACF")
stacf(temp_data$count, main = "Accident Counts STACF")
