
msoa_total_check <- msoa_daily_counts %>%
  group_by(msoa21cd) %>%
  summarise(total_count = sum(count), .groups = "drop")

map_for_verification <- london_lsoa %>%
  select(lsoa21cd, msoa21cd) %>%
  left_join(msoa_total_check, by = "msoa21cd") %>%
  mutate(total_count = replace_na(total_count, 0))

tm_shape(map_for_verification) +
  tm_fill("total_count", palette = "Reds", title = "Total Accidents") +
  tm_borders() +
  tm_layout(title = "Total Accidents by MSOA (Joined Back to LSOA)")


map_for_verification%>%dim()
map_for_verification%>%
  group_by(msoa21cd)%>%
  summarise(n())%>%
  dim()
