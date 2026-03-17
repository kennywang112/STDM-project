library(tmap)
library(spdep)
source('utils/read_data.R')
source('utils/map_func.R')

rt <- readRDS("./Data/spatial_data.rds")
final_data <- rt[[2]]

# accident <- st_read("./Data/accident_2000.gpkg")
mapped_df # this is same as accident but mapped throuh code list
accidents_joined <- st_join(mapped_df, london_lsoa_4326, left = FALSE)%>%
  st_transform(27700)

cscale <- 'msoa21cd'
if (cscale == 'lad22cd') {
  londona_geom <- london_lad_geom
  pop <- rt[[1]]
}else if (cscale == 'msoa21cd') {
  londona_geom <- london_msoa_geom
  pop <- rt[[1]]
} else if (cscale == 'lsoa21cd') {
  londona_geom <- london_lsoa
  pop <- rt[[1]]
}

road_type <- c("Single carriageway", "Dual carriageway", "Roundabout", "One way street", "Slip road")

junction_type <- c("Crossroads", "Junction with more than four arms (not roundabout)",
                   "Not at junction or within 20 metres", "T or staggered junction", 
                   "Using private drive or entrance")

map_plots <- list()

for(i in road_type){
  cat(paste0("Processing road type: ", i, "\n"))
  # if('/' %in% i) {
  #   print(i)
  #   # remove the backslash from the road type
  #   i <- gsub("/", "", i)
  # }
  msoa_counts <- accidents_joined %>%
    filter(road_type == i)%>%
    # filter(junction_detail == i)%>%
    st_drop_geometry() %>%   
    filter(!is.na(.data[[cscale]])) %>%
    group_by(.data[[cscale]]) %>%
    summarise(accident_count = n())
  
  london_stats <- londona_geom %>%
    left_join(msoa_counts, by = cscale) %>%
    mutate(accident_count = replace_na(accident_count, 0))

  # title <- paste0("Accidents per LSOA for Road Type: ", i)

  tmap_mode("plot")
  map_plot <- tm_shape(london_stats) +
    add_map_decorations_polygon("accident_count", i)
  map_plots[[i]] <- map_plot
  # tmap_save(map_plot, filename = paste0("Data/Layout/London_Accidents_",i,"_",cscale,".png", sep=''), width = 8, height = 6, dpi = 300)
}

library(gridExtra)
road_accident_map <- tmap_arrange(map_plots[[1]], map_plots[[2]], map_plots[[3]], map_plots[[4]], map_plots[[5]], ncol = 2)
road_accident_map

tmap_save(road_accident_map, filename = "Data/Layout/London_Accidents_road_type.png", 
          width = 12, height = 16, dpi = 300)


