library(leaflet)
nyc_map = function(data) {
  points_by_neighborhood = data %>% 
    group_by(neighborhood) %>%
    summarize(num_points=n())
  map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")
  pal <- colorNumeric(palette = "YlOrRd",domain = range(map_data@data$num_points, na.rm=T))
  leaflet(map_data) %>%
    addTiles() %>% 
    addPolygons(fillColor = ~pal(num_points), popup = ~neighborhood,fillOpacity = .7,
                weight = 1.3) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.98, 40.75, zoom = 13)
}
