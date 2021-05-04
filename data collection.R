??setView
# Base layer
leaflet() %>%
  # Base map
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # Centering view on the following coordinates and zooming in.
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)
# Storing the providers inside of the maps variable 
maps <- providers

# Printing the first five
print(head(maps,5))

# Base layer
leaflet() %>%
  # Base map
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # Centering view on the following coordinates and zooming in.
  setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%
  # Adding a marker and a popup.
  addMarkers(lng = -71.0589, lat = 42.3601, popup = 'Sup?')

install.packages(c('raster', 'sf', 'rgdal', 'rgeos'))
library(c('raster', 'sf', 'rgdal', 'rgeos'))
connection = 
  dbConnect(drv = MySQL(), 
            user = "xxx", 
            password = 'xxx', 
            host = 'mybookdatabase.cgac79lt7rx0.us-east-2.rds.amazonaws.com', 
            port = 3306, 
            dbname = 'nikita99')
leaflet() %>%
  # Base map
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # Centering view on NYC this time and zoom out a bit
  setView(lng = -74.0060, lat = 40.7128, zoom = 10) %>% 
  addMarkers(lng = fullData$Lon,
             lat = fullData$Lat,
             popup = fullData$SkyDesciption)



library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
ourShape <- raster::shapefile('shape/ZipCodes.shp')

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = -74.0060, lat = 40.7128, zoom = 11) %>%
  addPolygons(data = ourShape)

leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
set.seed(42)
lats <- 40.7544882 + rnorm(10)/100
lngs <- -73.9879923 + rnorm(10)/200
points <- data.frame(lat=lats, lng=lngs)
points
points_spdf <- points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)
points
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(num_points), popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)

lats <- fullData$Lat
lngs <- fullData$Lon
points <- data.frame(lat=lats, lng=lngs)
points
points_spdf <- points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)
points
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(num_points), popup = ~neighborhood,fillOpacity = .7,
              weight = 1.3) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)

pal <- colorNumeric(palette = "YlOrRd",domain = range(map_data@data$num_points, na.rm=T))

library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)





# data<- switch(input$var,
#               "NYC" = fullData %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])),
#               "Manhattan" = fullData%>% 
#                 filter(borough=="Manhattan") %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])),
#               "Brooklyn" = fullData%>% 
#                 filter(borough=="Brooklyn") %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])),
#               "Queens" = fullData%>% 
#                 filter(borough=="Queens") %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])),
#               "Bronx" = fullData%>% 
#                 filter(borough=="Bronx") %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])),
#               "Staten Island" = fullData%>% 
#                 filter(borough=="Staten Island") %>% 
#                 filter(day%in%c(input$range[1]:input$range[2])))
# 
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
richmond = fullData %>% 
  filter(borough == "Richmond")
kings = fullData %>% 
  filter(borough == "Kings")
new_york = fullData %>% 
  filter(borough == "New York")
queens = fullData %>% 
  filter(borough == "Queens")
bronx = fullData %>% 
  filter(borough == "Bronx")
lats <- bronx$Lat
lngs <- bronx$Lon
points <- data.frame(lat=lats, lng=lngs)
points
points_spdf <- points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)
points
View(points)
trial = points %>%
  left_join(fullData, by = c("lat","lng"))
trial = distinct(trial)
si = points %>% 
  left_join(fullData,by = c("lat","lng"))
si = distinct(si)
ny = points %>% 
  left_join(fullData, by = c("lat","lng"))
ny = distinct(ny)
View(ny)
qu = points %>% 
  left_join(fullData, by = c("lat","lng"))
br = points %>% 
  left_join(fullData, by = c("lat","lng"))
br = distinct(br)
qu = distinct(qu)
View(qu)
View(br)
fullData$lat = fullData$Lat
fullData$lng = fullData$Lon
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)
trial1 = trial
trial1$neighborhood = trial1$neighborhood.x
trial1$boroughCode = trial$boroughCode.x 
trial1 = trial1 %>% 
  transmute(lat,lng, neighborhood,boroughCode,X.id,day)
si1 = si
si1$neighborhood=si1$neighborhood.x
si1$boroughCode=si1$boroughCode.x
ny1 = ny
ny1$neighborhood = ny1$neighborhood.x
ny1$boroughCode = ny1$boroughCode.x
ny1 = ny1 %>% 
  transmute(lat,lng,neighborhood,boroughCode,X.id,day)
View(ny1)
View(si1)
qu1 = qu
qu1$neighborhood = qu1$neighborhood.x
qu1$boroughCode = qu1$boroughCode.x
qu1 = qu1 %>% 
  transmute(lat,lng,neighborhood,boroughCode,X.id,day)
View(qu1)
br1 = br
br1$neighborhood = br1$neighborhood.x
br1$boroughCode = br1$boroughCode.x
si1 = si1 %>% 
  transmute(lat,lng,neighborhood,boroughCode,X.id,day)
br1 = br1 %>% 
  transmute(lat,lng,neighborhood,boroughCode,X.id,day)

points_by_neighborhood <- qu1 %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())
View(points_by_neighborhood)
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(num_points), popup = ~neighborhood,fillOpacity = .7,
              weight = 1.3) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)

pal <- colorNumeric(palette = "YlOrRd",domain = range(map_data@data$num_points, na.rm=T))

?bootstrapPage
write_csv(trial1,"kingsborough.csv")
write_csv(si1,"richmondborough.csv")
write_csv(ny1,"newyorkborough.csv")
write_csv(qu1,"queensborough.csv")
write_csv(br1,"bronxborough.csv")
a = read_csv("bronxborough.csv")
View(a)
shinyApp(ui,server)
