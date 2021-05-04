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
