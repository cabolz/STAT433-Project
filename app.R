library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.78), maptype = "terrain", zoom = 11)
ggmap(nyc_map)
register_google(key = "AIzaSyB62vo0Ry0KhRaMYc4LW0z2mEF7l25s4LU")
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
summary(nyc_neighborhoods)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))
ggmap(nyc_map) + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA)
nyc_neighborhoods_df$id

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
points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
manhattan_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
Staten_Island_map<- get_map(location = c(lon = -74.15, lat = 40.58), maptype = "terrain", zoom = 12)
ggmap(Staten_Island_map)
brooklyn_map <- get_map(location = c(lon = -73.92, lat = 40.66), maptype = "terrain", zoom =12)
ggmap(brooklyn_map)
bronx_map <- get_map(location = c(lon = -73.88, lat = 40.83), maptype = "terrain", zoom = 12)
ggmap(bronx_map)
queens_map <- get_map(location = c(lon = -73.81, lat = 40.72), maptype = "terrain", zoom = 11)
ggmap(queens_map)
UberandWeather
nyc_neighborhoods_df
justCountyandBurough<-data.frame(nyc_neighborhoods$neighborhood,nyc_neighborhoods$boroughCode,nyc_neighborhoods$borough,nyc_neighborhoods$id)
justCountyandBurough<-mutate(justCountyandBurough,id = nyc_neighborhoods.id)
justCountyandBurough
justCountyand<-justCountyandBurough %>% 
  full_join(nyc_neighborhoods_df)
justCountyand
slender<-justCountyand %>% 
  transmute(nyc_neighborhoods.neighborhood,nyc_neighborhoods.boroughCode,nyc_neighborhoods.borough,id,
            long = round(long,4), lat = round(lat,4),order,hole,piece,group)
View(slender)
tail(slender)

set.seed(42)
lats <- 40.7 + rnorm(10)/100
lngs <- -73.79 + rnorm(10)/200
pointsa <- data.frame(lat=lats, lng=lngs)
pointsa
points_spdfa <- pointsa
coordinates(points_spdfa) <- ~lng + lat
proj4string(points_spdfa) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdfa, nyc_neighborhoods)
pointsa <- cbind(points, matches)
pointsa
points_by_neighborhooda <- pointsa %>%
  group_by(neighborhood) %>%
  summarize(num_pointsa=n())
map_dataa <- geo_join(nyc_neighborhoods, points_by_neighborhooda, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_dataa@data$num_points, na.rm=T))
plot_dataa <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhooda, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
queens_map <- get_map(location = c(lon = -73.81, lat = 40.72), maptype = "terrain", zoom = 11)
ggmap(queens_map) + 
  geom_polygon(data=plot_dataa, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)



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
points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
manhattan_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)

#Queens Borough info
lati<- W %>% transmute(W$lat)
longti<- W %>% transmute(W$long)
coor <- data.frame("lat" = lati, "long" = longti)
coor <- coor %>% transmute(lat = W.lat, long = W.long)
coor
coor_spdf=coor
coordinates(coor_spdf)<- ~long + lat
proj4string(coor_spdf)<- proj4string(nyc_neighborhoods)
matches <- over(coor_spdf, nyc_neighborhoods)
coor <- cbind(coor,matches)
coor
queen <- coor %>% 
  filter(borough=="Queens")
queen
points_by_neighborhood <- queen %>% 
  group_by(neighborhood) %>% 
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood,"neighborhood", "neighborhood")
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
manhattan_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
queens_map <- get_map(location = c(lon = -73.81, lat = 40.72), maptype = "terrain", zoom = 11)
ggmap(queens_map) +
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
fullQueensData <- coor %>% 
  full_join(W)

#Brooklyn Borough
latb=W %>% 
  transmute(W$lat)
lonb=W %>% 
  transmute(W$long)
coorb <- data.frame("lat" = latb, "long" = lonb)
coorb <- coor %>% transmute(lat = W.lat, long = W.long)
coorb
coor_spdfb=coorb
coordinates(coor_spdfb)<- ~long + lat
proj4string(coor_spdfb)<- proj4string(nyc_neighborhoods)
matches <- over(coor_spdfb, nyc_neighborhoods)
coorb <- cbind(coorb,matches)
coorb
brooklyn <- coorb %>% 
  filter(borough=="Brooklyn")
brooklyn
points_by_neighborhood <- brooklyn%>% 
  group_by(neighborhood) %>% 
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood,"neighborhood", "neighborhood")
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
brooklyn_map <- get_map(location = c(lon = -73.92, lat = 40.66), maptype = "terrain", zoom =12)
ggmap(brooklyn_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
fullBrooklynData<-coorb %>% 
  full_join(W)
fullBrooklynData


