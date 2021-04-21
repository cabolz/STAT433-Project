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

UberandWeather

justCountyandBurough<-data.frame(nyc_neighborhoods$neighborhood,nyc_neighborhoods$boroughCode,nyc_neighborhoods$borough,nyc_neighborhoods$id)
justCountyandBurough
