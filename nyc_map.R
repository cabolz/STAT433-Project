library(ggmap)
library(readr)

nyc_map <- get_map(location = c(lon = -73.99, lat = 40.74), maptype = "terrain", zoom = 11)
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

UberandWeather = read_csv("UberandWeather.csv")
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


#Full Data set with buroughs 
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
head(coor)
points_by_neighborhood <- coor %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")
?colorNumeric
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
nyc_map <- get_map(location = c(lon = -73.99, lat = 40.74), maptype = "terrain", zoom = 11)
wholeNYC<- ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=log(num_points)), alpha=0.75)
coor1<- data.frame(coor$lat,coor$long,coor$neighborhood,coor$boroughCode,coor$borough)
coor2<- coor1 %>% 
  transmute(Lat = coor.lat,Lon = coor.long,neighborhood = coor.neighborhood, boroughCode = coor.boroughCode,
            borough = coor.borough)
coor2
coor2<- coor2 %>% 
  transmute(hour,minute,Lat,Lon,day,Humidity,Skydesciption,temperature,pressure,windspeed,neighborhood,borough)
View(coor2)
coor2<- na.omit(coor2)
fullData<- UberandWeather %>% 
  right_join(coor2) %>% 
  right_join(nyc_neighborhoods_df$id)
fullData
View(fullData)
fullData<- distinct(fullData)
fullData
wholeNYC

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

#Manhattan Data
latm=W %>% 
  transmute(W$lat)
lonm=W %>% 
  transmute(W$long)
coorm <- data.frame("lat" = latm, "long" = lonm)
coorm <- coor %>% transmute(lat = W.lat, long = W.long)
coorm
coor_spdfm=coorm
coordinates(coor_spdfm)<- ~long + lat
proj4string(coor_spdfm)<- proj4string(nyc_neighborhoods)
matches <- over(coor_spdfm, nyc_neighborhoods)
coorm <- cbind(coorm,matches)
coorm
manhattan <- coorm %>% 
  filter(borough=="Manhattan")
manhattan
points_by_neighborhood <- manhattan%>% 
  group_by(neighborhood) %>% 
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood,"neighborhood", "neighborhood")
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
Manhattan_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)
ggmap(Manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
fullManhattanData<-coorm %>% 
  full_join(W)
fullManhattanData

#Bronx borough
latbr=W %>% 
  transmute(W$lat)
lonbr=W %>% 
  transmute(W$long)
coorbr <- data.frame("lat" = latbr, "long" = lonbr)
coorbr <- coorbr %>% transmute(lat = W.lat, long = W.long)
coorbr
coor_spdfbr=coorbr
coordinates(coor_spdfbr)<- ~long + lat
proj4string(coor_spdfbr)<- proj4string(nyc_neighborhoods)
matches <- over(coor_spdfbr, nyc_neighborhoods)
coorbr <- cbind(coorbr,matches)
coorbr
bronx <- coorbr %>% 
  filter(borough=="Bronx")
bronx
points_by_neighborhood <- bronx%>% 
  group_by(neighborhood) %>% 
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood,"neighborhood", "neighborhood")
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
bronx_map <- get_map(location = c(lon = -73.88, lat = 40.83), maptype = "terrain", zoom = 12)
ggmap(bronx_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
fullBronxData<-coorbr %>% 
  full_join(W) %>% 
  filter(borough == 'Bronx')
fullBronxData

#Staten Island
latsi=W %>% 
  transmute(W$lat)
lonsi=W %>% 
  transmute(W$long)
coorsi <- data.frame("lat" = latsi, "long" = lonsi)
coorsi <- coorsi %>% transmute(lat = W.lat, long = W.long)
coorsi
coor_spdfsi=coorsi
coordinates(coor_spdfsi)<- ~long + lat
proj4string(coor_spdfsi)<- proj4string(nyc_neighborhoods)
matches <- over(coor_spdfsi, nyc_neighborhoods)
coorsi <- cbind(coorsi,matches)
coorsi
statenisland <- coorsi %>% 
  filter(borough=="Staten Island")
statenisland
points_by_neighborhood <- statenisland%>% 
  group_by(neighborhood) %>% 
  summarize(num_points=n())
map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood,"neighborhood", "neighborhood")
pal <- colorNumeric(palette = "RdBu",
                    domain = range(map_data@data$num_points, na.rm=T))
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., points_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))
Staten_Island_map<- get_map(location = c(lon = -74.15, lat = 40.58), maptype = "terrain", zoom = 12)
ggmap(Staten_Island_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
fullStatenIslandData<-coorsi %>% 
  full_join(W)
fullStatenIslandData

ggmap(Staten_Island_map) + 
  geom_point(data = plot_data, aes(x=long,y=lat))




