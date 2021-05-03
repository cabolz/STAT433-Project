library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)

borough<- c("New York","Kings","Queens","Bronx","Richmond")
fullData<- read_csv("fullData.csv")
View(fullData)
fullData = fullData %>%
  mutate( 
    borough = case_when(
      str_detect(borough, "Brooklyn") ~ paste("Kings"),
      str_detect(borough, "Manhattan") ~ paste("New York"),
      str_detect(borough, "Staten Island") ~ paste("Richmond"),
      str_detect(borough, "Queens") ~ paste("Queens"),
      str_detect(borough, "Bronx") ~ paste("Bronx")))

fullData %>% filter(day%in%c(1:31))
write_csv(fullData,"fullData.csv")
mapKey = register_google(key = "AIzaSyB62vo0Ry0KhRaMYc4LW0z2mEF7l25s4LU")

# Get lat/long coordinates for NY counties
choropleth = map_data("county") %>% 
  filter(region == "new york") %>% 
  select(-group, -order, - region) %>% 
  rename(County = subregion) %>% 
  mutate(County = str_to_title(County)) %>% 
  filter(County == "New York" | County == "Kings" | County == "Queens" | 
           County == "Bronx" | County == "Richmond")

ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  sidebarLayout(
    sidebarPanel(
      helpText("data from NYC Taxi & Limousine Commission"),
      sliderInput("range","May 2014",value=c(1,31),min=1,max=31),
      selectInput("var","Which borough would you like to see?",borough)),
    mainPanel(plotlyOutput(outputId = "countyChoropleth")))
)

server <- function(input, output) {
  output$countyChoropleth <- renderPlotly({
    
    plot = ggplot(choropleth, aes(long, lat, group = County)) +
      geom_polygon(aes(fill = County), colour = alpha("black", 1/2), size = 0.1)  +
      
      labs (
        title = "Number of Uber Pickups by County") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) 
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 700, tooltip = c("County", "Species", "Density")) %>% 
      config(displayModeBar = T) %>% 
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
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
  filter(borough == "Richmond") %>% 
kings = fullData %>% 
  filter(borough == "Kings")
lats <- kings$Lat
lngs <- kings$Lon
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
