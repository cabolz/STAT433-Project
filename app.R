library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

# Load in dataset
UberandWeather = read_csv("UberandWeather.csv")

ui<-fluidPage(
  titlePanel("NYC Map"),
  
  sidebarLayout(
    mainPanel(leaflet() %>%
                addTiles() %>%
                setView(-74.00, 40.71, zoom = 12))  
  )
)

?leafletOutput #I'm using the example from this help question and changed the coordinates

NewYorkCityMap<-shinyApp(
  ui<-fluidPage(leafletOutput('NYCMap'),titlePanel("NYC Map")),
  server <- function(input,output){
    map<-leaflet() %>% addTiles() %>% setView(-74.00,40.71, zoom =11)
    output$NYCMap<-renderLeaflet(map)
  }
)              
runApp(NewYorkCityMap)

data

NYC<-leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 10)
NYC
# Define UI for app that draws a histogram ----
# found this on the internet, I just want to map the map and add the points later with nyc data
?leafletOutput

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
    
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points())
    })
  }

shinyApp(ui, server)

app <- shinyApp(
  ui = fluidPage(leafletOutput('Map')),
  server = function(input, output) {
    map = leaflet() %>% addTiles() %>% setView(-97.65, 42.0285, zoom = 17)
    output$Map = renderLeaflet(map)
  }
)
runApp(app)