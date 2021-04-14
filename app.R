library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

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
              


server <- function(input, output) {
  

}

shinyApp(ui = ui, server = server)
