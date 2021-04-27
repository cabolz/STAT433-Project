library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(shiny)

#uber app
borough<- c("NYC","Manhattan","Brooklyn","Queens","Bronx","Staten Island")
ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  sidebarLayout(
    sidebarPanel(
    helpText("data from NYC Taxi & Limousine Commission"),
    sliderInput("rng","May 2014",value=c(1,31),min=1,max=31),
    selectInput("borough","Which borough would you like to see?",borough))
  ),
  mainPanel(plotOutput("map"))
)


server <- function(input, output, session) {
  output$map<- renderPlot({
    
  })
}


shinyApp(ui, server)


