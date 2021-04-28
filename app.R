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
source("helper.R")
ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  sidebarLayout(
    sidebarPanel(
    helpText("data from NYC Taxi & Limousine Commission"),
    sliderInput("rng","May 2014",value=c(1,31),min=1,max=31),
    selectInput("var","Which borough would you like to see?",borough))
  ),
  mainPanel(plotOutput("map"))
)


server <- function(input, output, session) {
  output$map<- renderPlot({
    data<- switch(input$var,
                  "NYC" = fullData,
                  "Manhattan" = fullData%>% filter(borough=="Manhattan"),
                  "Brooklyn" = fullData%>% filter(borough=="Brooklyn"),
                  "Queens" = fullData%>% filter(borough=="Queens"),
                  "Bronx" = fullData%>% filter(borough=="Bronx"),
                  "Staten Island" = fullData%>% filter(borough=="Staten Island"))
  View(fullData)
  })
}


shinyApp(ui, server)


