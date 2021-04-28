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
library(plotly)

#uber app
borough<- c("NYC","Manhattan","Brooklyn","Queens","Bronx","Staten Island")
fullData<- read_csv("fullData.csv")
fullData %>% filter(day%in%c(1:31))

register_google(key = "AIzaSyB62vo0Ry0KhRaMYc4LW0z2mEF7l25s4LU")

source("helper.R")

ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  sidebarLayout(
    sidebarPanel(
      helpText("data from NYC Taxi & Limousine Commission"),
      sliderInput("range","May 2014",value=c(1,31),min=1,max=31),
      selectInput("var","Which borough would you like to see?",borough)),
    mainPanel(plotOutput("map"))),
  plotlyOutput(outputId = "map")
)


server <- function(input, output) {
  output$map<- renderPlotly({
    data<- switch(input$var,
                  "NYC" = fullData %>% 
                    filter(day%in%c(input$range[1]:input$range[2])),
                  "Manhattan" = fullData%>% 
                    filter(borough=="Manhattan") %>% 
                    filter(day%in%c(input$range[1]:input$range[2])),
                  "Brooklyn" = fullData%>% 
                    filter(borough=="Brooklyn") %>% 
                    filter(day%in%c(input$range[1]:input$range[2])),
                  "Queens" = fullData%>% 
                    filter(borough=="Queens") %>% 
                    filter(day%in%c(input$range[1]:input$range[2])),
                  "Bronx" = fullData%>% 
                    filter(borough=="Bronx") %>% 
                    filter(day%in%c(input$range[1]:input$range[2])),
                  "Staten Island" = fullData%>% 
                    filter(borough=="Staten Island") %>% 
                    filter(day%in%c(input$range[1]:input$range[2])))
    
    plot = ggmap(get_map(location = c(lon = -73.99, lat = 40.74), maptype = "terrain", zoom = 11))
    
    ggplotly(plot, width = 500) %>% 
      config(displayModeBar = T) %>% 
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
    
    })
}

shinyApp(ui, server)

