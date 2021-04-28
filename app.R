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
      sliderInput("range","May 2014",value=c(1,31),min=1,max=31),
      selectInput("var","Which borough would you like to see?",borough)),
    mainPanel(plotOutput("map")))
)


server <- function(input, output, session) {
  output$map<- renderPlot({
    data<- switch(input$var,
                  "NYC" = fullData %>% 
                    filter(fullData$day>=input$range[1]&&fullData$range<=input$range[2]),
                  "Manhattan" = fullData%>% 
                    filter(borough=="Manhattan") %>% 
                    filter(fullData$day>=input$range[1]&&fullData$day<=input$range[2]),
                  "Brooklyn" = fullData%>% 
                    filter(borough=="Brooklyn") %>% 
                    filter(fullData$day>=input$range[1]&&fullData$day<=input$range[2]),
                  "Queens" = fullData%>% 
                    filter(borough=="Queens") %>% 
                    filter(fullData$day>=input$range[1]&&fullData$day<=input$range[2]),
                  "Bronx" = fullData%>% 
                    filter(borough=="Bronx") %>% 
                    filter(fullData$day>=input$range[1]&&fullData$day<=input$range[2]),
                  "Staten Island" = fullData%>% 
                    filter(borough=="Staten Island") %>% 
                    filter(fullData$day>=input$range[1]&&fullData$day<=input$range[2]))
    print(data)
  })
}

shinyApp(ui, server)


# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("integer", "Integer:",
                  min = 0, max = 1000,
                  value = 500),
      
      # Input: Decimal interval with step value ----
      sliderInput("decimal", "Decimal:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1, max = 1000,
                  value = c(200,500)),
      
      # Input: Custom currency format for with basic animation ----
      sliderInput("format", "Custom Format:",
                  min = 0, max = 10000,
                  value = 0, step = 2500,
                  pre = "$", sep = ",",
                  animate = TRUE),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("animation", "Looping Animation:",
                  min = 1, max = 2000,
                  value = 1, step = 10,
                  animate =
                    animationOptions(interval = 300, loop = TRUE))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Integer",
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer,
                             input$decimal,
                             paste(input$range, collapse = " "),
                             input$format,
                             input$animation)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}
