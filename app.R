library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)



ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  sidebarLayout(
    sidebarPanel(
      helpText("data from NYC Taxi & Limousine Commission"),
    sliderInput("range", label = h3("Slider Range for May 2014"), min = 1, 
                max = 31, value = c(10, 20))),
    mainPanel(plotlyOutput(outputId = "countyChoropleth"))))

server <- function(input, output) {
  output$countyChoropleth <- renderPlotly({

    plot = ggplot(choropleth, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Species by County") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )+
      scale_fill_viridis_c(option = "magma")

    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  #choose a borough to be displayed on Choropleth map
  # this.borough = reactive(switch(input$var,
  #                       "Manhattan" = tab$`New York`,
  #                       "Brooklyn" = tab$Kings,
  #                       "Queens" = tab$Queens,
  #                       "Bronx" = tab$Bronx,
  #                       "Staten Island" = tab$Richmond))

}

shinyApp(ui, server)


