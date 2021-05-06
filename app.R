library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)

choropleth1 = read.csv("choropleth1.csv")
choropleth2 = read.csv("choropleth2.csv")
choropleth3 = read.csv("choropleth3.csv")
choropleth4 = read.csv("choropleth4.csv")
choropleth5 = read.csv("choropleth5.csv")

ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  h3("Caitlin Bolz, Matt Voss, Steven Xia"),
  h5("You can scroll over each county/borough and see which county/borough that 
     is and the number of pickups that there was that week."),
  h4("Uber Pickups in the first week of May. This includes May 1st to 3rd, which 
     were a Thursday, Friday, and Saturday."),
  plotlyOutput(outputId = "Choropleth1"),
  h4("Uber Pickups in the second week of May. This includes May 4th to 10th, which 
     were Monday through Sunday."),
  plotlyOutput(outputId = "Choropleth2"),
  h4("Uber Pickups in the third week of May. This includes May 11th to 117th, which 
     were Monday through Sunday."),
  plotlyOutput(outputId = "Choropleth3"),
  h4("Uber Pickups in the fourth week of May. This includes May 18th to 244th, which 
     were Monday through Sunday."),
  plotlyOutput(outputId = "Choropleth4"),
  h4("Uber Pickups in the fifth week of May. This includes May 25th to 21st, which 
     were Monday through Sunday."),
  plotlyOutput(outputId = "Choropleth5"))

server <- function(input, output) {
  output$Choropleth1 <- renderPlotly({
    plot = ggplot(choropleth1, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_gradient2(low = "yellow", mid = "white", high = "blue",  midpoint = 10000)
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth2 <- renderPlotly({
    plot = ggplot(choropleth2, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_gradient2(low = "yellow", mid = "white", high = "blue",  midpoint = 10000)
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth3 <- renderPlotly({
    plot = ggplot(choropleth3, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_gradient2(low = "yellow", mid = "white", high = "blue",  midpoint = 10000)
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth4 <- renderPlotly({
    plot = ggplot(choropleth4, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
    scale_fill_gradient2(low = "yellow", mid = "white", high = "blue",  midpoint = 10000)
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth5 <- renderPlotly({
    plot = ggplot(choropleth5, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_gradient2(low = "yellow", mid = "white", high = "blue",  midpoint = 10000)
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
}

shinyApp(ui, server)


