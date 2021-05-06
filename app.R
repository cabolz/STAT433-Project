library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)



ui <- fluidPage(
  titlePanel("NYC Uber Pickup"),
  plotlyOutput(outputId = "Choropleth1"),
  plotlyOutput(outputId = "Choropleth2"),
  plotlyOutput(outputId = "Choropleth3"),
  plotlyOutput(outputId = "Choropleth4"),
  plotlyOutput(outputId = "Choropleth5"))

server <- function(input, output) {
  output$Choropleth1 <- renderPlotly({
    plot = ggplot(choropleth1, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Pickups by County/Borough") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth2 <- renderPlotly({
    plot = ggplot(choropleth2, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Pickups by County/Borough") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_viridis_c()
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth3 <- renderPlotly({
    plot = ggplot(choropleth3, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Pickups by County/Borough") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_viridis_c(option = "magma")
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth4 <- renderPlotly({
    plot = ggplot(choropleth4, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Pickups by County/Borough") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_viridis_c(option = "magma")
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
  
  output$Choropleth5 <- renderPlotly({
    plot = ggplot(choropleth5, aes(long, lat, group = borough)) +
      geom_polygon(aes(fill = pickups), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Pickups by County/Borough") +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
      scale_fill_viridis_c(option = "magma")
    ggplotly(plot, width = 700, tooltip = c("borough", "pickups")) %>%
      plotly::config(displayModeBar = T) %>%
      layout(xaxis=list(fixedrange=F),
             yaxis=list(fixedrange=F))
  })
}

shinyApp(ui, server)


