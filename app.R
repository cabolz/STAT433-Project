library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)

borough<- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
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
# register_google(key = "AIzaSyB62vo0Ry0KhRaMYc4LW0z2mEF7l25s4LU")

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