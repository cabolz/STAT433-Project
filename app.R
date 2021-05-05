library(dplyr)
library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(stringr)
library(tidyverse)

borough<- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
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
      sliderInput("range","May 2014",value=c(1,31),min=1,max=31)),
      #selectInput("var","Which borough would you like to see?",borough)),
    mainPanel(plotlyOutput(outputId = "countyChoropleth"))))

server <- function(input, output) {
  output$countyChoropleth <- renderPlotly({

    plot = ggplot(choropleth, aes(long, lat, group = County)) +
      geom_polygon(aes(fill = County), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Species by County") +
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
library(plotly)
library(rjson)
r1 = 'http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson'
geojson <- rjson::fromJSON(file=r1)
full = fullData %>% 
  transmute(borough,neighborhood,day) %>% 
  group_by(borough,day,neighborhood) %>% 
  summarize(num_points= n(),borough,day)
full = distinct(full)
full
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=geojson,
  locations=full$neighborhood,
  z=full$num_points,
  colorscale="Viridis",
  featureidkey = "properties.neighborhood"
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =9,
    center=list(lon = -73.98,lat =  40.75))
)
fig
