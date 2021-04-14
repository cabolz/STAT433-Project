library(shiny)
ui<-fluidPage(
  titlePanel("NYC Map"),
  sidebarLayout(
  mainPanel(leaflet() %>%
              addTiles() %>%
              setView(-74.00, 40.71, zoom = 12))  
            )
              )
shinyApp(ui)