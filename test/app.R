library(shiny)
library(dplyr)
library(leaflet)

dat <- data.frame(lat = 39.5,
                  lng = -98.35,
                  message = "Hi, world!",
                  date = Sys.Date(),
                  count = 32,
                  stringsAsFactors = FALSE)

# here's where I make the html-ready compound strings for the labels
dat$label <- with(dat, paste(
  "<p> <b>", message, "</b> </br>",
  date, "</br>",
  "Count:", count,
  "</p>"))


ui <- fluidPage(
  
  leafletOutput("my_map")  
  
)

server <- function(input, output) {
  
  output$my_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("Stamen.Toner") %>%
      setView(lat = 39.5, lng = -98.35, zoom = 6) %>%
      addCircleMarkers(data = dat,
                       lat = ~lat, lng = ~lng,
                       popup = ~label,  # and here's where I replaced 'label' with 'popup'
                       radius = 10, fillOpacity = 3/4, stroke = FALSE, color = 'steelblue')
    
  })
  
}

shinyApp(ui, server)

