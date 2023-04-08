# NOTES 23-04-07
  # You tried to implement the color pallete centered at zero for the percent change metrics and got it working!
    #Next Steps: 
      # Add the legend back in. Currently hard to see where the breaks actually are
      # add in for all metrics.
      # consider controls for outliers??
    

library(shiny)
library(shinydashboard)
#library(shinydashboardPlus)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(sf)
library(rsconnect)
library(here)
library(shinyalert)
source('utils.R')

#library(rgdal)

# input <- list()
# > input$metric <- "percent_change_in_trips_per_period"
# > input$day_type <- "wkd"
# > input$period <- "3 PM - 7 PM"
# > input$network <- "2030 Medium Growth Scenario"

metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"

# 
# hex_data <- readRDS(here::here("data", "2030_hex_comparison.rds")) %>%
#   # select(-network.y) %>%
#   # pivot_longer(cols = -c(rowid, comparison_network, routes,
#   #                        period, daytype, spring_2020_trips_per_period,
#   #                        spring_2020_hours_in_period, spring_2020_routes,
#   #                        spring_2020_avg_trips_per_hour)) %>%
#   mutate(metric_name = str_replace_all(name, "_", " "),
#          metric_name = str_to_title(network_name) ,
#          comparison_network_name = str_replace_all(comparison_network, "_", " "),
#          comparison_network_name = str_to_title(comparison_network_name) ,
#          ) %>%
#   mutate(label = case_when(name %in% c("percent_change_in_trips_per_period",
#                                        "percent_change_in_avg_trips_per_hour") ~
#                              paste0( "<p> <b>", metric_name,": ", "</b> ", value, "%", "</br>",
#                                    "<b>", comparison_network_name, " routes: ", "</b> ", routes, "</br>",
#                                    "<b>", "Spring 2020 Routes: ",  "</b> ", spring_2020_routes, "</br>",
#                                     "</p>"),
#                            TRUE ~  paste0( "<p> <b>", metric_name,": ", "</b> ", round(value,2), "</br>",
#                                            "<b>", comparison_network_name, " Routes: ", "</b> ", routes, "</br>",
#                                            "<b>", "Spring 2020 Routes: ",  "</b> ", spring_2020_routes, "</br>",
#                                            "</p>")))
# 
# saveRDS( hex_data, here::here("data", "2030_hex_comparison.rds"))


hex_data <- readRDS(here::here("data", "2030_hex_comparison.rds"))

hex_grid <- readRDS(here::here("data", "filtered_hex_grid.rds")) %>% 
  st_transform(4326)



# Define UI 
#UI #####
body <- dashboardBody(
  
  #####
  tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 80;

      
        $("#map_container").height(boxHeight);
        $("#metric_map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
  
  #####
  tabItems( 
    # First tab content
    tabItem(tabName = "hexagons", 
            
            fluidRow(
              column(width = 12,  
                     box( id = "map_container", width = NULL, solidHeader = TRUE,
                        
                          
                          
                           leaflet::leafletOutput("metric_map"))))
    )
    ,
    tabItem(tabName = "block_groups",
            fluidRow(
              column(width = 12,
                     box( id = "map_container_1", width = NULL, solidHeader = TRUE,
                          dataTableOutput('mytable'))))
    )
  
) 

)



ui <- dashboardPage(
  dashboardHeader(title = "Trips by Hexagon"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Hexagons", tabName = "hexagons", icon = icon("stroopwafel")),
      menuItem("Table", tabName = "block_groups", icon = icon("object-ungroup"))
    ),
    
    
    selectInput("network", "Select Network",
                choices = c("Spring 2020" = "Spring 2020",
                            "Low Growth MC 2030" = "2030 Low Growth Scenario", 
                            "Mid Growth MC 2030" = "2030 Medium Growth Scenario", 
                            "High Growth MC 2030" = "2030 High Growth Scenario"),
                selected = "2030 Medium Growth Scenario"
    ), 
    
    selectInput("metric", "Select Display Metric",
                choices = c( #"Trips/Period" = "trips_per_period",
                             #"Avg Trips/Hour" = "avg_trips_per_hour", 
                             #"Change in Trips/Period" = "change_in_trips_per_period", 
                             #"Change in Avg Trips/Hour" = "change_in_avg_trips_per_hour", 
                            "% Change Trips/Period" = "percent_change_in_trips_per_period", 
                            "% Change Avg Trips/Hour" = "percent_change_in_avg_trips_per_hour" #, 
                           
                            #"New Service", 
                            #"Max Service"
                            ),
                selected = "% Change Trips/Period"
    ), 
    
    selectInput("day_type", "Select Weekday",
                choices = c("Weekday" = "wkd", "Saturday" = "sat", "Sunday" = "sun"),
                selected = "wkd"
    ), 
    
    selectInput("period", "Select Time Period",
                choices = c( "5 AM - 9 AM" , "9 AM - 3 PM", 
                            "3 PM - 7 PM" , "7 PM - 10 PM" , 
                            "11 PM - 12 AM" , "Overnight" ), 
                
                selected = "3 PM - 7 PM"
    ), 
    
    actionButton("recalc", "Update Map & Filters")
    ), 
  
  body
)               
#SERVER #####

server <- function(input, output) {
  # startup alert #####
  
  # shinyalert(
  #   title = "King County Metro Transit Accessibility Demo",
  #   text = "This app shows how many places can be reached in a 30 minute transit trip at noon during Spring 2022. The results presented here are preliminary and should not be used for trip planning or policy making.",
  #   size = "s", 
  #   closeOnEsc = TRUE,
  #   closeOnClickOutside = TRUE,
  #   html = FALSE,
  #   type = "",
  #   showConfirmButton = TRUE,
  #   showCancelButton = FALSE,
  #   confirmButtonText = "OK",
  #   confirmButtonCol = "#AEDEF4",
  #   timer = 0,
  #   imageUrl = metro,
  #   animation = TRUE
  # )
  # 
  #reactive functions #####
  
  
  
  #ok, right now it's just a df. Need to decide how far to take the transformation before spatializing
  #need to read in spatial df, make sure already in 4326
  
  #if metric is %, filter out inf
  #if metric is new service, filter 2020 == 0
  #if network is 2020, can show change data. filter selections?
  
  #will need to develop ways to handle breaks and cuts for scales
  
  reactive_hex_data_sf <- reactive({

    
     if( input$metric == "New Service"){
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==  input$day_type & # "wkd", #
                 period == input$period & # "3 PM - 7 PM", #
                 comparison_network ==   input$network & #"2030 High Growth Scenario",
               name %in% c( "trips_per_period")) %>%  #, "avg_trips_per_hour" # taking out the avg until we get basic functionality workujng 
        filter(spring_2020_routes == "No Service") %>% 
        select(rowid, period, daytype, routes, name, value, label)
    } else if( input$metric %in% c("percent_change_in_trips_per_period", 
                                   "percent_change_in_avg_trips_per_hour")){
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network, 
               name ==input$metric)  %>% 
        filter(is.finite(value)) %>% 
        mutate(value = value *100) %>% 
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, label, metric_name)
      
      minVal <- min(filtered_hex_data$value)
      maxVal <- max(filtered_hex_data$value)
      domain <- c(minVal,maxVal)
      
      center <- as.numeric(100)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(minVal,maxVal,
                                      interval=interval,interval_options = seq(10,5000,10),
                                      center=center,floor_at= -1 * as.numeric(100))
      df_pal <- inferColor(color_bucket, 
                           color_below = "#b2182b", 
                           color_above = "#2166ac", 
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(value, breaks = color_bucket$breaks, labels = color_bucket$breaks_label)) %>%
        mutate(metric_color_label = as.character(metric_color_label)) %>%
        dplyr::left_join(df_pal) #%>%
                   # rename(WATER_LAND_RATIO_Color_Group = Color_Label,
                          # WATER_LAND_RATIO_Color_Value = Color_Value))
      
      
      }else if( input$metric == "Max Service"){
        filtered_hex_data  <-  hex_data %>% 
              group_by(rowid) %>% 
              filter(name == "trips_per_period") %>% 
             slice_max(value, with_ties = F) %>% 
              rename(network = comparison_network) %>% 
              select(rowid, network, period, daytype, routes, name, value, label, metric_name )
        
    } else {
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network, 
               name == input$metric)  %>% 
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, label, metric_name)
      
    }
    
 
      
    reactive_hex_data_sf <- hex_grid %>% 
        left_join(filtered_hex_data, by = "rowid") %>% 
      filter(!is.na(period)) 
          
       
    })
    
  
  
  reactive_hex_table <- reactive({
    
    
    if( input$metric == "New Service"){
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==  input$day_type & # "wkd", #
                 period == input$period & # "3 PM - 7 PM", #
                 comparison_network ==   input$network & #"2030 High Growth Scenario",
                 name %in% c( "trips_per_period")) %>%  #, "avg_trips_per_hour" # taking out the avg until we get basic functionality workujng 
        filter(spring_2020_routes == "No Service") %>% 
        select(rowid, period, daytype, routes, name, value,metric_name)
    } else if( input$metric %in% c("percent_change_in_trips_per_period", 
                                   "percent_change_in_avg_trips_per_hour")){
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network & 
               name ==input$metric)  %>% 
        filter(is.finite(value)) %>% 
        mutate(value = value *100) %>% 
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, metric_name)
    }else if( input$metric == "Max Service"){
      filtered_hex_data  <-  hex_data %>% 
        group_by(rowid) %>% 
        filter(name == "trips_per_period") %>% 
        slice_max(value, with_ties = F) %>% 
        rename(network = comparison_network) %>% 
        select(rowid, network, period, daytype, routes, name, value, metric_name )
      
    } else {
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network &
               name == input$metric)  %>% 
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, metric_name)
      
    }
    
    
    

    
  })
  
  
 
  
  output$mytable = renderDataTable(reactive_hex_table())
 
  
  colorpal <- reactive({
    
colorQuantile("viridis", reactive_hex_data_sf()$value,n = 7, reverse = F)
    
    
    # if( input$metric == "trips_per_period"){
    #   colorBin("viridis", reactive_hex_data_sf()$trips_per_period, reverse = F)
    #   
    # } else if ( input$metric == "avg_trips_per_hour"){
    #   colorBin("viridis", reactive_hex_data_sf()$avg_trips_per_hour, reverse = F)
    #   
    # } else if ( input$metric == "change_in_trips_per_period"){
    #   colorBin("viridis", reactive_hex_data_sf()$change_in_trips_per_period, reverse = F) 
    #   
    # } else if ( input$metric == "change_in_avg_trips_per_hour"){
    #   colorBin("viridis", reactive_hex_data_sf()$change_in_avg_trips_per_hour, reverse = F) 
    #   
    # } else if ( input$metric == "percent_change_in_trips_per_period"){
    #   colorBin("viridis", reactive_hex_data_sf()$percent_change_in_trips_per_period, reverse = F)
    # } else if ( input$metric == "percent_change_in_avg_trips_per_hour"){
    #   colorBin("viridis", reactive_hex_data_sf()$percent_change_in_avg_trips_per_hour, reverse = F)
    # } else if ( input$metric == "New Service"){
    #     colorBin("viridis", reactive_hex_data_sf()$trips_per_period, reverse = F)
    # } else if ( input$metric == "Max Service"){      
    #   colorBin("viridis", reactive_hex_data_sf()$trips_per_period, reverse = F)}
      
   

  })
  
  # 
  # # map functions #####
  output$metric_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::setView(lng = -122.3321, lat = 47.6062, zoom = 11 ) %>%
      leaflet.extras::addSearchOSM() %>%
      leafem::addLogo( img =   metro,
                       src="remote",
                       position = "bottomright",
                       #offset.x = 30,
                       #offset.y = 100,
                       height = 30,
                       width = 80) %>%
      leaflet::addScaleBar(position = "topright")
  })
  # 
  # # Incremental changes to the map (in this case, replacing the
  # # circles when a new color is chosen) should be performed in
  # # an observer. Each independent set of things that can change
  # # should be managed in its own observer.   # Change Routes #####
  observeEvent(input$recalc ,{
    pal <- colorpal()
    #input$recalc,
    proxy <-  leafletProxy("metric_map") %>%
      clearShapes() %>%
      clearControls() %>%
  #     clearGroup("Community Assets") %>% 
      addPolygons( data = reactive_hex_data_sf() , weight = 1, opacity = 1,
                   color = ~reactive_hex_data_sf()$metric_color_group,
                   # dashArray = "3",
                   layerId = reactive_hex_data_sf()$rowid,
                   fillOpacity = 0.9,
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#666",
                     #dashArray = "",
                     fillOpacity = 0.9,
                     bringToFront = TRUE),
                 #  label = ~accessibility,
                   # labelOptions = labelOptions(
                   #   style = list("font-weight" = "normal", padding = "3px 8px"),
                   #   textsize = "15px",
                   #   direction = "auto"),
                   fillColor = ~reactive_hex_data_sf()$metric_color_group,
                   popup = ~label #paste0(input$metric, ": ", reactive_hex_data_sf()$value)
      ) #%>%
  #     addMarkers(
  #       data = reactive_assets(), 
  #       group = "Community Assets",
  #       popup = ~paste0(name, " (", assettype, ")")
  #       #     clusterOptions = markerClusterOptions()
  #     ) %>% 
  #     addLayersControl(
  #       overlayGroups = c( "Community Assets"),
  #       options = layersControlOptions(collapsed = FALSE)
  # #     )  %>% 
  #     addLegend(position = "topright",
  #               colors = df_pal$Color_Value,
  #               labels = df_pal$Color_Label,
  #               opacity =  0.9
  #               # pal = pal,
  #               # values = reactive_hex_data_sf()$value,
  #               # title = unique(reactive_hex_data_sf()$metric_name), 
  #               # labFormat = function(type, cuts, p) {             
  #               #   n = length(cuts)             
  #               #     paste0(as.integer(cuts)[-n], " to ", as.integer(cuts)[-1])}
  #     ) #%>%
  #     hideGroup("Community Assets")
  #   
  #   
 }  ,ignoreNULL = FALSE)
  # 
  # 
 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)