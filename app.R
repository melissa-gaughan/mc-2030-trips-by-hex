# NOTES 23-04-07
  # You tried to implement the color pallete centered at zero for the percent change metrics and got it working!
    #Next Steps: 
      # Add the legend back in. Currently hard to see where the breaks actually are # DONE
      # add in for all metrics. -- DONE
        # NOTE: Max Service and New Service are still acting weird and the legend is out of order
                #also Saturday and Sunday --DONE
      # consider controls for outliers??
        #probably going to need to tbh. outliers in the data are making it so that smaller changes are getting collapsed. 
#selecting Change with 2020 network causes crash

#NOTES 23-04-18
  # You need to filter the 2020 GTFS to get post SC data. --This actually ended up being ok because the 201 GTFS that you reference starts at the 
  #service change and goes through April 2020. 
    

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
#  input$metric <- "change_in_trips_per_period"
#  input$day_type <- "wkd"
#  input$period <- "3 PM - 7 PM"
#  input$network <- "2030 Medium Growth Scenario"

metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"

# 
#hex_data_old <- readRDS(here::here("data", "2030_hex_comparison.rds"))
# hex_data <- readRDS(here::here("data", "2030_hex_comparison_2023-04-11.rds")) %>%
#   select(-network.y) %>%
#   pivot_longer(cols = -c(rowid, comparison_network, routes,
#                          period, daytype, spring_2020_trips_per_period,
#                          spring_2020_hours_in_period, spring_2020_routes,
#                          spring_2020_avg_trips_per_hour)) %>%
#   mutate(metric_name = str_replace_all(name, "_", " "),
#          metric_name = str_to_title(metric_name) ,
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
 #saveRDS( hex_data, here::here("data", "2030_hex_comparison_clean.rds"))


hex_data <- readRDS(here::here("data", "2030_hex_comparison_clean.rds"))

hex_grid <- readRDS(here::here("data", "filtered_hex_grid.rds")) %>% 
  st_transform(4326)

routes <- readRDS(here::here("data", "mc_2030_and_201_routes.rds")) %>% 
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
  
  # tabs #####
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
    ), 
    tabItem(tabName = "data_dictionary",
            fluidRow(
              column(width = 12,
                     box( id = "map_container_2", width = NULL, solidHeader = TRUE,
                          includeMarkdown("help.md"))))
    )
    
  
) 

)



ui <- dashboardPage(
  dashboardHeader(title = "Trips by Hexagon"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Hexagons", tabName = "hexagons", icon = icon("stroopwafel")),
      menuItem("Table", tabName = "block_groups", icon = icon("object-ungroup")), 
      menuItem("Data Dictionary", tabName = "data_dictionary", icon = icon("circle-question"))
      
    ),
    
    
    selectInput("network", "Select Network",
                choices = c("Spring 2020" = "Spring 2020",
                            "Low Growth MC 2030" = "2030 Low Growth Scenario", 
                            "Mid Growth MC 2030" = "2030 Medium Growth Scenario", 
                            "High Growth MC 2030" = "2030 High Growth Scenario"),
                selected = "2030 Medium Growth Scenario"
    ), 

    
    selectInput("metric", "Select Display Metric",
                choices = c( "Trips/Period" = "trips_per_period",
                             "Avg Trips/Hour" = "avg_trips_per_hour", 
                             "Change in Trips/Period" = "change_in_trips_per_period", 
                             "Change in Avg Trips/Hour" = "change_in_avg_trips_per_hour", 
                            "% Change Trips/Period" = "percent_change_in_trips_per_period", 
                            "% Change Avg Trips/Hour" = "percent_change_in_avg_trips_per_hour" , 
                           
                            "New Service", 
                            "Max Service"
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
                            "10 PM - 12 AM" , "Overnight" ), 
                
                selected = "3 PM - 7 PM"
    ), 
    selectInput("routes",
                "Routes (Display Only)",
                choices = NULL, 
                multiple = TRUE),
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
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML('help.md'))
  })
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
      
      minVal <- min(filtered_hex_data$value)
      maxVal <- max(filtered_hex_data$value)
      domain <- c(minVal,maxVal)
      values_df <- tibble(value = filtered_hex_data$value)
      
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(minVal,maxVal,values_df = values_df,
                                      interval=interval,interval_options = seq(10,5000,10),
                                      center=center)
      df_pal <- inferColor(color_bucket, 
                           color_below = "#b2182b", 
                           color_above = "#2166ac", 
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(value, breaks = color_bucket$breaks, 
                                        labels = color_bucket$breaks_label, 
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>% 
        arrange(metric_color_label)
    } else if( input$metric %in% c("percent_change_in_trips_per_period", 
                                   "percent_change_in_avg_trips_per_hour")){
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network, 
               name ==input$metric)  %>% 
        filter(is.finite(value)) %>% 
      #  mutate(value = value *100) %>% #taking out *100 here to test color function
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, label, metric_name)
      
      minVal <- min(filtered_hex_data$value)
      maxVal <- max(filtered_hex_data$value)
      domain <- c(minVal,maxVal)
      values_df <- tibble(value = filtered_hex_data$value)
      
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(minVal,maxVal,values_df = values_df,
                                      interval=interval,interval_options = seq(10,5000,10),
                                      center=center)
      df_pal <- inferColor(color_bucket, 
                           color_below = "#b2182b", 
                           color_above = "#2166ac", 
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(value, breaks = color_bucket$breaks, 
                                        labels = color_bucket$breaks_label, 
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>% 
        arrange(metric_color_label)
      
      #%>%
                   # rename(WATER_LAND_RATIO_Color_Group = Color_Label,
                          # WATER_LAND_RATIO_Color_Value = Color_Value))
      
      
      }else if( input$metric == "Max Service"){
        filtered_hex_data  <-  hex_data %>% 
              group_by(rowid) %>% 
              filter(name == "trips_per_period") %>% 
             slice_max(value, with_ties = F) %>% 
              rename(network = comparison_network) %>% 
              select(rowid, network, period, daytype, routes, name, value, label, metric_name )
        
        minVal <- min(filtered_hex_data$value)
        maxVal <- max(filtered_hex_data$value)
        domain <- c(minVal,maxVal)
        values_df <- tibble(value = filtered_hex_data$value)
        
        center <- as.numeric(0)
        interval <- ifelse((maxVal - minVal)>10,10,
                           ifelse((maxVal - minVal)>5,1,0.2))
        
        color_bucket <- calculateBucket(minVal,maxVal,values_df = values_df,
                                        interval=interval,interval_options = seq(10,5000,10),
                                        center=center)
        df_pal <- inferColor(color_bucket, 
                             color_below = "#b2182b", 
                             color_above = "#2166ac", 
                             interval=interval,
                             center=center)
        
        
        filtered_hex_data <- filtered_hex_data %>%
          mutate(metric_color_label = cut(value, breaks = color_bucket$breaks, 
                                          labels = color_bucket$breaks_label, 
                                          include.lowest = TRUE)) %>%
          mutate(metric_color_label = as.factor(metric_color_label)) %>%
          dplyr::left_join(df_pal) %>% 
          arrange(metric_color_label)
        
    } else {
      filtered_hex_data  <-  hex_data %>% 
        filter(daytype ==input$day_type &
                 period == input$period &
                 comparison_network == input$network, 
               name == input$metric)  %>% 
        select(rowid, period, daytype, routes, spring_2020_routes, name, value, label, metric_name)
      
      minVal <- min(filtered_hex_data$value)
      maxVal <- max(filtered_hex_data$value)
      domain <- c(minVal,maxVal)
      values_df <- tibble(value = filtered_hex_data$value)
      
      #setting the center at 0 if there is no negative number is bad
      
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(minVal,maxVal,values_df = values_df,
                                      interval=interval,interval_options = seq(10,5000,10),
                                      center=center)
      df_pal <- inferColor(color_bucket, 
                           color_below = "#b2182b", 
                           color_above = "#2166ac", 
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(value, breaks = color_bucket$breaks, 
                                        labels = color_bucket$breaks_label, 
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>% 
        arrange(metric_color_label)
      
    }
    
 
      
    reactive_hex_data_sf <- hex_grid %>% 
        left_join(filtered_hex_data, by = "rowid") %>% 
      filter(!is.na(period)) 
          
       
    })
    
  reactive_label <- reactive({
   label_data <-  reactive_hex_data_sf() %>%
     select(metric_color_label, metric_color_group) %>% 
     distinct(metric_color_label, metric_color_group) %>% 
     arrange(metric_color_label)

  })
  
 metric_df <-  c( "Trips/Period" = "trips_per_period",
               "Avg Trips/Hour" = "avg_trips_per_hour", 
               "Change in Trips/Period" = "change_in_trips_per_period", 
               "Change in Avg Trips/Hour" = "change_in_avg_trips_per_hour", 
               "% Change Trips/Period" = "percent_change_in_trips_per_period", 
               "% Change Avg Trips/Hour" = "percent_change_in_avg_trips_per_hour" , 
               
               "New Service" = "New Service",
               "Max Service" = "Max Service")
  
  
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
  
  
  # update routes to correspond to network selected
  observeEvent(input$network, {
    #req(input$network)
    #freezeReactiveValue(input, "routes")
    
    if(input$network == "Spring 2020"){
      network_routes <- routes %>% 
        filter(network == input$network) %>% 
        mutate(route_short_name = as.numeric(route_short_name)) %>% 
        filter(route_short_name < 500 | route_short_name %in% c(671:677))
      choices <- as.character(sort(unique( network_routes$route_short_name)))
      updateSelectInput( inputId = "routes", choices = choices)
    }else{
    
      network_routes <- routes %>% 
        filter(network == input$network)
      choices <- sort(unique( network_routes$route_short_name))
      updateSelectInput( inputId = "routes", choices = choices)
    }
  })
  
  
  reactive_routes <- reactive({
    req(input$routes)
    
    filtered_routes <- routes %>%
      filter(network == input$network ) %>% 
      filter(route_short_name %in% input$routes) 
    
  })
  
  
  
 
  
  output$mytable = renderDataTable(reactive_hex_table())
 
  
 
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
  observeEvent(( input$recalc ), {
    #for first load and subsequent when no routes are selected
    #loads hexes but not routes
   if(!isTruthy(input$routes) ){
    proxy <-  leafletProxy("metric_map") %>%
      clearGroup("hexes") %>%
      clearControls() %>%
      addPolygons( data = reactive_hex_data_sf() ,
                   weight = 1, 
                   opacity = 1,
                   color = "white",
                   layerId = reactive_hex_data_sf()$rowid,
                   fillOpacity = 0.8,
                   highlightOptions = highlightOptions(
                     weight = 5,
                     color = "#666",
                     fillOpacity = 0.8,
                     bringToFront = TRUE),
                   fillColor = ~reactive_hex_data_sf()$metric_color_group,
                   popup = ~label, 
                   group = "hexes"
      ) %>%
      
      addLegend(position = "topright",
                colors = reactive_label()$metric_color_group,
                labels = reactive_label()$metric_color_label,
                opacity =  0.8,
                 title = names(metric_df)[metric_df==input$metric])
   } else {
     proxy <-  leafletProxy("metric_map") %>%
       clearGroup("hexes") %>%
       clearGroup("Routes") %>%
       clearControls() %>%
       addPolygons( data = reactive_hex_data_sf() ,
                    weight = 1, 
                    opacity = 1,
                    color = "white",
                    layerId = reactive_hex_data_sf()$rowid,
                    fillOpacity = 0.8,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.8,
                      bringToFront = TRUE),
                    fillColor = ~reactive_hex_data_sf()$metric_color_group,
                    popup = ~label, 
                    group = "hexes"
       ) %>%
       addPolylines(
         data = reactive_routes(), 
         color = "black",
         weight = 3 , 
         group = "Routes",
         label = ~route_short_name,
         popup = ~paste0("<br>Route: ", route_short_name) ) %>%
       
       addLegend(position = "topright",
                 colors = reactive_label()$metric_color_group,
                 labels = reactive_label()$metric_color_label,
                 opacity =  0.8,
                 title = names(metric_df)[metric_df==input$metric])
   }
               
 }  ,ignoreNULL = FALSE)

  # 
  # 
 
  
  observeEvent( input$routes,  {
    #for changing routes without messing with metrics
    #if/else logic controls for when no routes are selected (clears map of routes)
   
    if(isTruthy(input$routes)){
    proxy <-  leafletProxy("metric_map") %>%
      clearGroup("Routes") %>%
      addPolylines(
        data = reactive_routes(), 
        color = "black",
        weight = 3 , 
        group = "Routes",
        label = ~route_short_name,
        popup = ~paste0("<br>Route: ", route_short_name) ) 
    } else{
      proxy <-  leafletProxy("metric_map") %>%
        clearGroup("Routes")
    }
    
  }  ,ignoreNULL = FALSE)
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
