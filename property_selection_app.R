library(shiny)
library(raster)
library(leaflet)
library(sf)
library(sp)
library(splines)
library(tidyverse)
library(pubtheme)
library(rsconnect)

st = read.csv('data/stops.txt')
st = st %>%
  select(stop_id, stop_code, stop_name, stop_lat, stop_lon)

dc = readRDS('data/tracts.and.census.with.EV.stations.rds')
dc = dc[dc$state == 'CT',]

tr = readRDS('data/trips.rds')
tr = tr %>% 
  arrange(stop1, stop2)

destinations <- st %>% filter(stop_name %in% c("Grand Central", "New Haven")) 
all_other_stops <- st %>% filter(!stop_name %in% c("Grand Central", "New Haven"))

# define UI

ui <- fluidPage(
  titlePanel("Train Station Map and Trips"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_station", "Choose a Train Station:", 
                  choices = unique(all_other_stops$stop_name),
                  selected = "Stamford")
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("tripPlot"),
      plotOutput("tripTime")
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    pal1 <- colorNumeric(palette = c("gray", "blue"), domain = dc$pop)
    dc@data$label <- paste0('<b>GEOID: ', dc@data$GEOID, '</b><br>',
                            dc@data$county, '<br>',
                            dc@data$state, '<br>',
                            'Population: ', format(dc@data$pop, big.mark=","))
    
    leaflet(dc) %>%
      setView(lng = -73, lat = 41, zoom = 8) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal1(dc@data$pop), 
                  weight = 0.5, 
                  color = 'black', 
                  fillOpacity = 0.7, 
                  label = ~label %>% lapply(htmltools::HTML))
  })
  
  observe({
    selected_station <- input$selected_station
    selected_stop <- st %>% filter(stop_name == selected_station)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = selected_stop,
                       lng = ~stop_lon,
                       lat = ~stop_lat,
                       popup = ~paste("Metro-North Station:", stop_name),
                       stroke = FALSE,
                       fillOpacity = 0.8,
                       fillColor = "green",
                       radius = 5) %>%
      addCircleMarkers(data = destinations,
                       lng = ~stop_lon,
                       lat = ~stop_lat,
                       popup = ~paste("Target WorkPlace Station:", stop_name),
                       stroke = FALSE,
                       fillOpacity = 0.8,
                       fillColor = "red",
                       radius = 6)
  })
  
  output$tripPlot <- renderPlot({
    selected_station <- input$selected_station
    
    ct_lines <- tr %>% 
      filter(stop1 == selected_station | stop2 == selected_station) %>% 
      mutate(trip = ifelse(tf == "from",
                           paste(stop2, "->", stop1),
                           paste(stop1, "->", stop2)))
    
    ggplot(ct_lines, aes(x = reorder(trip, n), 
                         y = n, 
                         fill = stop2)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.5, vjust = -0.5, color = "black", size = 3) +
      coord_flip() +
      labs(x = "Trips From/To Selected Station To/From New Haven/Grand Central", 
           y = "Number of Trips", 
           title = "Number of Trips per Route",
           fill = "Target Stations") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(1, 1, 1, 1, "cm"),
            axis.text.y = element_text(size = 12)) +
      ylim(0, max(ct_lines$n, na.rm = TRUE) * 1.1)
    
  })
  
  output$tripTime = renderPlot({
    selected_station <- input$selected_station
    
    ct_lines <- tr %>% 
      filter(stop1 == selected_station | stop2 == selected_station) %>% 
      mutate(trip = ifelse(tf == "from",
                           paste(stop2, "->", stop1),
                           paste(stop1, "->", stop2)))
    
    ggplot(ct_lines, aes(x = reorder(trip, mean), 
                         y = mean, 
                         fill = stop2)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(mean, 2)), hjust = -0.5, vjust = -0.5, color = "black", size = 3) +
      coord_flip() +
      labs(x = "Trip From/To Selected Station To/From Grand Central/New Haven", 
           y = "Average Duration of Trip From/To Grand Central/New Haven", 
           title = "Trips Duration per Route",
           fill = "Departure Stop") +
      theme_minimal(base_size = 12) +  
      theme(plot.margin = margin(1, 1, 1, 1, "cm"),
            axis.text.y = element_text(size = 12)
      ) +
      ylim(0, max(ct_lines$mean) * 1.1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
