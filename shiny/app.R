library(shiny)
library(shinyBS)
library(leaflet)
library(dplyr)
library(tidyr)
library(caret)

# read data
locationsData <- read.csv("./www/AusCoordinates.csv", header = TRUE, sep=",")
# named vector of laction IDs
locations <- setNames(unclass(locationsData$Location), c(levels(locationsData$Location)) )

# named vector of directions
directions = c("E"=1, "ENE"=2, "ESE"=3, "N"=4,  "NE"=5, "NNE"=6, "NNW"=7, "NW"=8, "S"=9,
               "SE"=10, "SSE"=11, "SSW"=12, "SW"=13, "W"=14, "WNW"=15, "WSW"=16)

# current station
currStation = locationsData[1,]

# lod model: logRegModel
load("./www/logRegModel.Rdata");

# colors usd to highlight location: sunshine: fill, color. rain: fill, color
colors = c("#FFE700","#00b253","#b7dfe7","#A3BED6")

# UI code
ui <- bootstrapPage( theme = "styles.css",
                     div( class = "outer",
                          
                          # map in the background
                          leafletOutput("map", width="100%", height="100%"),
                          
                          absolutePanel( id = "controls", class = "control-panel", 
                                         
                                         h3("Rain in Australia"),
                                         
                                         h4("Predict rain tomorrow moving a few sliders today!"),
                                         
                                         #6 Input: location value ----
                                         selectInput('Location', 'Location', locations),
                                         bsTooltip("Location", "Select one region from the list of available regions to predict rainfall for tomorrow",
                                                   "right", options = list(container = "body")),
                                         
                                         
                                         #1 Input: Simple Humidity3pm ----
                                         sliderInput("Humidity3pm", "Humidity (percents):",
                                                     min = 0, max = 100,
                                                     value = 70, step = 5),
                                         bsTooltip("Humidity3pm", "How humid is it today?",
                                                   "right", options = list(container = "body")),
                                         
                                         #2 Input: Sunshine ----
                                         sliderInput("Sunshine", "Sunshine (hrs/day):",
                                                     min = 0, max = 12,
                                                     value = 8, step = 1),
                                         bsTooltip("Sunshine", "How many hours of sunshine?",
                                                   "right", options = list(container = "body")),
                                         
                                         #3 Input: Cloud3pm ----
                                         sliderInput("Cloud3pm", "Clouds (oktas):",
                                                     min = 0, max = 8,
                                                     value = 1, step = 1),
                                         bsTooltip("Cloud3pm", "Imagine that the sky is split into eight even parts. How many parts do the cloud cover today?",
                                                   "right", options = list(container = "body")),
                                         
                                         #4 Input: WindGustSpeed value ----
                                         sliderInput("WindGustSpeed", "Wind Gust Speed (km/h):",
                                                     min = 0, max = 100,
                                                     value = 3, step = 5),
                                         bsTooltip("WindGustSpeed", "If you know specify the wind gust speed",
                                                   "right", options = list(container = "body")),
                                         
                                         selectInput('WindDir3pm', 'Wind Direction', directions),
                                         bsTooltip("WindDir3pm", "Pick E - East, S - South, W - West, N -North or the combintation of the above",
                                                   "right", options = list(container = "body")),
                                         
                                         #5 Input: Rainfall value ----
                                         sliderInput("Rainfall", "Rainfall (mm):",
                                                     min = 0, max = 50,
                                                     value = 0, step = 5),
                                         bsTooltip("Rainfall", "Is it raining today?",
                                                   "right", options = list(container = "body")),    
                                         
                                         #7 Input: Pressure3pm value ----
                                         sliderInput("Pressure3pm", "Pressure (hpa):",
                                                     min = 977, max = 1042,
                                                     value = 1018, step = 5),
                                         bsTooltip("Pressure3pm", "What is hpa, you ask. Hectopascals are a measure of atmospheric pressure, which is a measure of the force applied by the weight of air above the area.",
                                                   "right", options = list(container = "body"))
                          ),
                          
                          absolutePanel( class = "prediction-panel", 
                                         # Output: prediction
                                         h4("Chance of Rain Tomorrow"),
                                         textOutput("prediction")
                          )
                          
                     ))

# Server Code
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  inputData <- reactive({
    data.frame(
      name = c("Humidity3pm",  "Sunshine", "Cloud3pm", "WindGustSpeed", "Rainfall",
               "Location", "Pressure3pm", "Pressure9am", "WindDir3pm"
      ),
      value = as.numeric(c(input$Humidity3pm, input$Sunshine, input$Cloud3pm,
                           input$WindGustSpeed, input$Rainfall, input$Location, input$Pressure3pm,
                           input$Pressure3pm, input$WindDir3pm ))
    )
  })
  
  
  output$prediction <- reactive({
    data <- spread(inputData(), name, value)
    data$WindDir3pm = as.factor(data$WindDir3pm)
    p <- paste0(round(predict(logRegModel, newdata =  data, type = "prob")[,"1"]*100, digits = 2),"%")
  })
  
  
  # draw a map
  output$map <- renderLeaflet({
    map = leaflet() %>% setView(lng = 133.8836, lat = -23.69748, zoom = 5 ) %>% addTiles() %>% 
      addCircleMarkers(data = locationsData, lng = ~Longtitude, lat = ~Latitude, label =~Location, 
                       color = colors[2], fillColor = colors[1], radius = 30, 
                       layerId =paste0(locationsData$Location,locationsData$Latitude,locationsData$Longtitude),
                       labelOptions = labelOptions(noHide = T, textOnly = F, className = "map-label"))
  })
  
  # react on ocation change event
  locationChanged <- reactive({
    station <-locationsData %>% filter(Location == names(locations)[which(locations == input$Location)])
  })
  
  # Observe location changes
  observe({
    station <- locationChanged()
    if (nrow(station) > 0) {
      map <- leafletProxy("map")
      dist <- 0.8
      lat <- station$Latitude[1]
      lng <- station$Longtitude[1]
      nm <- station$Location[1]
      stationId = paste0(currStation$Location,currStation$Latitude,currStation$Longtitude);
      # remove highlights from previous station add regular visualization 
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist) %>%
        removeMarker(stationId) %>%
        addCircleMarkers(lng = currStation$Longtitude, lat = currStation$Latitude,  
                         color = colors[2], fillColor = colors[1], radius = 30,
                         layerId = stationId, labelOptions = labelOptions(noHide = T, textOnly = F, className = "map-label"),
                         label =currStation$Location)  %>%       
        addCircleMarkers(lng = lng, lat = lat,  
                         color = colors[2], fillColor = colors[2], radius = 40,
                         label = nm, labelOptions = labelOptions(noHide = T, textOnly = F, className = "selected-map-label"),
                         layerId =paste0(nm,lat,lng)) 
      currStation <<- station
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
