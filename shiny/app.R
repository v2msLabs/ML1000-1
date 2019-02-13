library(shiny)
library(shinyBS)
library(leaflet)


# Define UI for slider demo app ----
# following variables for input and RainTomorrow for output
#"Humidity3pm"   "Sunshine"      "Cloud3pm"      "WindGustSpeed" "Rainfall"      "Location"     
#"Pressure3pm"   "Pressure9am"   "Cloud9am"  "RainTomorrow"
#"Adelaide","Albany","Albury","AliceSprings","BadgerysCreek","Ballarat","Bendigo","Brisbane","Cairns","Canberra","Cobar","CoffsHarbour","Dartmoor","Darwin","GoldCoast","Hobart","Katherine","Launceston","Melbourne","MelbourneAirport","Mildura","Moree","MountGambier","MountGinini","Newcastle","Nhil","NorahHead","NorfolkIsland","Nuriootpa","PearceRAAF","Penrith","Perth","PerthAirport","Portland","Richmond","Sale","SalmonGums","Sydney","SydneyAirport","Townsville","Tuggeranong","Uluru","WaggaWagga","Walpole","Watsonia","Williamtown","Witchcliffe","Wollongong","Woomera"
data <- c("Adelaide","Albany","Albury","AliceSprings","BadgerysCreek","Ballarat","Bendigo","Brisbane","Cairns","Canberra","Cobar","CoffsHarbour","Dartmoor","Darwin","GoldCoast","Hobart","Katherine","Launceston","Melbourne","MelbourneAirport","Mildura","Moree","MountGambier","MountGinini","Newcastle","Nhil","NorahHead","NorfolkIsland","Nuriootpa","PearceRAAF","Penrith","Perth","PerthAirport","Portland","Richmond","Sale","SalmonGums","Sydney","SydneyAirport","Townsville","Tuggeranong","Uluru","WaggaWagga","Walpole","Watsonia","Williamtown","Witchcliffe","Wollongong","Woomera")
regions <<- list("Adelaide" = "Adelaide","Albany" = "Albany")
weatherData = read.csv("../data/weatherAUS.csv", header = TRUE, na.strings = c("NA","","#NA"),sep=",")
regions <- unique(weatherData$Location)
###knitr::include_graphics("images/weatherStations.png")


ui <- bootstrapPage( theme = "styles.css",
  div( class = "outer",

  # map in the background
  leafletOutput("map", width="100%", height="100%"),
  
  absolutePanel( id = "controls", class = "control-panel", 

    titlePanel("title" = "Rain in Australia"),
    
    #6 Input: location value ----
    selectInput('Location', 'Location', regions),
    bsTooltip("Location", "Select one region from the list of available regions to predict rainfall for tomorrow",
              "right", options = list(container = "body")),
    
    
    #1 Input: Simple Humidity3pm ----
    sliderInput("Humidity3pm", "Humidity (percents):",
                min = 0, max = 100,
                value = 60),
    bsTooltip("Humidity3pm", "What is humidity today",
              "right", options = list(container = "body")),
    
    #2 Input: Sunshine ----
    sliderInput("Sunshine", "Sunshine (hrs/day):",
                min = 0, max = 15,
                value = 10, step = 5),
    bsTooltip("Sunshine", "How many ours of sunshine?",
              "right", options = list(container = "body")),
    
    #3 Input: Cloud3pm ----
    sliderInput("Cloud3pm", "Clouds (oktas):",
                min = 0, max = 8,
                value = 2),
    bsTooltip("Cloud3pm", "Imagine that the sky is split into for even parts. How many parts do the cloud cover today?",
              "right", options = list(container = "body")),
    
    #4 Input: WindGustSpeed value ----
    sliderInput("WindGustSpeed", "Wind Gust Speed (km/h):",
                min = 7, max = 115,
                value = 75, step = 5),
    bsTooltip("WindGustSpeed", "If you know specify wind gust speed",
              "right", options = list(container = "body")),
    
    #5 Input: Rainfall value ----
    sliderInput("Rainfall", "Rainfall (mm):",
                min = 0, max = 175,
                value = 125, step = 5),
    
    #7 Input: Pressure3pm value ----
    sliderInput("Pressure3pm", "Pressure (hpa):",
                min = 0, max = 1,
                value = 0.5, step = 0.1),
    br(),
    actionButton("predictButton", "Predict Rainfall")
  ),

  # why do we need this?
  absolutePanel( class = "result-panel", 
    # Output: Table summarizing the values entered ----
    tableOutput("values")
  )
  
))

# Define server logic for slider examples ----
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    data.frame(
      Name = c("Humidity3pm",
               "Sunshine",
               "Cloud3pm",
               "WindGustSpeed",
               "Rainfall",
               "Location",
               "Pressure3pm"
      ),
      Value = as.character(c(input$Humidity3pm,
                             input$Sunshine,
                             input$Cloud3pm,
                             input$WindGustSpeed,
                             input$Rainfall,
                             input$Location,
                             input$Pressure3pm
      )),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  # draw a map
  output$map <- renderLeaflet({
    # The code that generates the map has been excluded for performance sake
    locations = read.csv("../data/AusCoordinates.csv", header = TRUE, sep=",")
    map = leaflet() %>% setView(lng = 133.8836, lat = -23.69748, zoom = 5 ) %>% addTiles() %>% 
      addCircleMarkers(data = locations, lng = ~Longtitude, lat = ~Latitude, label =~Location,  
                       labelOptions = labelOptions(noHide = T, textOnly = T))
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
