library(shiny)


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
ui <- fluidPage(
  
  # App title ----
  titlePanel("Australian Regional Weather"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      #6 Input: location value ----
      selectInput('Location', 'Location', regions),
      
      
            #1 Input: Simple Humidity3pm ----
      sliderInput("Humidity3pm", "Humidity3pm:",
                  min = 0, max = 100,
                  value = 60),
      
      #2 Input: Sunshine ----
      sliderInput("Sunshine", "Sunshine:",
                  min = 0, max = 15,
                  value = 10, step = 5),
      
      #3 Input: Cloud3pm ----
      sliderInput("Cloud3pm", "Cloud3pm:",
                  min = 0, max = 8,
                  value = 5),
      
      #4 Input: WindGustSpeed value ----
      sliderInput("WindGustSpeed", "Wind Gust Speed:",
                  min = 7, max = 115,
                  value = 75, step = 5),
      
      #5 Input: Rainfall value ----
      sliderInput("Rainfall", "Rainfall:",
                  min = 0, max = 175,
                  value = 125, step = 5),

      #7 Input: Pressure3pm value ----
      sliderInput("Pressure3pm", "Pressure3pm:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      
      #8 Input: Pressure9am ----
      sliderInput("Pressure9am", "Pressure9am:",
                  min = 1, max = 1000,
                  value = 0.5, step = 0.1),
      #9 Input: Cloud9am ----
      sliderInput("Cloud9am", "Cloud9am:",
                  min = 1, max = 1000,
                  value = 0.5, step = 0.1)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Table summarizing the values entered ----
      tableOutput("values")
      
      
      
    )
    
  )
 
)

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
               "Pressure3pm",
               "Pressure9am",
               "Cloud9am"
      ),
      Value = as.character(c(input$Humidity3pm,
                             input$Sunshine,
                             input$Cloud3pm,
                             input$WindGustSpeed,
                             input$Rainfall,
                             input$Location,
                             input$Pressure3pm,
                             input$Pressure9am,
                             input$Cloud9am
      )),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
