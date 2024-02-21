# Install necessary packages
install.packages(c("shiny", "plotly", "readr", "leaflet", "dplyr"))

# Load necessary packages
library(shiny)
library(plotly)
library(readr)
library(leaflet)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Tsunami Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Choose a column:", 
                  choices = c("COUNTRY", "REGION", "TS_INTENSITY"))
    ),
    mainPanel(
      plotlyOutput("histogram"),
      plotlyOutput("pieChart"),
      tableOutput("topEntries"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read the data
  data <- read_csv("C:/Users/mutku/Desktop/Trimester3/Time series regression/Tsunami_Dataset.csv")
  
  output$histogram <- renderPlotly({
    # Create histograms for LATITUDE and LONGITUDE
    p1 <- plot_ly(data, x = ~LATITUDE, type = "histogram", name = "LATITUDE")
    p2 <- plot_ly(data, x = ~LONGITUDE, type = "histogram", name = "LONGITUDE")
    subplot(p1, p2)
  })
  
  output$pieChart <- renderPlotly({
    # Create a pie chart for the selected column
    pieData <- data %>%
      group_by(get(input$column)) %>%
      summarise(Count = n())
    plot_ly(pieData, labels = ~get(input$column), values = ~Count, type = "pie")
  })
  
  output$topEntries <- renderTable({
    # Find top 10 entries by the count of the selected column
    topEntries <- data %>%
      group_by(get(input$column)) %>%
      summarise(Count = n()) %>%
      top_n(10, Count)
    topEntries
  })
  
  output$map <- renderLeaflet({
    # Create a chloropleth map of world with count of country acting as magnitude
    countryData <- data %>%
      group_by(COUNTRY) %>%
      summarise(Count = n())
    leaflet(countryData) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addTiles() %>%
      addCircleMarkers(~LONGITUDE, ~LATITUDE, radius = ~sqrt(Count), color = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

