# Install necessary packages
install.packages(c("shiny", "plotly", "readr"))

# Load necessary packages
library(shiny)
library(plotly)
library(readr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Irrigation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Choose a season:", 
                  choices = c("Kharif", "Rabi", "Perennial"))
    ),
    mainPanel(
      plotlyOutput("barPlot"),
      tableOutput("correlation"),
      tableOutput("totalIrrigation")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read the data
  data <- read_csv("C:/Users/mutku/Desktop/Trimester3/Time series regression/Dashboard_UPVillageSchedule.csv")
  
  # Calculate total gross irrigated area
  data$Total_gross_irrigated_area <- rowSums(data[,c("Kharif_seasongross_irrigated_area", "Rabi_seasongross_irrigated_area", "Perennial_season_gross_irrigated_area")], na.rm = TRUE)
  
  output$barPlot <- renderPlotly({
    # Depending on the input, select a column
    column <- switch(input$season,
                     "Kharif" = "Kharif_seasongross_irrigated_area",
                     "Rabi" = "Rabi_seasongross_irrigated_area",
                     "Perennial" = "Perennial_season_gross_irrigated_area")
    
    # Find top 5 districts
    topDistricts <- data %>%
      group_by(District_name) %>%
      summarise(IrrigatedArea = sum(get(column), na.rm = TRUE)) %>%
      top_n(5) %>%
      arrange(desc(IrrigatedArea))
    
    # Create a plotly bar plot
    p <- plot_ly(topDistricts, x = ~District_name, y = ~IrrigatedArea, type = 'bar')
    
    # Return the plot
    return(p)
  })
  
  output$correlation <- renderTable({
    # Calculate correlation
    correlation <- cor(data$Post_monsoonavg_ground_water_level, data$Kharif_seasongross_irrigated_area, use = "complete.obs")
    
    # Return the correlation
    return(data.frame(Correlation = correlation))
  })
  
  output$totalIrrigation <- renderTable({
    # Calculate total gross irrigated area for each district
    totalIrrigation <- data %>%
      group_by(District_name) %>%
      summarise(TotalIrrigatedArea = sum(Total_gross_irrigated_area, na.rm = TRUE))
    
    # Return the total irrigation
    return(totalIrrigation)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
# R-Dashboard
