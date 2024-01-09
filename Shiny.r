# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
install.packages("readxl")
library(readxl)
# Make sure to install and load the 'sf' package
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)


# Assuming 'final_data.csv' contains columns like 'JOUR', 'Sum_NB_VALD', etc.
final_datar <- read_csv("final_data.csv") # Replace with your actual file path
new_data <- read_xlsx("zonesdarrets.xlsx")
# Assuming you have the dplyr package installed and loaded
new_data <- sample_n(new_data, 1000)

final_data1 <- final_datar
# Define the user interface
ui <- fluidPage(
  titlePanel("Transportation Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date_1", "Select Start Date (Period 1):", value = as.Date("2019-01-01")),
      dateInput("end_date_1", "Select End Date (Period 1):", value = as.Date("2023-01-03")),
      dateInput("start_date_2", "Select Start Date (Period 2):", value = as.Date("2019-01-07")),
      dateInput("end_date_2", "Select End Date (Period 2):", value = as.Date("2023-03-10")),
      selectInput("selected_station", "Choose a Station:", choices = unique(final_data1$LIBELLE_ARRET)),
      actionButton("update_btn", "Update Data"),
      leafletOutput("map_station")
    ),
    mainPanel(
      plotOutput("validationPlot"),
      plotOutput("avg_validations_plot"),
      plotOutput("comparison_plot")
        # Add this line for the monthly validations plot
     # tableOutput("station_stats")
       # Add this line for the average validations plot
      
    )
  )
)
server <- function(input, output, session) {
  # Filter data based on user inputs
  filtered_data_ref <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_ref, JOUR <= input$end_date_ref)
  })
  
  filtered_data_comp <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_comp, JOUR <= input$end_date_comp)
  })
  
  # Update ridership comparison plot based on filtered data
  output$comparison_plot <- renderPlot({
    # Combine the datasets for plotting
    ref_data <- filtered_data_ref() %>%
      mutate(Period = "1")
    comp_data <- filtered_data_comp() %>%
      mutate(Period = "2")
    combined_data <- rbind(ref_data, comp_data)
    
    # Plot the data
    ggplot(combined_data, aes(x = JOUR, y = Sum_NB_VALD, color = Period)) +
      geom_line() +
      labs(title = "Ridership Comparison", x = "Date", y = "Total Validations") +
      scale_color_manual(values = c("1" = "blue", "2" = "red")) +
      theme_minimal()
  })
  
  # Reactive expression for map data
  processMapData <- reactive({
    st_as_sf(new_data, coords = c("ZdAXEpsg2154", "ZdAYEpsg2154"), crs = 2154) %>%
      st_transform(crs = 4326)
  })
  
  # Create a leaflet map for station selection
  output$map_station <- renderLeaflet({
    leaflet(data = processMapData()) %>%
      addTiles() %>%
      addMarkers(~st_coordinates(geometry)[, 1], ~st_coordinates(geometry)[, 2], popup = ~as.character(ZdCId))
  })
  
  # Render plot for sum of validations by month and year
  output$validationPlot <- renderPlot({
    # Aggregate data by Month_Year
    monthly_data <- final_data1 %>%
      group_by(Month_Year) %>%
      summarise(Sum_Validations = sum(Sum_NB_VALD, na.rm = TRUE)) %>%
      arrange(Month_Year)  # Make sure the data is sorted by Month_Year
    
    # Plot the data
    ggplot(monthly_data, aes(x = Month_Year, y = Sum_Validations)) +
      geom_col() +  # Use geom_col for a bar chart
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x labels for readability
      labs(x = "Month and Year", y = "Sum of Validations", title = "Monthly Sum of Validations")
  })
  # Reactive expression for calculating the average number of validations per weekday
  avg_validations_per_weekday <- reactive({
    final_data1 %>%
      group_by(Weekday) %>%
      summarise(Avg_Validations = mean(Sum_NB_VALD, na.rm = TRUE)) %>%
      arrange(match(Weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
  })
  
  # Render the plot for average validations per weekday
  output$avg_validations_plot <- renderPlot({
    req(avg_validations_per_weekday())  # Ensure data is not empty
    data_to_plot <- avg_validations_per_weekday()
    
    ggplot(data_to_plot, aes(x = Weekday, y = Avg_Validations)) +
      geom_col(fill = "steelblue") +
      labs(title = "Average Validations per Weekday",
           x = "Weekday",
           y = "Average Number of Validations") +
      theme_minimal()
  })
  
  # Variable to store the selected station ID
  selected_station_id <- reactiveVal()
  
  # Observe the selection on the map and update the station ID
  observeEvent(input$map_station_marker_click, {
    selected_station_id(as.character(input$map_station_marker_click$popup))
  })
  
  # Render the station statistics table
  output$station_stats <- renderTable({
    # Check if a station has been selected
    if (is.null(selected_station_id())) {
      return()
    }
    
    # Filter final_data1 based on the selected station ID
    stats_data <- final_data1 %>%
      filter(ID_REFA_LDA == selected_station_id()) %>%
      group_by(LIBELLE_ARRET, Weekday) %>%
      summarise(
        Total_Validations = sum(Sum_NB_VALD, na.rm = TRUE),
        Avg_Validations = mean(Sum_NB_VALD, na.rm = TRUE),
        Max_Validations = max(Sum_NB_VALD, na.rm = TRUE)
      )
    
    # Return the stats data if available
    if (nrow(stats_data) > 0) {
      stats_data
    } else {
      return()
    }
  })
}

shinyApp(ui, server)
