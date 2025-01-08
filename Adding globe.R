library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(threejs)
# Load and preprocess data
data <- fread("Data_withlon_lat.csv")

# Convert 'Review Rate' to date format
data$date <- format(floor_date(data$`Review Rate`, "month"), "%Y-%m")
data$date <- as.Date(paste0(data$date, "-01"), format = "%Y-%m-%d")

data_g4 <- data %>%
  group_by(Airline, date, Departure, Destination,Dep_Latitude, Dep_Longitude,Dest_latitude,Dest_longitude) %>%
  summarize(
    satisfaction_ratio = sum(Satisfaction == "Satisfied") / n(),
    Rating = mean(Rating, na.rm = TRUE),
    Legroom = mean(Legroom, na.rm = TRUE),
    `Seat Comfort` = mean(`Seat Comfort`, na.rm = TRUE),
    Entertainment = mean(Entertainment, na.rm = TRUE),
    `Customer Service` = mean(`Customer Service`, na.rm = TRUE),
    Value = mean(Value, na.rm = TRUE),
    Cleanliness = mean(Cleanliness, na.rm = TRUE),
    `Check In` = mean(`Check In`, na.rm = TRUE),
    Food = mean(Food, na.rm = TRUE)
  )


# UI
ui <- dashboardPage(
  dashboardHeader(title = 'Airline Dashboard'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Landing Page', tabName = 'landing', icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      # Landing Page Tab
      tabItem(
        tabName = 'landing',
        fluidRow(
          column(12,
                 box(
                   title = 'Global Airline Network', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   globeOutput("airline_globe", height = "600px")
                 )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered data for combined analysis
  filtered_data_combined <- reactive({
    data_g4 %>%
      filter(Departure == input$Departure, 
             Destination == input$Destination,
             date >= as.Date(input$date_range_combined[1]) & date <= as.Date(input$date_range_combined[2]))
  })
  
  
  output$airline_globe <- renderGlobe({
    # Clean the data by removing rows with NA or invalid latitudes and longitudes
    data_cleaned <- data_g4 %>%
      filter(!is.na(Dep_Latitude) & !is.na(Dep_Longitude) & 
               !is.na(Dest_latitude) & !is.na(Dest_longitude) &
               Dep_Latitude >= -90 & Dep_Latitude <= 90 & 
               Dest_latitude >= -90 & Dest_latitude <= 90 & 
               Dep_Longitude >= -180 & Dep_Longitude <= 180 & 
               Dest_longitude >= -180 & Dest_longitude <= 180)
    
    # Check if cleaned data has valid entries
    if (nrow(data_cleaned) == 0) {
      print("No valid data available for globe plot.")
      return(NULL)  # No data to display
    }
    
    # Create connections data (aggregate by Departure and Destination)
    connections <- data_cleaned %>%
      group_by(Departure, Destination) %>%
      summarize(
        flights = n(),  # Count number of flights (rows)
        from_lat = first(Dep_Latitude),   # Latitude for departure city
        from_lon = first(Dep_Longitude),  # Longitude for departure city
        to_lat = first(Dest_latitude),    # Latitude for destination city
        to_lon = first(Dest_longitude)   # Longitude for destination city
      ) %>%
      filter(!is.na(from_lat) & !is.na(from_lon) & !is.na(to_lat) & !is.na(to_lon))  # Remove rows with NAs
    
    # Debugging: Check the cleaned data for any issues
    print("Connections Data (first few rows):")
    print(head(connections))
    
    # Check for any remaining missing or problematic values in connections data
    if (any(is.na(connections$from_lat)) || any(is.na(connections$from_lon)) ||
        any(is.na(connections$to_lat)) || any(is.na(connections$to_lon)) ||
        any(connections$flights == 0)) {
      print("Data issue detected: NA or zero flights.")
      return(NULL)  # Prevent rendering if data is still incomplete
    }
    
    # Ensure latitudes and longitudes are numeric and valid
    connections$from_lat <- as.numeric(connections$from_lat)
    connections$from_lon <- as.numeric(connections$from_lon)
    connections$to_lat <- as.numeric(connections$to_lat)
    connections$to_lon <- as.numeric(connections$to_lon)
    connections$flights <- as.numeric(connections$flights)
    
    # Further validation: Check if any latitudes or longitudes are out of range
    if (any(connections$from_lat < -90 | connections$from_lat > 90 |
            connections$to_lat < -90 | connections$to_lat > 90 |
            connections$from_lon < -180 | connections$from_lon > 180 |
            connections$to_lon < -180 | connections$to_lon > 180)) {
      print("Data issue detected: Invalid latitude or longitude values.")
      return(NULL)
    }
    
    # Check if there are valid connections data
    if (nrow(connections) == 0) {
      print("No valid connections for globejs.")
      return(NULL)  # If no valid connections, don't render
    }
    
    # Final check: Ensure the `arcs` data frame has no NAs
    arcs_data <- data.frame(
      from_lat = connections$from_lat,
      from_lon = connections$from_lon,
      to_lat = connections$to_lat,
      to_lon = connections$to_lon,
      flights = connections$flights
    )
    
    if (any(is.na(arcs_data))) {
      print("Data issue detected: NA values in arcs data.")
      return(NULL)  # Prevent rendering if data is still invalid
    }
    
    # Render the globe with flight arcs based on the connections data
    globejs(
      lat = c(connections$from_lat, connections$to_lat), 
      long = c(connections$from_lon, connections$to_lon),
      arcs = arcs_data,
      arcsColor = "#FF0000",  # Arc color
      arcsHeight = 0.3,
      arcsLwd = connections$flights / 40,  # Arc line thickness based on number of flights
      arcsOpacity = 0.6,
      atmosphere = TRUE, 
      color = "blue"
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)


