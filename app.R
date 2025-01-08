# Install required packages if not already installed
if (!require("reticulate")) install.packages("reticulate")
if (!require("plotly")) install.packages("plotly")
if (!require("data.table")) install.packages("data.table")
if (!require("stringr")) install.packages("stringr")
if (!require("dotenv")) install.packages("dotenv")
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("igraph")) install.packages("igraph", type="binary")
if (!require("threejs")) install.packages("threejs")

# Load libraries
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(reticulate)
library(plotly)
library(dotenv)
library(threejs)


# Load and preprocess data
# check if file is on the local machine
if (file.exists("data/data_ugc.csv")) {
  data <- fread("data/data_ugc.csv")
} else {
  data <- fread("https://github.com/dorianb04/airline-ucg/raw/refs/heads/main/data/data_ugc.csv?download=")
}

# Convert 'Review Rate' to date format
data$date <- format(floor_date(data$`Review Rate`, "month"), "%Y-%m")
data$date <- as.Date(paste0(data$date, "-01"), format = "%Y-%m-%d")

# Summarize data by Airline and date
data_g1 <- data %>%
  group_by(Airline, date) %>%
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

data_g2 <- data %>%
  group_by(Airline, date, Departure) %>%
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

data_g3 <- data %>%
  group_by(Airline, date, Destination) %>%
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

# Preprocessing for globe visualization
data_g4 <- data %>%
  group_by(Airline, date, Departure, Destination, Dep_Latitude, Dep_Longitude, Dest_latitude, Dest_longitude) %>%
  summarize(
    count = n(),
    .groups = 'drop'
  )

generate_wordcloud <- function(data, airline_name, min_bigrams = 2) {
  pal <- brewer.pal(8, "Dark2")
  
  # Filter data for the airline company selected
  airline_data <- data %>% filter(Airline == airline_name)
  
  # Check if there is data
  if (nrow(airline_data) == 0) {
    plot.new()
    text(0.5, 0.5, "No data available for the selected airline", cex = 1.5)
    return()
  }
  
  # Bigrams creation
  bigram_counts <- airline_data %>%
    unnest_tokens(bigram, Review, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    filter(n > min_bigrams)
  
  # Check if there are bigrams which respect criteria
  if (nrow(bigram_counts) == 0) {
    plot.new()
    text(0.5, 0.5, "No bigrams found with the specified minimum frequency", cex = 1.5)
    return()
  }
  
  # Wordcloud creation
  par(mar = c(0,0,0,0))  # Remove margins
  wordcloud(
    words = bigram_counts$bigram,
    freq = bigram_counts$n,
    min.freq = 50,  # Reduced minimum frequency
    scale = c(8, 1),  # Increased scale for larger words
    colors = pal,
    random.order = FALSE,
    max.words = 20  # Increased maximum number of words
  )
}

# Function to rank airlines by satisfaction ratio
rank_satisfaction <- function(data, start_date, end_date) {
  filtered_data <- data %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  
  if (nrow(filtered_data) == 0) {
    return(NULL)
  }
  
  ranked_data <- filtered_data %>%
    group_by(Airline) %>%
    summarize(
      satisfied = mean(satisfaction_ratio, na.rm = TRUE),
      unsatisfied = 1 - satisfied
    ) %>%
    arrange(desc(satisfied)) %>%
    ungroup()
  
  # Create the stacked bar plot
  plot_ly(ranked_data) %>%
    add_trace(
      x = ~satisfied,
      y = ~reorder(Airline, satisfied),
      type = 'bar',
      orientation = 'h',
      name = 'Satisfied',
      text = ~sprintf("%.1f%%", satisfied * 100),
      textposition = 'inside',
      insidetextfont = list(color = 'white'),
      marker = list(
        color = '#1f77b4',
        opacity = 0.8,
        line = list(width = 1)
      )
    ) %>%
    add_trace(
      x = ~unsatisfied,
      y = ~reorder(Airline, satisfied),
      type = 'bar',
      orientation = 'h',
      name = 'Unsatisfied',
      text = ~sprintf("%.1f%%", unsatisfied * 100),
      textposition = 'inside',
      insidetextfont = list(color = 'white'),
      marker = list(
        color = '#d62728',
        opacity = 0.8,
        line = list(width = 1)
      )
    ) %>%
    layout(
      barmode = 'stack',
      title = list(
        text = 'Satisfaction Ratio Distribution by Airline',
        font = list(size = 16)
      ),
      xaxis = list(
        title = 'Percentage',
        zeroline = FALSE,
        gridcolor = 'rgb(240,240,240)',
        tickformat = ',.0%'
      ),
      yaxis = list(
        title = '',
        zeroline = FALSE,
        gridcolor = 'rgb(240,240,240)'
      ),
      plot_bgcolor = '#FAFAFA',
      paper_bgcolor = 'white',
      margin = list(l = 150),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    )
}

##########################################
# UI
ui <- dashboardPage(
  dashboardHeader(title = 'Choose your Airline'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Route Network', tabName = 'network', icon = icon("route")),
      menuItem('Airline Analysis', tabName = 'airline', icon = icon("plane")),
      menuItem('Departure Analysis', tabName = 'departure', icon = icon("map-marker-alt")),
      menuItem('Destination Analysis', tabName = 'destination', icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      # Route Network tab
      tabItem(
        tabName = 'network',
        fluidRow(
          column(12,
                 box(
                   title = 'Filters', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   selectInput('airline_network', 'Select Airline:', 
                               choices = unique(data$Airline),
                               selected = "Air France")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = 'Global Airline Network', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   globeOutput("airline_globe", height = "600px")
                 )
          )
        )
      ),
      
      # Airline Analysis tab
      tabItem(
        tabName = 'airline',
        fluidRow(
          column(12,
                 box(
                   title = 'Filters', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   selectInput('Airline', 'Select Airline:', choices = unique(data_g1$Airline)),
                   dateRangeInput("date_range", "Select Date Range:", 
                                  start = "2020-01-01", end = "2024-12-31", 
                                  min = "2020-01-01", max = "2024-12-31")
                 )
          )
        ),
        fluidRow(
          column(8,
                 box(
                   title = "Review Topics Analysis",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   height = "600px",  # Set fixed height
                   plotlyOutput("clustering_plot", height="530px")
                 )
          ),
          column(4,
                 box(
                   title = "Topic Analysis",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   height = "600px",  # Match height with clustering box
                   selectInput("selected_topic", "Select Topic:", choices = NULL),
                   # Metrics tiles
                   fluidRow(
                     column(6,
                            valueBoxOutput("topic_review_count", width = 12)
                     ),
                     column(6,
                            valueBoxOutput("topic_avg_rating", width = 12)
                     )
                   ),
                   # Rating distribution
                   plotlyOutput("topic_rating_dist", height = "320px", width = "100%")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Satisfaction Ratio Evolution", status = "primary", solidHeader = TRUE,
                   width = 12,
                   plotOutput("satisfaction_plot")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Airline Word Cloud", status = "primary", solidHeader = TRUE,
                   width = 12,
                   plotOutput("word_cloud")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Rank Satisfaction Ratio by Airlines", status = "primary", solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("rank_satisfaction")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Ratings by Feature", status = "primary", solidHeader = TRUE,
                   width = 12,
                   selectInput("rating_feature", "Select Feature:", 
                               choices = c("Legroom", "Seat Comfort", "Entertainment", 
                                           "Customer Service", "Value", "Cleanliness", 
                                           "Check In", "Food")),
                   plotOutput("rating_feature_plot")
                 )
          )
        )
      ),
      # Departure Analysis tab
      tabItem(
        tabName = 'departure',
        fluidRow(
          column(12,
                 box(
                   title = 'Filters', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   selectInput('Departure', 'Select Departure:', choices = unique(data$Departure), selected = "Paris"),
                   dateRangeInput("date_range_dep", "Select Date Range:", 
                                  start = "2020-01-01", end = "2024-12-31", 
                                  min = "2020-01-01", max = "2024-12-31")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Satisfaction Ratio Evolution by Departure", status = "primary", solidHeader = TRUE,
                   width = 12,
                   selectInput('selected_plot_airlines', 'Select Airlines for the Plot:', 
                               choices = unique(data_g2$Airline), 
                               selected = "Air France",
                               multiple = TRUE),
                   plotOutput("satisfaction_dep_plot")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Ratings by Feature (Departure)", status = "primary", solidHeader = TRUE,
                   width = 12,
                   selectInput("rating_feature_dep", "Select Feature:", 
                               choices = c("Legroom", "Seat Comfort", "Entertainment", 
                                           "Customer Service", "Value", "Cleanliness", 
                                           "Check In", "Food")),
                   plotOutput("rating_feature_dep_plot")
                 )
          )
        )
      ),
      # Destination Analysis tab
      tabItem(
        tabName = 'destination',
        fluidRow(
          column(12,
                 box(
                   title = 'Filters', status = 'primary', solidHeader = TRUE,
                   width = 12,
                   selectInput('Destination', 'Select Destination:', choices = unique(data$Destination), selected = "Paris"),
                   dateRangeInput("date_range_dest", "Select Date Range:", 
                                  start = "2020-01-01", end = "2024-12-31", 
                                  min = "2020-01-01", max = "2024-12-31")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Satisfaction Ratio Evolution by Destination", status = "primary", solidHeader = TRUE,
                   width = 12,
                   selectInput('selected_airlines', 'Select Airlines for the Plot:', 
                               choices = unique(data_g3$Airline), 
                               selected = "Air France",
                               multiple = TRUE),
                   plotOutput("satisfaction_dest_plot")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Ratings by Feature (Destination)", status = "primary", solidHeader = TRUE,
                   width = 12,
                   selectInput("rating_feature_dest", "Select Feature:", 
                               choices = c("Legroom", "Seat Comfort", "Entertainment", 
                                           "Customer Service", "Value", "Cleanliness", 
                                           "Check In", "Food")),
                   plotOutput("rating_feature_dest_plot")
                 )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Reactive filtered data for airline
  filtered_data <- reactive({
    data_g1 %>%
      filter(Airline == input$Airline, 
             date >= as.Date(input$date_range[1]) & date <= as.Date(input$date_range[2]))
  })
  
  # Reactive filtered data for departure
  filtered_data_dep <- reactive({
    data_g2 %>%
      filter(Departure == input$Departure, 
             date >= as.Date(input$date_range_dep[1]) & date <= as.Date(input$date_range_dep[2]))
  })
  
  filtered_data_dep_airlines <- reactive({
    filtered_data_dep() %>%
      filter(Airline %in% input$selected_plot_airlines)
  })
  
  # Reactive filtered data for destination
  filtered_data_dest <- reactive({
    data_g3 %>%
      filter(Destination == input$Destination, 
             date >= as.Date(input$date_range_dest[1]) & date <= as.Date(input$date_range_dest[2]))
  })
  
  filtered_data_dest_airlines <- reactive({
    filtered_data_dest() %>%
      filter(Airline %in% input$selected_airlines)
  })
  
  # Reactive expression for embeddings and clustering
  review_clustering <- reactive({
    req(input$Airline)
    
    # Filter reviews for selected airline
    airline_data <- read.csv(paste0("data/topic_clustering/", input$Airline, "_plot_data.csv"))
    
    airline_data
  })
  
  output$satisfaction_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = satisfaction_ratio)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue") +
      labs(title = paste("Satisfaction Ratio for", input$Airline),
           x = "Month", y = "Satisfaction Ratio") +
      theme_minimal()
  })
  
  output$word_cloud <- renderPlot({
    generate_wordcloud(data, input$Airline)
  })
  
  output$rank_satisfaction <- renderPlotly({
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    
    filtered_rank_data <- data_g1 %>%
      filter(date >= start_date & date <= end_date)
    
    rank_satisfaction(filtered_rank_data, start_date, end_date)
  })
  
  output$rating_feature_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = .data[[input$rating_feature]])) +
      geom_line(color = "green", size = 1) +
      geom_point(color = "darkgreen") +
      labs(title = paste(input$rating_feature, "Trend for", input$Airline),
           x = "Month", y = input$rating_feature) +
      theme_minimal()
  })
  
  output$satisfaction_dep_plot <- renderPlot({
    departure_data <- filtered_data_dep_airlines()
    
    if (nrow(departure_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available for the selected filters"), size = 6) +
        theme_void()
    } else {
      ggplot(departure_data, aes(x = date, y = Rating, color = Airline)) +
        geom_line(size = 1) +
        geom_point() +
        labs(title = paste("Rating Evolution for Departure:", input$Departure),
             x = "Month", y = "Average Rating") +
        scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  output$rating_feature_dep_plot <- renderPlot({
    departure_data <- filtered_data_dep()
    selected_feature <- input$rating_feature_dep
    
    avg_ratings <- departure_data %>%
      group_by(Airline) %>%
      summarize(avg_rating = mean(.data[[selected_feature]], na.rm = TRUE))
    
    if (nrow(avg_ratings) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available for the selected Departure"), size = 6) +
        theme_void()
    } else {
      ggplot(avg_ratings, aes(x = Airline, y = avg_rating, fill = Airline)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Comparison of", selected_feature, "for Departure:", input$Departure),
             x = "Airline", y = paste("Average", selected_feature)) +
        scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
        theme_minimal()
    }
  })
  
  output$satisfaction_dest_plot <- renderPlot({
    destination_data <- filtered_data_dest_airlines()
    
    if (nrow(destination_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available for the selected filters"), size = 6) +
        theme_void()
    } else {
      ggplot(destination_data, aes(x = date, y = Rating, color = Airline)) +
        geom_line(size = 1) +
        geom_point() +
        labs(title = paste("Rating Evolution for Destination:", input$Destination),
             x = "Month", y = "Average Rating") +
        scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  output$rating_feature_dest_plot <- renderPlot({
    destination_data <- filtered_data_dest()
    selected_feature <- input$rating_feature_dest
    
    avg_ratings <- destination_data %>%
      group_by(Airline) %>%
      summarize(avg_rating = mean(.data[[selected_feature]], na.rm = TRUE))
    
    if (nrow(avg_ratings) == 0) {
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "No data available for the selected destination"), size = 6) +
        theme_void()
    } else {
      ggplot(avg_ratings, aes(x = Airline, y = avg_rating, fill = Airline)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Comparison of", selected_feature, "for Destination:", input$Destination),
             x = "Airline", y = paste("Average", selected_feature)) +
        scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
        theme_minimal()
    }
  })
  
  output$clustering_plot <- renderPlotly({
    req(review_clustering())
    clustered_data <- review_clustering()
    
    plot_ly(clustered_data, 
            x = ~x,
            y = ~y,
            color = ~topic,
            size = ~rating**5,
            sizes = c(10, 30),
            type = 'scatter',
            mode = 'markers',
            marker = list(
              opacity = 0.7,
              line = list(width = 1)
            ),
            text = ~paste(
              '<b>Topic:</b>', topic,
              '<br><b>Rating:</b>', rating,
              '<br><b>Review:</b>', substr(review, 1, 150), "..."
            ),
            hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = paste('Review Topics Analysis for', input$Airline),
          font = list(size = 16)
        ),
        xaxis = list(
          title = 'Topic Space Dimension 1',
          zeroline = FALSE,
          gridcolor = 'rgb(240,240,240)'
        ),
        yaxis = list(
          title = 'Topic Space Dimension 2',
          zeroline = FALSE,
          gridcolor = 'rgb(240,240,240)'
        ),
        legend = list(
          title = list(text = '<b>Topics</b>'),
          itemsizing = 'constant'
        ),
        hoverlabel = list(bgcolor = "white"),
        plot_bgcolor = '#FAFAFA',
        paper_bgcolor = 'white'
      )
  })
  
  observe({
    plot_data <- review_clustering()
    updateSelectInput(session, "selected_topic",
                      choices = sort(unique(plot_data$topic)))
  })
  
  # Create review count metric
  output$topic_review_count <- renderValueBox({
    req(input$selected_topic)
    plot_data <- review_clustering()
    
    review_count <- sum(plot_data$topic == input$selected_topic)
    
    valueBox(
      value = format(review_count, big.mark = ","),
      subtitle = "Reviews",
      icon = icon("comments"),
      color = "green"
    )
  })
  
  # Create average rating metric
  output$topic_avg_rating <- renderValueBox({
    req(input$selected_topic)
    plot_data <- review_clustering()
    
    avg_rating <- mean(plot_data$rating[plot_data$topic == input$selected_topic])
    
    valueBox(
      value = sprintf("%.1f", avg_rating),
      subtitle = "Avg Rating",
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$topic_rating_dist <- renderPlotly({
    req(input$selected_topic)
    plot_data <- review_clustering()
    
    topic_data <- plot_data %>%
      filter(topic == input$selected_topic) %>%
      count(rating) %>%
      mutate(
        percentage = n / sum(n) * 100,
        rating_label = paste0(rating, "★")
      ) %>%
      arrange(desc(rating))  # Keep highest ratings first
    
    # Create a horizontal bar chart with styled bars
    fig <- plot_ly() %>%
      add_trace(
        data = topic_data,
        x = ~percentage,
        y = ~rating_label,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = c('#2ecc71', '#87D37C', '#f1c40f', '#e67e22', '#e74c3c'),
          line = list(color = 'rgba(255,255,255,0.5)', width = 1)
        ),
        text = ~sprintf("%.1f%%", percentage),
        textposition = 'auto',
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Count: %{customdata}<br>",
          "Percentage: %{x:.1f}%",
          "<extra></extra>"
        ),
        customdata = ~n
      ) %>%
      layout(
        title = list(
          text = "Rating Distribution",
          x = 0.5,
          xanchor = 'center'
        ),
        xaxis = list(
          title = "Percentage of Reviews",
          showgrid = TRUE,
          gridcolor = 'rgba(0,0,0,0.1)',
          zeroline = FALSE,
          range = c(0, max(topic_data$percentage) * 1.1)
        ),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE
        ),
        showlegend = FALSE,
        plot_bgcolor = '#FAFAFA',
        paper_bgcolor = 'white',
        margin = list(l = 50, r = 30, t = 50, b = 30),
        bargap = 0.2
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$airline_globe <- renderGlobe({
    # Get filtered data
    connections <- data_g4 %>%
      filter(Airline == input$airline_network,
             !is.na(Dep_Latitude), 
             !is.na(Dep_Longitude),
             !is.na(Dest_latitude),
             !is.na(Dest_longitude)) %>%
      mutate(
        Dep_Latitude = as.numeric(Dep_Latitude),
        Dep_Longitude = as.numeric(Dep_Longitude),
        Dest_latitude = as.numeric(Dest_latitude),
        Dest_longitude = as.numeric(Dest_longitude)
      )
    
    # Create arcs data
    arcs <- data.frame(
      from_lat = connections$Dep_Longitude,
      from_lon = connections$Dep_Latitude,
      to_lat = connections$Dest_longitude,
      to_lon = connections$Dest_latitude
    )
    
    # Globe visualization
    globejs(
      lat = c(connections$Dep_Longitude, connections$Dest_longitude),
      long = c(connections$Dep_Latitude, connections$Dest_latitude),
      arcs = arcs,
      arcsColor = "#FF0000",
      arcsHeight = 0.3,
      arcsOpacity = 0.6,
      atmosphere = TRUE
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
