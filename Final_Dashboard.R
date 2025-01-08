# install.packages("tm")
# install.packages("wordcloud")

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
library(umap)
library(plotly)

# Set up Python environment and load the SentenceTransformer model globally

########### CHANGE THIS PATH TO YOUR OWN ANACONDA ENVIRONMENT ##################
use_python("C:/Users/doria/anaconda3/envs/ml", required = TRUE)
################################################################################
py_run_string("
from sentence_transformers import SentenceTransformer
model = SentenceTransformer('Alibaba-NLP/gte-base-en-v1.5', trust_remote_code=True)
")

# Load and preprocess data
data <- fread("data_ugc.csv")

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
  wordcloud(
    words = bigram_counts$bigram,
    freq = bigram_counts$n,
    min.freq = 100,
    scale = c(5, 1),
    colors = pal,
    random.order = FALSE,
    max.words = 50
  )
}

# Function to rank airlines by satisfaction ratio
rank_satisfaction <- function(data, start_date, end_date) {
  filtered_data <- data %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  
  if (nrow(filtered_data) == 0) {
    return(NULL)  # Return NULL if no data in the date range
  }
  
  ranked_data <- filtered_data %>%
    group_by(Airline) %>%
    summarize(avg_satisfaction_ratio = mean(satisfaction_ratio, na.rm = TRUE)) %>%
    arrange(desc(avg_satisfaction_ratio)) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  ggplot(ranked_data, aes(x = reorder(Airline, rank), y = avg_satisfaction_ratio)) +
    geom_line(group = 1, color = "red") +
    geom_point(color = "red") +
    labs(
      title = "Rank of Satisfaction Ratio (during selected period)",
      x = "Companies (Ranked by Order)",
      y = "Average Satisfaction Ratio"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Updated get_embeddings function using the globally loaded model
get_embeddings <- function(texts) {
  embeddings_py <- py$model$encode(texts)
  embeddings_r <- py_to_r(embeddings_py)
  # Convert to matrix if necessary
  if (!is.matrix(embeddings_r)) {
    embeddings_r <- matrix(unlist(embeddings_r), nrow = length(texts), byrow = TRUE)
  }
  return(embeddings_r)
}

# Create UMAP clustering function
create_clustering <- function(embeddings, n_neighbors = 15, min_dist = 0.1) {
  umap_config <- umap.defaults
  umap_config$n_neighbors <- n_neighbors
  umap_config$min_dist <- min_dist
  
  umap_result <- umap(embeddings, config = umap_config)
  return(umap_result)
}

##########################################
# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = 'Choose your Airline'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Airline Analysis', tabName = 'airline', icon = icon("plane")),
      menuItem('Departure Analysis', tabName = 'departure', icon = icon("map-marker-alt")),
      menuItem('Destination Analysis', tabName = 'destination', icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
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
                   plotOutput("rank_satisfaction")
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
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Review Clustering", status = "primary", solidHeader = TRUE,
                   width = 12,
                   sliderInput("sample_size", "Number of Reviews to Sample:", 
                               min = 10, max = 500, value = 100, step = 10),
                   plotlyOutput("clustering_plot")
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
server <- function(input, output) {
  
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
  review_embeddings <- reactive({
    req(input$Airline)
    
    # Filter reviews for selected airline
    airline_data <- data %>%
      filter(Airline == input$Airline)
    
    # Determine number of reviews to sample based on user input
    n_sample <- min(nrow(airline_data), input$sample_size)
    sampled_reviews <- airline_data %>% 
      sample_n(n_sample)
    
    # Get embeddings using the global model
    withProgress(message = 'Creating embeddings...', {
      embeddings <- get_embeddings(sampled_reviews$Review)
    })
    
    # Create UMAP clustering using fixed parameters
    withProgress(message = 'Clustering...', {
      umap_result <- create_clustering(
        embeddings, 
        n_neighbors = 15,      # constant value
        min_dist = 0.1         # constant value
      )
    })
    
    # Return clustering results as a dataframe
    data.frame(
      UMAP1 = umap_result$layout[,1],
      UMAP2 = umap_result$layout[,2],
      Review = sampled_reviews$Review,
      Rating = sampled_reviews$Rating
    )
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
  
  output$rank_satisfaction <- renderPlot({
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
    req(review_embeddings())
    
    # Convert Rating to factor for discrete coloring
    clustered_data <- review_embeddings()
    clustered_data$Rating <- factor(clustered_data$Rating)
    
    plot_ly(data = clustered_data,
            x = ~UMAP1,
            y = ~UMAP2,
            type = "scatter",
            mode = "markers",
            color = ~Rating,            # Map color to factor(Rating)
            colors = "Set1",            # Choose a discrete color palette
            text = ~Review,
            hovertemplate = paste(
              "<span style='text-align:left;white-space:normal;display:block;width:300px;height:auto;'>",
              "<b>Rating:</b> %{marker.color}<br>",
              "<b>Review:</b><br>%{text}",
              "</span><extra></extra>"
            )
    ) %>%
      layout(
        title = paste("Review Clustering for", input$Airline),
        xaxis = list(title = "UMAP Dimension 1"),
        yaxis = list(title = "UMAP Dimension 2"),
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black"),
          align = "left",
          width = 300
        ),
        legend = list(
          title = list(text = "<b>Rating Levels</b>"),  # Bold title for the legend
          x = 1.02,    # Position legend to the right of the plot
          y = 1,       # Align legend with the top of the plot
          bgcolor = 'rgba(255, 255, 255, 0.5)', # Semi-transparent white background
          bordercolor = 'black',                # Border color for legend
          borderwidth = 1,
          orientation = "v"  # Vertical orientation
        ),
        margin = list(r = 150)  # Increase right margin to accommodate legend
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
