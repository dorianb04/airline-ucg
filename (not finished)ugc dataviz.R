install.packages('shiny')
install.packages('shinydashboard')
install.packages('RColorBrewer')
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

setwd('D:\\MainWorkingPlace\\market analysis')
data<- fread("data_ugc.csv") 
#as the requirement, we follow the evolution by month
data$date <-format(floor_date(data$review_date, "month"), "%Y-%m")


###import and prepare the visualization 




#graph for destination--------------------------
##graph1, evolution of satisfaction ratio 
data_g1 <- data %>%
  group_by(data$company,data$date) %>%
  summarize(
    satisfaction_ratio = sum(satisfaction == "Satisfied") / n()
  )
colnames(data_g1) <- c("company", "date",'satisfaction_ratio')
data_g1$date<-as.Date(paste0(data_g1$date, "-01"), format = "%Y-%m-%d")

##graph2, word cloud
# Function to preprocess text (basic cleaning steps)
preprocess_text <- function(text) {
  text %>%
    str_to_lower() %>% 
    str_remove_all("[^[:alnum:][:space:]]") %>%  # Remove special characters
    str_squish()  # Remove extra whitespaces
}

##graph2 Function to generate bigram word cloud
library(tidyverse)
library(tidytext)
library(wordcloud)

# Function to preprocess text
preprocess_text <- function(text) {
  text %>%
    str_to_lower() %>% 
    str_remove_all("[^[:alnum:][:space:]]") %>%  # Remove special characters
    str_squish()  # Remove extra whitespaces
}

# Function to generate bigram word cloud
generate_bigram_wordcloud <- function(data, company_name, min_bigrams) {
  
  # Filter data for the selected company
  company_data <- data %>% filter(company == company_name)
  
  # Preprocess the review text
  company_data$cleaned_review_text <- sapply(company_data$review_text, preprocess_text)
  
  # Tokenize text into bigrams
  bigrams <- company_data %>%
    unnest_tokens(bigram, cleaned_review_text, token = "ngrams", n = 2)
  
  # Split bigrams into two words for stop word filtering
  bigram_words <- bigrams %>%
    separate(bigram, into = c("word1", "word2"), sep = " ")
  
  # Remove stop words from bigrams
  data("stop_words")  # Load stop words dataset
  filtered_bigrams <- bigram_words %>%
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ")  # Reunite into bigram
  
  # Count the bigrams
  bigram_counts <- filtered_bigrams %>%
    count(bigram, sort = TRUE)
  
  # Filter bigrams that appear more than `min_bigrams` times
  frequent_bigrams <- bigram_counts %>%
    filter(n > min_bigrams)
  
  # Calculate average ratings for each bigram
  bigram_ratings <- frequent_bigrams %>%
    rowwise() %>%
    mutate(
      rating = mean(company_data$rating[grepl(bigram, company_data$cleaned_review_text, ignore.case = TRUE)], na.rm = TRUE),
      count = sum(grepl(bigram, company_data$cleaned_review_text, ignore.case = TRUE))
    )
  
  # Create a named vector of bigram frequencies
  bigram_freq <- setNames(frequent_bigrams$n, frequent_bigrams$bigram)
  
  # Define a color function based on average rating
  color_func <- function(word) {
    rating <- bigram_ratings$rating[bigram_ratings$bigram == word]
    
    if (length(rating) > 0) {
      if (rating <= 1.5) {
        return("darkred")
      } else if (rating <= 2.2) {
        return("red")
      } else if (rating <= 2.8) {
        return("gray")
      } else if (rating <= 3.5) {
        return("green")
      } else {
        return("darkgreen")
      }
    } else {
      return("black")  # Default color
    }
  }
  
  # Generate the word cloud
  wc_plot <- wordcloud(
    words = names(bigram_freq),
    freq = bigram_freq,
    min.freq = 1,
    scale = c(3, 1.5),
    colors = sapply(names(bigram_freq), color_func),
    random.order = FALSE,
    rot.per = 0.25,
    use.r.layout = FALSE,
    family = "serif",
    main = paste("Bigram WordCloud for", company_name),
    width = 1000, # width of the plot in pixels
    height = 600
  )
  
  # Return the word cloud plot object
  return(wc_plot)
}

##graph3, rank of satisfaction ratio
##graph3, rank of satisfaction ratio by airlines 
rank_satisfaction <- function(data,start_date,end_date) {
  # Calculate the overall average satisfaction ratio for each company
  data%>%
    mutate(month = floor_date(date, "month")) %>%  # Convert to first day of the month
    filter(month >= start_date & month <= end_date)
  
  ranked_data <- data %>%
    group_by(company) %>%
    summarise(avg_satisfaction_ratio = mean(satisfaction_ratio, na.rm = TRUE)) %>%
    arrange(desc(avg_satisfaction_ratio)) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # Plot: Companies ranked on x-axis, satisfaction ratio on y-axis
  plot <- ggplot(ranked_data, aes(x = reorder(company, rank), y = avg_satisfaction_ratio)) +
    geom_line(group = 1, color = "red") +  # Single line across companies
    geom_point(color = "red") +
    labs(
      title = "Rank of Satisfaction Ratio (during selected period)",
      x = "Companies (Ranked by Order)",
      y = "Average Satisfaction Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(plot)
}







#----------------------for constructing dashboard--------------------------

ui<-dashboardPage(
  
  dashboardHeader(title = 'UGC Airline'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('airline',tabName = 'airline',icon=icon("user")),
      menuItem('destination',tabName = 'destination',icon=icon("user"))
    )
  ),
  
  dashboardBody(
    #first tab
    tabItems(
      tabItem(tabName =  'airline',
             fluidRow(
               column(12,
               box(
                 title = 'filters',status = 'primary',solidHeader = TRUE,
                 height = "210px", width = "500px",
                 selectInput('company','select company:',choices=unique(data_g1$company)),
                 dateRangeInput("date_range", "Select Date Range:", 
                                start = "2020-01-01", end = "2024-12-31",min = "2020-01-01", max = "2024-12-31")
               )),
               
               column(12,
               box(
                 title = "Satisfaction Ratio Evolution", status = "primary", solidHeader = TRUE,
                 height = "500px", width = "1000px",
                 plotOutput("satisfaction_plot"))),
               
               column(12,
                box(
                  title = "Airline Word Cloud", status = "primary", solidHeader = TRUE,
                  height = "500px", width = "1000px",
                  plotOutput("word_cloud"))             ),
               column(12,
                      box(
                        title = "Rank Satisfaction Ratio by Airlines ", status = "primary", solidHeader = TRUE,
                        height = "500px", width = "1000px",
                        plotOutput("rank_satisfaction"))             )
    )
    # #second tab
    # tabItems('destination'),
    # box(plotOutput("satisfication by month"),width = 8),
    # box(selectInput('features','Features:',
    #                 c('Company','Destination')),width=4),
    # box(selectInput('Company'))
))))




server<-function(input,output){
  filtered_data <- reactive({
        selected_company<-input$company
        start_date <- format(as.Date(input$date_range[1]))
        #"%Y-%m"
        end_date <- format(as.Date(input$date_range[2]))
        data_g1 %>%
          mutate(month = floor_date(date, "month")) %>%  # Convert to first day of the month
          filter(company == selected_company,month >= start_date & month <= end_date)
      
  })
  # Render the plot
  output$satisfaction_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = month, y = satisfaction_ratio)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "darkblue") +
      labs(
        title = paste("Satisfaction Ratio for", input$company),
        x = "Month", y = "Satisfaction Ratio"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
  })
  output$word_cloud <- renderPlot({
    generate_bigram_wordcloud(data, input$company, min_bigrams =100)
  }) #rank_satisfaction
  
  output$rank_satisfaction <- renderPlot({
    rank_satisfaction(data_g1,format(as.Date(input$date_range[1])),end_date <- format(as.Date(input$date_range[2])))
  })
  
}


shinyApp(ui,server)
