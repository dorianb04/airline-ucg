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

# Function to generate bigram word cloud
generate_bigram_wordcloud <- function(data, company_name, min_bigrams) {
  
  # Filter data for the selected company
  company_data <- data %>% filter(company == company_name)
  
  # Preprocess the review text
  company_data$cleaned_review_text <- sapply(company_data$review_text, preprocess_text)
  
  # Tokenize text into bigrams
  bigrams <- company_data %>%
    unnest_tokens(bigram, cleaned_review_text, token = "ngrams", n = 2)
  
  # Count the bigrams
  bigram_counts <- bigrams %>%
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
  par(mar = c(1, 1, 1, 1))
  wc_plot <- wordcloud(
    words = names(bigram_freq),
    freq = bigram_freq,
    min.freq = 1,
    scale = c(3,1.5),
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
  
  # # Print bigram ratings and frequencies
  # for (i in 1:nrow(bigram_ratings)) {
  #   cat(sprintf("Bigram: %s, Avg. Rating: %.2f, Count: %d\n", 
  #               bigram_ratings$bigram[i], bigram_ratings$rating[i], bigram_ratings$count[i]))
  # }
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
                 height = "600px", width = "1000px",
                 plotOutput("satisfaction_plot")),
               
               column(12,
                box(
                  title = "Airline Word Cloud", status = "primary", solidHeader = TRUE,
                  height = "600px", width = "1000px",
                  plotOutput("word_cloud"))             )
    )
    # #second tab
    # tabItems('destination'),
    # box(plotOutput("satisfication by month"),width = 8),
    # box(selectInput('features','Features:',
    #                 c('Company','Destination')),width=4),
    # box(selectInput('Company'))
)))))




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
    generate_bigram_wordcloud(data, input$company, min_bigrams =200)
  })
  
}

shinyApp(ui,server)
