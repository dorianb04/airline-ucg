# Install required packages if not already installed
if (!require("reticulate")) install.packages("reticulate")
if (!require("plotly")) install.packages("plotly")
if (!require("data.table")) install.packages("data.table")
if (!require("stringr")) install.packages("stringr")
if (!require("dotenv")) install.packages("dotenv")

# Load libraries
library(reticulate)
library(plotly)
library(data.table)
library(stringr)
library(dotenv)

load_dot_env()

api_key <- Sys.getenv("GROQ_API_KEY")
cat("API key exists:", api_key != "", "\n")

# Create Python dictionary to store variables
py_run_string("
import os
os.environ['GROQ_API_KEY'] = ''  # Create the environment variable
")

# Set the API key in Python's environment
py$os$environ['GROQ_API_KEY'] <- api_key


# Install required Python packages
use_python("C:/Users/doria/anaconda3/envs/ml/python.exe", required = TRUE)


# Import Python modules
top2vec <- import("top2vec")
umap <- import("umap")

# Read your data
data <- fread("C:/Users/doria/OneDrive/ESSEC-CENTRALESUPELEC/term1/marketing/airline-ucg/data_ugc.csv")

airline_name <- "Asiana Airlines"

# Filter data on specific airline
data <- data %>% filter(Airline == airline_name)

reviews <- data$Review
ratings <- data$Rating

# Create Top2Vec model
model <- top2vec$Top2Vec(
  documents = reviews,
  workers = as.integer(8),
  min_count = as.integer(50),
)

# Get document embeddings and reduce dimensionality
doc_embeddings <- model$document_vectors
umap_reducer <- umap$UMAP(
  n_neighbors = as.integer(5),
  n_components = as.integer(2),
  min_dist=0.1,
  metric = 'cosine',
  random_state = as.integer(42)
)
umap_embeddings <- umap_reducer$fit_transform(doc_embeddings)

# Get topics and their words
doc_topics <- model$get_documents_topics(as.list(seq(0, length(reviews)-1)))[[1]]
topic_words <- model$topic_words

# First create all topic strings
create_topic_strings <- function(topic_words_matrix) {
  topics_list <- apply(topic_words_matrix, 1, function(row) {
    # Take first 20 words and format them
    words <- row[1:20]
    quoted_words <- paste0('"', words, '"')
    paste(quoted_words, collapse="  ")
  })
  
  # Create a numbered list of topics
  formatted_topics <- paste(
    sapply(seq_along(topics_list), 
           function(i) paste0("Topic ", i, ": ", topics_list[i])),
    collapse="\n\n"
  )
  return(formatted_topics)
}

# Format all topics into one string
all_topics <- create_topic_strings(topic_words)

# Create the Python function for batch title generation
py_run_string('
from groq import Groq
import os

client = Groq()

def generate_titles_batch(all_topics):
    prompt = f"""Below are sets of keywords from airline review topics. For each topic, generate a short, clear 3-4 word title that captures its main theme.
    Be concise and specific, avoiding generic words like "experience" or "service" when possible.
    Format your response as a numbered list matching the topic numbers.
    
    {all_topics}"""
    
    chat_completion = client.chat.completions.create(
        messages=[
            {
                "role": "system",
                "content": "You are a specialized assistant that creates concise, specific titles from keywords. Return titles in a numbered list format."
            },
            {
                "role": "user",
                "content": prompt
            }
        ],
        model="llama-3.3-70b-versatile",
        temperature=0.3,
        max_tokens=1024,
        top_p=1,
        stream=False
    )
    
    return chat_completion.choices[0].message.content
')

# Generate all titles at once
titles_response <- py$generate_titles_batch(all_topics)

# Print the response
cat("\nGenerated Titles:\n")
cat(titles_response)

# Process the response into a vector of titles
# You might need to adjust this based on the exact format of the response
process_titles_response <- function(response) {
  # Split response into lines
  lines <- strsplit(response, "\n")[[1]]
  # Keep only numbered lines
  lines <- grep("^[0-9]+\\.", lines, value = TRUE)
  # Extract just the titles (remove numbers and dots)
  titles <- gsub("^[0-9]+\\.\\s*", "", lines)
  return(titles)
}

topic_titles <- process_titles_response(titles_response)

# Create plotting dataframe
plot_data <- data.frame(
  x = umap_embeddings[,1],
  y = umap_embeddings[,2],
  topic = topic_titles[doc_topics + 1],
  rating = ratings,
  review = reviews
)

# save plot data inside of a file named based on the comapny name
write.csv(plot_data, paste0("data/topic_clustering/", airline_name, "_plot_data.csv"), row.names = FALSE)

# Create visualization
p <- plot_ly(plot_data, 
             x = ~x,
             y = ~y,
             color = ~topic,
             size = ~rating,
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
      text =  ' Reviews - Topic Analysis',
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
      title = list(text = '<b>Generated Topics</b>'),
      itemsizing = 'constant'
    ),
    hoverlabel = list(bgcolor = "white"),
    plot_bgcolor = '#FAFAFA',
    paper_bgcolor = 'white'
  )

# Display the plot
p

# Print summary statistics for each topic
cat("\nTopic Analysis Summary:\n")
for(topic in unique(plot_data$topic)) {
  topic_ratings <- plot_data$rating[plot_data$topic == topic]
  cat(sprintf("\n\nTopic: %s", topic))
  cat(sprintf("\nNumber of reviews: %d", length(topic_ratings)))
  cat(sprintf("\nAverage rating: %.2f / 5.0", mean(topic_ratings)))
  cat("\nRating distribution:")
  cat(sprintf("\n  1★: %d (%.1f%%)", 
              sum(topic_ratings == 1),
              100 * sum(topic_ratings == 1) / length(topic_ratings)))
  cat(sprintf("\n  2★: %d (%.1f%%)", 
              sum(topic_ratings == 2),
              100 * sum(topic_ratings == 2) / length(topic_ratings)))
  cat(sprintf("\n  3★: %d (%.1f%%)", 
              sum(topic_ratings == 3),
              100 * sum(topic_ratings == 3) / length(topic_ratings)))
  cat(sprintf("\n  4★: %d (%.1f%%)", 
              sum(topic_ratings == 4),
              100 * sum(topic_ratings == 4) / length(topic_ratings)))
  cat(sprintf("\n  5★: %d (%.1f%%)", 
              sum(topic_ratings == 5),
              100 * sum(topic_ratings == 5) / length(topic_ratings)))
  cat("\n---")
}
