# Loan's part - create word clouds based on the review_title variable
# Ensure that the correct working directory is selected
getwd()
# Load the necessary packages
library(wordcloud)
library(tm)
library(stopwords)
library(wordcloud2)
library(dplyr)
# Assign the readed data  to "data"
data <- read.csv("Munged.csv")
# assign a specific column of "data" to "reviews"
reviews <- data$review_title
# Clean the data
# Specify the "reviews" as character vectors in a corpus; assign to texts.corpus
texts.corpus <- Corpus(VectorSource(reviews))
# Lowercase all terms
texts.corpus <- tm_map(texts.corpus, content_transformer(tolower))
# Punctuation removal
texts.corpus <- tm_map(texts.corpus, removePunctuation)
# Numerical removal
texts.corpus <- tm_map(texts.corpus, removeNumbers)
# stopwords removal
texts.corpus <- tm_map(texts.corpus, removeWords, stopwords("en"))
# Removed desired words
texts.corpus <- tm_map(texts.corpus, removeWords, c("can", "just", "amazon", "product", "overall"))
# create a term-document matrix and analyze its weights
tdm <- TermDocumentMatrix(texts.corpus)
# inspect frequent words
inspect(tdm)
# Matrix conversion
terms <- as.matrix(tdm)
# Rank the list by frequency in decreasing order
textCounts <- sort(rowSums(terms), decreasing = T)
# View the first few rows
head(textCounts)
# See the last few rows
tail(textCounts)
# Create a dataframe containing terms and frequencies
cloudFrame <- data.frame(word = names(textCounts), freq = textCounts)
# Filter the dataframe with a frequency at least 50
cloudFrame <- cloudFrame %>% filter(freq >= 50)
# Set the margin size for the plot
par(mar = c(1, 1, 1, 1))
# Fix the seed to avoid changing results
set.seed(1234)
# Use the wordcloud package to create a word cloud
wordcloud(names(textCounts), textCounts, colors = brewer.pal(3, "Dark2"), random.order = F, min.freq = 20)
# Terms aggregated by count
word <- aggregate(freq~word, data = cloudFrame, FUN = sum)
# Use the wordcloud2 package to create a word cloud
wordcloud2(word, color = "random-light", size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1, fontFamily = "Officina Sans Bold", shape = "star", backgroundColor = "black")

# Loan's part - create word clouds based on the review_content variable
# assign a specific column of "data" to "reviews2"
reviews2 <- data$review_content
# Clean the data
# Specify the "reviews2" as character vectors in a corpus; assign to texts.corpus
texts.corpus2 <- Corpus(VectorSource(reviews2))
# Lowercase all terms
texts.corpus2 <- tm_map(texts.corpus2, content_transformer(tolower))
# Punctuation removal
texts.corpus2 <- tm_map(texts.corpus2, removePunctuation)
# Numerical removal
texts.corpus2 <- tm_map(texts.corpus2, removeNumbers)
# stopwords removal
texts.corpus2 <- tm_map(texts.corpus2, removeWords, stopwords("en"))
# Removed desired words
texts.corpus2 <- tm_map(texts.corpus2, removeWords, c("can", "just", "use", "product", "order", "purchased", "overall"))
# create a term-document matrix and analyze its weights
tdm2 <- TermDocumentMatrix(texts.corpus2)
# inspect frequent words
inspect(tdm2)
# Matrix conversion
terms2 <- as.matrix(tdm2)
# Rank the list by frequency in decreasing order
textCounts2 <- sort(rowSums(terms2), decreasing = T)
# View the first few rows
head(textCounts2)
# Create a dataframe containing terms and frequencies
cloudFrame2 <- data.frame(word = names(textCounts2), freq = textCounts2)
# Filter the dataframe with a frequency at least 150
cloudFrame2 <- cloudFrame2 %>% filter(freq >= 150)
# Set the margin size for the plot
par(mar = c(1, 1, 1, 1))
# Fix the seed to avoid changing results
set.seed(1234)
# Use the wordcloud package to create a word cloud
wordcloud(names(textCounts2), textCounts2, colors = brewer.pal(3, "Dark2"), random.order = F, min.freq = 100, max.words = 100)
# Terms aggregated by count
word2 <- aggregate(freq~word, data = cloudFrame2, FUN = sum)
par(mar = rep(0, 4))
# Use the wordcloud2 package to create a word cloud
wordcloud2(word2, color = "random-light", size = 1, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1, fontFamily = "Officina Sans Bold", shape = "star", backgroundColor = "black")

# Loan's part - create a word cloud with Shiny
# Load the necessary packages
library(shiny)
# Create a user interface
ui <- fluidPage(
  # Title of application
  titlePanel("Word Cloud"),
  # Put radio buttons and a numeric input on the sidebar
  sidebarLayout(
    # Display a word cloud
    mainPanel(wordcloud2Output(outputId = "wordcloud2")),
    sidebarPanel(
      # User can select variables: review_title or review_content using a radio button
      radioButtons(inputId = "variable",
                   label = "Pick a variable:",
                   choices = c("review_title" = "title",
                               "review_content" = "content"),
                   selected = "title"),
      # There is the option to select the shape of the word cloud
      radioButtons(inputId = "shape",
                   label = "Choose a shape if you want:",
                   choices = c("circle (default)", "cardioid", "diamond",
                               "triangle-forward", "triangle",
                               "pentagon", "star")),
      # There is an option to input the word size
      numericInput(inputId = "size",
                   label = "Input a number between 1 and 30 for size:",
                   value = 1, min = 1, max = 30),
    )
  )
)
# There are two variables, so I create two functions that read specific columns of data
readData <- function() {
  # Assign the readed data  to "data"
  data <- read.csv("Munged.csv")
  # assign a specific column of "data" to "reviews"
  reviews <- data$review_title
  # Clean the data
  # Specify the "reviews" as character vectors in a corpus; assign to texts.corpus
  texts.corpus <- Corpus(VectorSource(reviews))
  # Lowercase all terms
  texts.corpus <- tm_map(texts.corpus, content_transformer(tolower))
  # Punctuation removal
  texts.corpus <- tm_map(texts.corpus, removePunctuation)
  # Numerical removal
  texts.corpus <- tm_map(texts.corpus, removeNumbers)
  # stopwords removal
  texts.corpus <- tm_map(texts.corpus, removeWords, stopwords("en"))
  # Removed desired words
  texts.corpus <- tm_map(texts.corpus, removeWords, c("can", "just", "amazon", "product", "overall"))
  # create a term-document matrix and analyze its weights
  tdm <- TermDocumentMatrix(texts.corpus)
  # Matrix conversion
  terms <- as.matrix(tdm)
  # Rank the list by frequency in decreasing order
  textCounts <- sort(rowSums(terms), decreasing = T)
  # Create a dataframe containing terms and frequencies
  cloudFrame <- data.frame(word = names(textCounts), freq = textCounts)
  # Filter the dataframe with a frequency at least 50
  cloudFrame <- cloudFrame %>% filter(freq >= 50)
  # Terms aggregated by count
  word <- aggregate(freq~word, data = cloudFrame, FUN = sum)
  # List the terms and frequency
  return(word)
}
readData2 <- function() {
  data <- read.csv("Munged.csv")
  reviews2 <- data$review_content
  texts.corpus2 <- Corpus(VectorSource(reviews2))
  texts.corpus2 <- tm_map(texts.corpus2, content_transformer(tolower))
  texts.corpus2 <- tm_map(texts.corpus2, removePunctuation)
  texts.corpus2 <- tm_map(texts.corpus2, removeNumbers)
  texts.corpus2 <- tm_map(texts.corpus2, removeWords, stopwords("en"))
  texts.corpus2 <- tm_map(texts.corpus2, removeWords, c("can", "just", "use", "product", "order", "purchased", "overall"))
  tdm2 <- TermDocumentMatrix(texts.corpus2)
  terms2 <- as.matrix(tdm2)
  textCounts2 <- sort(rowSums(terms2), decreasing = T)
  cloudFrame2 <- data.frame(word = names(textCounts2), freq = textCounts2)
  cloudFrame2 <- cloudFrame2 %>% filter(freq >= 150)
  word2 <- aggregate(freq~word, data = cloudFrame2, FUN = sum)
  return(word2)
}
# To plot a word cloud, define a server
server <- function(input, output) {
  # Create a reactive expression
  df_source <- reactive({
    # Update the output when the variable is changed
    # Use readData() if the review_title variable is selected, otherwise use readData2()
    if (input$variable == "title"){
      df <- readData()
      return(df)
    } else {
      df <- readData2()
      return(df)
    }})
  # Activate the wordcloud2
  output$wordcloud2 <- renderWordcloud2({
    # Confirm that the variable has already been selected
    df <- req(df_source())
    # While the session is underway, changes can be made to the word cloud
    wordcloud2(df, shape = input$shape, size = input$size, color = "random-light", fontFamily = "Officina Sans Bold", backgroundColor = "black")
  })
}
# Launch the shiny app
shinyApp(ui = ui, server = server)
