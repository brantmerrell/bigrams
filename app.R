library(shiny)
bigram <- function(txtFile){
  # read file
  words <- readLines(txtFile)
  
  # split text into individual words by spaces, slashes, dashes, commas, periods, and semicolons
  words <- unlist(strsplit(words, "[\n\r\t /,;]"))
  
  # remove punctuation that precedes or trails words (but not possessive  apostrophes)
  words <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", words)
  
  # remove empty elements from words vector
  words <- words[grepl(".", words)]
  
  # begin vector of bigrams as empty
  bigrams <- c()
  
  # iterate through words to build biagrams vector
  for(n in 2:length(words)){
    bigrams <- c(bigrams,
                 paste(words[n-1], words[n]))
  }
  
  bigrams <- table(bigrams)
  
  # create histogram of bigrams
  return(hist(matrix(bigrams),
         main = paste("Histogram of bigrams"),
         xlab = "Tally of bigrams",
         ylab = "Frequency of tallies",
         border = "blue",
         col = "green"))
}
# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Josh Merrell Programming Exercise"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose text File")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      imageOutput("hist")
    )
  )
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    req(input$file1)
    
    bigram(input$file1$datapath)
  })
  
}

shinyApp(ui, server)
