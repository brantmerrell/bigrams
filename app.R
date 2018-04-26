library(shiny)
source("bigram.R")
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
    
    bigram(input$file1$datapath, output = "image")
  })
  
}

shinyApp(ui, server)
