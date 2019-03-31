#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Future SwiftKey Text Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
          sidebarPanel(
                  h2("Next Word Prediction App"),
                  em("This app makes a prediction of the next word in the last input sentence. 
                     It based on the entered words using n-grams model (n=2..4).   
                     It takes into account 3 - 1 last words."),
                  textInput("text", label = h3("Enter your text here:"), value = ),
                  submitButton("Submit"),
                  h6("Note: The word prediction model only supports phrases/words in english")
                  ),
          mainPanel(
                  h4("Phrase you entered:"),
                  em(textOutput("entered", h5, strong("bold"))), br(), 
                  h4("Predicted next word:"),
                  div(textOutput("predicted", h4, strong("bold")), style="color:blue"),
                  h4("All the next words predicted by rank:"),
                  div(textOutput("alternatives", h4, strong("bold")), style="color:green"), br(),
                  
                  h4("Links:"),
                  tags$a(href = "http://rpubs.com/Andrey_Vlasenko/SwiftKey_WordPredictionVer2", 
                         "1. Presentation"), br(), 
                  tags$a(href = "https://github.com/Andrey-Vlasenko/SwiftKeyProject_WordPrediction/Ver2", 
                         "2. GitHub directory") 
          )
                  )
))
