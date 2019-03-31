#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(NLP);library(tm);library(R.utils); library(stringi) 
library(knitr);  library(RWeka); library(caret); library(tidyr)
library(dplyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Future SwiftKey Text Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
          sidebarPanel(
                  h2("Next Word Prediction App"),
                  h4("This app makes a prediction of the next word in the last input sentence. 
                     It based on the entered words using n-grams model (n=1 to 4).   
                     It takes into account up to the last three words."),
                  textInput("text", label = h3("Enter your text here:"), value = ),
                  submitButton("Submit"),
                  h6("Note: The word prediction model only supports phrases/words in English"),
                  h6("If your words are not specifically in the data set then the following will be returned:"), 
                  em("said one will just like")
                  ),
          mainPanel(
                  h4("Phrase you entered:"),
                  em(textOutput("entered", h5, strong("bold"))), br(), 
                  h4("Predicted next word:"),
                  div(textOutput("predicted", h4, strong("bold")), style="color:blue"),
                  h4("All the next words predicted by rank:"),
                  div(textOutput("alternatives", h4, strong("bold")), style="color:green"), br(),
                  
                  h4("Links:"),
                  tags$a(href = "https://edemmon.github.io/Data-Science-Capstone/Final_Project_Pitch_Text_Prediction_Estorm2.html", 
                         "1. Presentation"), br(), 
                  tags$a(href = "https://github.com/edemmon/Data-Science-Capstone/tree/master/text_prediction", 
                         "2. GitHub directory"),  br(),
                  tags$a(href = "https://github.com/edemmon/Data-Science-Capstone/blob/master/data_creation.R", 
                         "2. GitHub directory for data creation")  
          )
                  )
))
