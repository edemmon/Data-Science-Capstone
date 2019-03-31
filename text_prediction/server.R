#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(NLP);library(tm);library(R.utils); library(stringi) 
library(knitr);  library(RWeka); library(caret); library(tidyr)
library(dplyr)
load("ngrams.RData")

textcleanse <- function(input, type){
        # type of output, vector or string
        # v = character vector, s = string
        if (type=="s") {
                input <- tolower(input) 
                input <- removePunctuation(input)
                input <- removeNumbers(input)
                input <- unlist(strsplit(input, " "))
                #input <- setdiff(input, stopwords(kind = "en"))
                input <- input[grepl('[[:alpha:]]', input)]
                input <- paste(input, sep=" ", collapse=" ") 
        } else {
                input <- tolower(input) 
                input <- removePunctuation(input)
                input <- removeNumbers(input)
                input <- rev(unlist(strsplit(input, " ")))
                #input <- setdiff(input, stopwords(kind = "en"))
                input <- input[grepl('[[:alpha:]]', input)]
        }
        return(input)
}

predictnextword <- function( input, maxwords ) {
        #-----------------------------
        # Constants
        #-----------------------------
        # ngram weights
        quadgramwt <- 1
        trigramwt  <- 0.6
        bigramwt   <- 0.1
        unigramwt  <- 0.05
        
        #-----------------------------
        # Load ngrams data
        #-----------------------------
        
        # Cleanse the input text
        input <- textcleanse(input, "v")
        
        # Inputs for searching quadgram, trigram and unigrams
        input.tri <- paste(input[3], input[2], input[1], sep = ' ')
        input.bi  <- paste(input[2], input[1], sep = ' ')
        input.uni <- input[1]
        
        #-----------------------------
        # Searching next word in ngrams
        #-----------------------------
        quadgramsearch <- grepl(paste0("^", input.tri, "$"), quad_df$prior)
        quadgramsubset <- quad_df[quadgramsearch,]
        if (sum(quadgramsearch) == 0) {
                quadgramsubset <- quad_df[1,]
                quadgramsubset$nextword <- "n.a."}
        quadgramsearch_12 <- grepl(paste0("^", input.bi, "$"), quad_df$t12)
        quadgramsubset_12 <- quad_df[quadgramsearch_12,]
        if (sum(quadgramsearch_12) == 0) {
                quadgramsubset_12 <- quad_df[1,]
                quadgramsubset_12$nextword <- "n.a."}
        quadgramsearch_23 <- grepl(paste0("^", input.bi, "$"), quad_df$t23)
        quadgramsubset_23 <- quad_df[quadgramsearch_23,]
        if (sum(quadgramsearch_23) == 0) {
                quadgramsubset_23 <- quad_df[1,]
                quadgramsubset_23$nextword <- "n.a."}
        quadgramsearch_13 <- grepl(paste0("^", input.bi, "$"), quad_df$t13)
        quadgramsubset_13 <- quad_df[quadgramsearch_13,]
        if (sum(quadgramsearch_13) == 0) {
                quadgramsubset_13 <- quad_df[1,]
                quadgramsubset_13$nextword <- "n.a."}
        
        
        trigramsearch <- grepl(paste0("^", input.bi, "$"), tri_df$prior)
        trigramsubset <- tri_df[trigramsearch,]
        if (sum(trigramsearch) == 0) {
                trigramsubset <- tri_df[1,]
                trigramsubset$nextword <- "n.a."}
        trigramsearch_1 <- grepl(paste0("^", input.uni, "$"), tri_df$firstname)
        trigramsubset_1 <- tri_df[trigramsearch_1,]
        if (sum(trigramsearch_1) == 0) {
                trigramsubset_1 <- tri_df[1,]
                trigramsubset_1$nextword <- "n.a."}
        
        bigramsearch  <- grepl(paste0("^",input.uni,"$"), bi_df$prior)
        bigramsubset  <- bi_df[bigramsearch,]
        if (sum(bigramsearch) == 0) {
                bigramsubset <- bi_df[1,]
                bigramsubset$nextword <- "n.a."}
        
        
        uni_df      <- uni_df[order(uni_df$freq, decreasing = T),]
        unigramsubset  <- uni_df[1:maxwords,]
        
        
        # Compute probability scores for the ngrams
        unigramsubset$score  <- unigramsubset$freq  / sum(uni_df$freq)      * unigramwt
        bigramsubset$score   <- bigramsubset$freq   / sum(bigramsubset$freq)        * bigramwt
        trigramsubset$score  <- trigramsubset$freq  / sum(trigramsubset$freq)  * trigramwt
        trigramsubset_1$score  <- trigramsubset_1$freq  / sum(trigramsubset_1$freq)  * trigramwt
        quadgramsubset$score <- quadgramsubset$freq / sum(quadgramsubset$freq) * quadgramwt
        quadgramsubset_12$score <- quadgramsubset_12$freq / sum(quadgramsubset_12$freq) * quadgramwt
        quadgramsubset_23$score <- quadgramsubset_23$freq / sum(quadgramsubset_23$freq) * quadgramwt
        quadgramsubset_13$score <- quadgramsubset_13$freq / sum(quadgramsubset_13$freq) * quadgramwt
        
        
        #-----------------------------
        # Predict the word based on ngram results
        #-----------------------------
        # Determine the predicted words and their score
        # predicted word for the dataframe
        nextword <- c(quadgramsubset$nextword, trigramsubset$nextword, 
                      bigramsubset$nextword, unigramsubset$term,
                      trigramsubset_1$nextword, 
                      quadgramsubset_12$nextword, quadgramsubset_23$nextword, quadgramsubset_13$nextword)
        # scores for the predicted words
        score <- c(quadgramsubset$score, trigramsubset$score, 
                   bigramsubset$score, unigramsubset$score,
                   trigramsubset_1$score,
                   quadgramsubset_12$score, quadgramsubset_23$score, quadgramsubset_13$score) 
        
        
        # Build the predicted word dataframe
        predictedword <- data.frame(next_word = nextword, score = score, stringsAsFactors = F)
        
        # Remove n.a.
        predictedword <- subset(predictedword, next_word != "n.a.")
        
        #-----------------------------
        # Final processing of predicted words
        #-----------------------------
        # Summarize the dataframe
        suppressWarnings(suppressMessages(library(dplyr)))
        predictedword <- predictedword %>%
                group_by(next_word) %>%
                summarize(score=sum(score))
        
        # Sort the dataframe using the score
        predictedword <- predictedword[order(predictedword$score, decreasing = T),]
        
        # Remove duplicates
        predictedword <- unique(predictedword$next_word)
        # Retain only the max results
        predictedword <- predictedword[1:maxwords]
        # Remove spaces
        predictedword <- predictedword[grepl('[[:alpha:]]',predictedword)]
        
        #output <- paste(predictedword, sep=",", collapse=", ")
        output <- predictedword
        #-----------------------------
        # Return final outcome
        #-----------------------------
        return(output)       
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        nextwords <- reactive({predictnextword(input$text, 5)})
        #top1 <-nextwords[[1]][1]
        #others <-nextwords[[1]][-1]
        output$entered <- renderText({input$text})
        output$predicted  <- renderText(paste(nextwords()[[1]][1]))
        output$alternatives  <- renderText(paste(nextwords()))
  
  
})
