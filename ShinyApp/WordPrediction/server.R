#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #build a reactive expression that is run 
    #when the predict button is pressed
    
    choices <- eventReactive(input$predict, {
        if (str_count(input$inputText, pattern = " ") + 1 >= 2) {
            result <- wordPrediction(input = input$inputText)
            rownames(result) <- 1:nrow(result)
            result$index <- rownames(result) 
            wordChoices <- as.list(result$LastWord)
        }
        else {
            wordChoices <- NULL
        }
            selectInput("Suggestions", 
                        "Choose Prediction",
                        choices = wordChoices
        
        )
    })
    
    output$predict_dropdown <- renderUI({
        choices()
    })
    
    predictionSelection <- eventReactive(input$Suggestions, {
        selectedText <- input$Suggestions
    })
 
    updatedInputText <- eventReactive(input$inputText, {
        newInputText <- input$inputText
    })
       
    output$updatedText <- renderText({
        paste("Output: ",updatedInputText(),predictionSelection())
    }
    )

    #STILL NEED TO WORK OUT HOW TO UPDATE THE INPUT TEXT BASED ON CHOICE
    
})
