#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #build a reactive expression that is run 
    #when the predict button is pressed
    
    choices <- eventReactive(input$predict, {
        result <- wordPredictionV2(input = input$inputText)
        rownames(result) <- 1:nrow(result)
        result$index <- rownames(result) 
        wordChoices <- as.list(result$LastWord)
        selectInput("Suggestions", 
                    "Chose Prediction",
                    choices = wordChoices
        )
    })
    
    
    output$predict_dropdown <- renderUI({
        choices()
    })
    
    
    #see to default with null 
    #http://stackoverflow.com/questions/40152857/how-to-dynamically-populate-dropdown-box-choices-in-shiny-dashboard

    
})
