#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  # Application title
  titlePanel("Predicting the Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        #img(src = "crystal-ball-512.png", height = 100, width = 100, align = "center"),
        textInput("inputText"
                  , label = p("Enter two or more words and click predict to see suggestions:"
                              , style = "color:blue")
                  ),
        actionButton("predict", "Predict")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       uiOutput("predict_dropdown"),
       textOutput("updatedText")
       #Add this is for the time, but needs to go be the choices for text box
    )
  )
))
