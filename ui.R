#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text prediction for Data Science Capstone"),
  
  # Input text 
  sidebarLayout(
    sidebarPanel(
      textInput("text", "This application will predict the next word based on data from twitter, news articles, and blogs. 
                    Please enter up to three words to get the predicted word.", ""),
      actionButton("goButton", "Submit")
      
    ),
    
    # Output prediction
    mainPanel( h3("Predicted word"),
                strong(textOutput("nextWord")),
                tags$br(),
                DT::dataTableOutput("table")
    )
  )
)
)
