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
  titlePanel("Brain Features EDA"),
  sidebarPanel(width = 3,   
  # Sidebar with a slider input for number of bins
    fileInput("brain_file","Choose a CSV file", accept = c(
      "text/csv",
      "text/comma-separated-values",
      ".csv")
    ),
    
    actionButton("selectColumns", "Select Column"),
    
    selectInput("columns", "Select Columns", choices = NULL),
    
    radioButtons("logScale", "log scale", choices = c(T,F))
  ),
  
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(8, align="center",
          plotOutput("FeatureByGenderHist")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(6, align="left",
          plotOutput("SurvivalFunctionPositve")
        ),
        column(6, align="right",
          plotOutput("SurvivalFunctionNegative",)
        )
      )
    )
  )
)
