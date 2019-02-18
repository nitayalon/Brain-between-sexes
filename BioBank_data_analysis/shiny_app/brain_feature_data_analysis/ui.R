library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bio Bank brain measures data", "Bio Bank Brain visualization"),
  
  # Sidebar with selection for columns from BioBank relevant data
  sidebarLayout(
    sidebarPanel(
      selectInput("columns", 
                  "Select Columns", 
                  choices = NULL),
      radioButtons('transform_data_for_analysis',
                   'Prepare data for analysis',
                   c('Yes'= T,
                     'No'= F),
                   selected = T),
      sliderInput("n_bins",
                  "Set number of bins",
                  min = 50,
                  max = 1000,
                  value = 100,
                  step = 10)
    ),
    
    # Multiple tabs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(
                    fluidRow(
                      column(width = 11,
                             "Histogram", plotOutput("groupsHistogram")),
                      column(width = 11,h5("Red lines indicate women mixture model, 
                                           blue lines indicate men")
                             )
                      )
                    ),
                  tabPanel("EM Summary", 
                           fluidRow(
                             column(width = 7, tableOutput("EmSummaryAlternative")))
                           ,
                           fluidRow(
                             column(width = 7, tableOutput("EmSummaryNull")))
                  ),
                  tabPanel("Statistical Summary", 
                           fluidRow(
                             column(width = 7,tableOutput("StatSummary")),
                             column(width = 7,tableOutput("TconfidenceInterval")),
                             column(width = 7,tableOutput("CohensconfidenceInterval"))
                           )
                  )
      )
    )
  )
))
