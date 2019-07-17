library(shiny)
library(dplyr)
library(ggplot2)
shinyServer(function(input, output,session) {
  
  brain.files.path <- ("~/mastersdegree/Thesis/DAPHNA_JOEL/Data/")
  
  info <- eventReactive(input$brain_file, {
    inFile <- input$brain_file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    # Changes in read.table 
    tbl <- read.csv(inFile$datapath, header = F, stringsAsFactors = F)
    print(head(tbl))
    vars <- names(tbl)
    print(vars)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    
    tbl
  })
  
  output$FeatureByGenderHist <- renderPlot({
    f <- info()
    plot.data <- select(f, V1, input$columns)
    names(plot.data) <- c("Gender","Feature")
    ggplot(plot.data,aes(x=Feature)) +
      geom_histogram(bins = 40) + facet_wrap(~Gender)}
    ,height = 400,width = 600)
  
  output$SurvivalFunctionPositve <- renderPlot({
    f <- info()
    plot_title <- "survival function - positive"
    plot.data <- select(f, V1, input$columns)
    names(plot.data) <- c("Gender","Feature")
    surviv <- plot.data$Feature[plot.data$Feature > 0]
    prec <- seq(0,max(surviv) + 1,0.1)
    tmp <- c()
    for(i in prec){
      tmp <- c(tmp,
               sum(surviv > i) / length(surviv))
    }
    if(input$logScale){
      plot(log(tmp), main = plot_title)
    }
    else{
      plot(tmp, main = plot_title)
    }
  })
  
  output$SurvivalFunctionNegative <- renderPlot({
    f <- info()
    plot_title <- "survival function - negative"
    plot.data <- select(f, V1, input$columns)
    names(plot.data) <- c("Gender","Feature")
    surviv <- plot.data$Feature[plot.data$Feature < 0]
    prec <- seq(0,min(surviv) - 1,-0.1)
    tmp <- c()
    for(i in prec){
      tmp <- c(tmp,
               sum(surviv < i) / length(surviv))
    }
    if(input$logScale){
      plot(log(tmp), main = plot_title)
    }
    else{
      plot(tmp, main = plot_title)
    }
  })
  
})
