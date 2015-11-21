library(shiny)
# By default, the file size limit is 5MB. 
# Here we'll raise limit to 40MB.
options(shiny.maxRequestSize = 40*1024^2)

shinyServer(function(input, output) {
  table <- reactive(
    {
    if(input$dataset == 'training'){
      read.csv("train.csv")
    }else if(input$dataset == 'testing'){
      read.csv("test.csv")
    }else if(input$dataset == 'localfile'){
      # input$file1 will be NULL initially. After the user selects and uploads a file, 
      # it will be a data frame with 'name', 'size', 'type', and 'datapath' columns. 
      # The 'datapath' column will contain the local filenames where the data can be found.
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    }
  })
  
  
  ### just for example ###
  output$plot_1 = renderPlot(
    {
      hist(table()[,1])
    }
    
  )
})