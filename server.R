library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
# By default, the file size limit is 5MB. 
# Here we'll raise limit to 40MB.
options(shiny.maxRequestSize = 40*1024^2)

shinyServer(function(input, output, session) {
  
  observe(
    {
      updateSliderInput(session,"max_size", min = input$min_size)
      
      if(input$clear > 0)
      {
        updateCheckboxGroupInput(session, inputId = "departments", selected= "ACCESSORIES")
      }
    }
  )

  departments = reactive(
    {
      if(input$all_select == "Select All" & input$plotSelect == "bargraph2"){
        as.character(unique(d$DepartmentDescription))[order(unique(d$DepartmentDescription))]
      }else
      {
        input$departments
      }
      
    }
  )
  
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
      if(input$plotSelect == "bargraph2")
      {
      test_data <- table() %>% as.data.frame()
      #train_data <- read.csv("train.csv") %>% as.data.frame()
      #department_name <- test_data$DepartmentDescription %>% as.data.frame() %>% distinct()
      
      department_daily_test <- test_data %>% 
        group_by(DepartmentDescription, Weekday) %>%
        summarise(Sales = n())
      #department_daily_train <- train_data %>% 
       # group_by(DepartmentDescription, Weekday) %>%
        #summarise(Sales = n())
      
      ####Function Summary: put a data set we are interested in into the function, 
      ####then plot out daily sale across all departments.
      
      ###Input: 
      ##dataset: a data set (test_data or train_data)
      ###Output:
      ## a daily sale plot across all departments
      
      all_department <- function(dataset){
        sales <- dataset %>%
          group_by(Weekday) %>%
          summarise(Sales = n())
        names(sales) <- c("Date", "Sales")
        plot <- ggplot(sales, aes(x = Date, y = Sales)) + 
          geom_bar(stat = "identity")+
          labs(x = "", y = "")
        return(plot)
      }
      
      ####Function Summary: Put the data set and department names we are interested in into the function,
      ####then plot out daily sale of those departments and use different color to distinguish different departments
      
      ###Input:
      ##depart: a list of departments, which we are interested in
      ##dataset: a data set (test_data or train_data)
      ###Output:
      ##a daily sale plot of departments in the list
      
      daily_sale_graph <- function(depart, dataset){
        sales <- dataset %>% 
          filter(DepartmentDescription %in% depart) %>% 
          group_by(DepartmentDescription, Weekday) %>%
          summarise(Sales = n())
        names(sales) <- c("Department", "Date", "Sales")
        plot <- ggplot(sales, aes(x = Date, y = Sales, fill = factor(Department))) + 
          geom_bar(stat = "identity")+
          labs(x = "", y = "", fill = "Department")
        return(plot)
      }
      
      
      #all_department(train_data)
      ##pick departments
      depart <- departments()
      ##plot daily sale of departments above in test_data
      daily_sale_graph(depart, table())
      ##plot daily sale of "DAIRY" department in train_data
      #daily_sale_graph("DAIRY", train_data)
    }
    }
  )
})