library(shiny)
library(ggplot2)
#library(plyr)
library(RSQLite)
library(dplyr)
library(reshape2)
library(RColorBrewer)
# By default, the file size limit is 5MB. 
# Here we'll raise limit to 40MB.
options(shiny.maxRequestSize = 40*1024^2)

shinyServer(function(input, output, session) {
  
  observe(
    {
      updateSliderInput(session,"max_size", min = input$min_size + 10)
      
      if(input$clear > 0)
      {
        updateCheckboxGroupInput(session, inputId = "departments", selected= "ACCESSORIES")
      }
      if(input$clear1 > 0)
      {
        updateCheckboxGroupInput(session, inputId = "departments1", 
                                 selected= c("ACCESSORIES", "AUTOMOTIVE"))
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
  
  basket_min = reactive(
    {
      input$min_size
    }
  )
  
  basket_max = reactive(
    {
      input$max_size
    }
  )
  
  departments = reactive(
    {
      if(input$all_select == "Select All" & input$plotSelect == "bargraph2"){
        table()$DepartmentDescription
      }else if(input$all_select1 == 'Select Specific Departments' &
               input$plotSelect == 'relationship')
      {
        input$departments1
        
      }else if(input$all_select1 == 'Select All' &
               input$plotSelect == 'relationship')
      {
        unique(table()$DepartmentDescription)[order(unique(table()$DepartmentDescription))]
        
      }else
      {
        input$departments
      }
      
    }
  )
  
  bubble = reactive(
    {
      if(input$dataset == "training")
      {
        data1 = as.matrix(read.csv("relationshipMatrix_train.csv"))
        rownames(data1) = data1[, "X"]
        data1 = data1[, -1]
        class(data1) <- "numeric"
        colnames(data1) = rownames(data1)
        data1
      }else if(input$dataset == 'testing'){
        data1 = as.matrix(read.csv("relationshipMatrix_test.csv"))
        rownames(data1) = data1[, "X"]
        data1 = data1[, -1]
        class(data1) <- "numeric"
        colnames(data1) = rownames(data1)
        data1
      }
    }  
  )
  
  
  
  
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
    }else if(input$plotSelect == "relationship")
    {
      ### INPUT  ###
      depart_select <- departments()
      
      longData <- melt(bubble()[depart_select,depart_select])
      longData$value = as.numeric(longData$value)
      
      # Define palette
      myPalette <- colorRampPalette(c("white", "dodgerblue", "dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"),
                                    space = "rgb")
      zp1 <- ggplot(longData,
                    aes(x = Var2, y = Var1, fill = value))
      zp1 <- zp1 + geom_tile()
      zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))  #colours = myPalette(100)
      zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
      zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
      zp1 <- zp1 + coord_equal()
      zp1 <- zp1 + theme_bw() 
      zp1 <- zp1 + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
      print(zp1)
      
      #Top20 <- head(longData[order(longData$value,decreasing = TRUE),],n=20)
      #Top20
    }else if(input$plotSelect == "bargraph1")
    {
      test = table()
      
      visit_grouped = group_by(test, VisitNumber)
      size_trip = summarise(visit_grouped, n_items = sum(ScanCount))
      
      weekday = test[,c("VisitNumber", "Weekday")] %>%
        unique()
      weekday_joined = left_join(x = size_trip, y = weekday, 
                                 by = "VisitNumber")
      
      # what the person can change, min and max of basket 
      min = basket_min()
      max = basket_max()
      
      # proportion that are within a certain size
      big_baskets = filter(weekday_joined, n_items > min & n_items < max)
      grouped = group_by(weekday_joined, Weekday)
      grouped_big = group_by(big_baskets, Weekday)
      
      total_df = summarise(grouped, total = n())
      big_df = summarise(grouped_big, big = n())
      joined = left_join(total_df, big_df, by = "Weekday")
      
      
      #proportion of basket size, which changes based on min and max
      percent = mutate(joined, prop = big/total)
      
      percent = arrange(percent, Weekday)
      arranged = rbind(percent[2,], percent[6,], percent[7,],
                       percent[5,], percent[1,], percent[3,], percent[4,])
      
      # plot proportion, which can change 
      barplot(arranged$prop, names.arg = c("Mon", "Tues", "Wed",
                                           "Thurs", "Fri", "Sat", "Sun"),
              ylab = "Proportion of Trips with Specified Basket Size", xlab = "Weekday",
              main = "Basket Size per Day of Week") 
      
    
    }
      
    
    
    }
  )
})