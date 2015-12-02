library(shiny)

dataset = c("Training Set" ="training", "Test Set" ="testing", "Choose from local" = "localfile")
plotSelect = c("Basket Size by Day Bargraph" = "bargraph1",
               "Department by Weekday Bargraph" = "bargraph2",
               "Department Relationship Chart" = "relationship")
selectAll = c("Select All", "Select Specific Departments")
d = read.csv("test.csv", header=TRUE)
departments = as.character(unique(d$DepartmentDescription))[order(unique(d$DepartmentDescription))]
  

shinyUI(fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", h4("Dataset: "), dataset),
      selectInput("plotSelect", h4("Plot: "), plotSelect),
      conditionalPanel(
        condition = "input.dataset == 'localfile'",
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ),
      conditionalPanel(
        condition = "input.plotSelect == 'bargraph1'",
        sliderInput("min_size", h4("Minimum Basket Size"), 
                    value = 1, step = 10,min = -30, max = 450),
        sliderInput("max_size", h4("Maximum Basket Size"), 
                    value = 1, step = 10,min = -30, max = 450)
      ), 
      conditionalPanel(
        condition = "input.plotSelect == 'bargraph2' ",
        selectInput("all_select", h4("All or Specific Departments?"), selectAll)
      ),
      conditionalPanel(
        condition = "input.all_select == 'Select Specific Departments' &
          input.plotSelect == 'bargraph2'",
        actionButton("clear", label="Clear All"),
        checkboxGroupInput("departments", h4("Departments:"), departments, 
                           selected = "ACCESSORIES")
      )
      
    ),
    mainPanel(
      plotOutput('plot_1')
    )
  )
))