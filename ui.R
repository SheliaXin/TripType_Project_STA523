library(shiny)

dataset = c("Training Set" ="training", "Test Set" ="testing", "Choose from local" = "localfile")
plotSelect = c("Average Size of Trip Density Plot" = "density",
                "Average Size of Trip Bargraph" = "bargraph1",
                "Department by Weekday Bargraph" = "bargraph2",
                "Department Relationship Chart" = "relationship")
days_departments = c("Days" = "days", "Departments" = "dept")
d = read.csv("test.csv", header=TRUE)
departments = c("Select All", as.character(unique(d$DepartmentDescription)))
  

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
        selectInput("days_departments", h4("Days or Departments?"), days_departments)
      ),
      conditionalPanel(
        condition = "input.plotSelect == 'density'",
        selectInput("days_departments", h4("Days or Departments?"), days_departments)
      ),
      conditionalPanel(
        condition = "input.days_departments == 'dept'",
        checkboxGroupInput("departments", h4("Departments: "), departments)
      )
      
    ),
    mainPanel(
      plotOutput('plot_1')
    )
  )
))