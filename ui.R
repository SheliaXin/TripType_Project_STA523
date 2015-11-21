library(shiny)
dataset = c("Training Set" ="training", "Test Set" ="testing", "Choose from local" = "localfile")

shinyUI(fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", h4("Dataset: "), dataset),
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
      )
      
      
    ),
    mainPanel(
      plotOutput('plot_1')
    )
  )
))