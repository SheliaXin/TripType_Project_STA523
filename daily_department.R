library(dplyr)
library(magrittr)
library(ggplot2)
####set directory and load dataset
setwd("/Users/andrea/Final_Project_STA523")
test_data <- read.csv("test.csv") %>% as.data.frame()
train_data <- read.csv("train.csv") %>% as.data.frame()
department_name <- test_data$DepartmentDescription %>% as.data.frame() %>% distinct()


####calculate daily sale of each department
####and save the data into a new data frame

####we can use it to make shiny faster, if necessary
department_daily_test <- test_data %>% 
  group_by(DepartmentDescription, Weekday) %>%
  summarise(Sales = n())
department_daily_train <- train_data %>% 
  group_by(DepartmentDescription, Weekday) %>%
  summarise(Sales = n())

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
    filter(DepartmentDescription == depart) %>% 
    group_by(DepartmentDescription, Weekday) %>%
    summarise(Sales = n())
  names(sales) <- c("Department", "Date", "Sales")
  plot <- ggplot(sales, aes(x = Date, y = Sales, fill = factor(Department))) + 
    geom_bar(stat = "identity")+
    labs(x = "", y = "", fill = "Department")
  return(plot)
}


all_department(train_data)
##pick departments
depart <- c("BOYS WEAR", "PERSONAL CARE", "CELEBRATION")
##plot daily sale of departments above in test_data
daily_sale_graph(depart, test_data)
##plot daily sale of "DAIRY" department in train_data
daily_sale_graph("DAIRY", train_data)



