setwd("/Users/andrea/Final_Project_STA523")
test_data <- read.csv("test.csv") %>% as.data.frame()
train_data <- read.csv("train.csv") %>% as.data.frame()
Fineline <- train_data$FinelineNumber

#the precentage of 500 fineline number
sum(sort(Fineline, decreasing = TRUE)[1:500])/length(fine)
#600
sum(sort(Fineline, decreasing = TRUE)[1:600])/length(fine)
#650
sum(sort(Fineline, decreasing = TRUE)[1:650])/length(fine)
#1000
sum(sort(Fineline, decreasing = TRUE)[1:1000])/length(fine)
