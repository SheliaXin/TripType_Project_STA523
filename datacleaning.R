# setwd("/Users/sheliaxin/Documents/Duke/Courses/STA523/Final Proj/Final_Project_STA523")
library(dplyr)
library(RSQLite)

file <- c("train","test")
# If you want the fineline number
ifFineLine <- FALSE


for(j in 1:2){
  # load data
  data = read.csv(paste0(file[j],".csv"))
  
  ## use sql
  con = dbConnect(RSQLite::SQLite(), ":memory:")
  dbWriteTable(con, "train", data)   # data frame -> database table
  
  # all department
  depart = as.character(unique(data$DepartmentDescription))[order(unique(data$DepartmentDescription))]
  # all fineline number
  Fineline = as.character(unique(data$FinelineNumber))[order(unique(data$FinelineNumber))]
  
  # set query, for training set, we have triptype, while for testing set, we do not have triptype.
  if(j == 1){
    query <- "SELECT TripType, VisitNumber, 
              SUM(CASE WHEN ScanCount > 0 THEN 1 ELSE 0 END) AS nBuy, 
              SUM(CASE WHEN ScanCount < 0 THEN 1 ELSE 0 END) AS nReturn FROM train GROUP BY VisitNumber"
  }else{
    query <- "SELECT VisitNumber, 
              SUM(CASE WHEN ScanCount > 0 THEN 1 ELSE 0 END) AS nBuy, 
              SUM(CASE WHEN ScanCount < 0 THEN 1 ELSE 0 END) AS nReturn FROM train GROUP BY VisitNumber"
  }
  
  # group by visitnumber
  res_v <- dbSendQuery(con, query) 
  number_v <- dbFetch(res_v, n=-1)  # n=-1 means return all
  visitnumber <- number_v$VisitNumber
  
  
  ##### Department matrix ###### 
  
  # initial department data frame
  Condf <- data.frame(VisitNumber = visitnumber)
  l <- list()
  for(i in 1:length(depart)){
    sql_query <- paste0("SELECT VisitNumber, DepartmentDescription AS '",depart[i],"' FROM train WHERE DepartmentDescription = '", depart[i], "' GROUP BY VisitNumber")
    res =  dbSendQuery(con,sql_query) 
    l[[i]] <- dbFetch(res, n=-1)
    Condf <- left_join(Condf,l[[i]], by = "VisitNumber")
  }
  
  Dept_m <- Condf[,-1]
  Dept_m[!is.na(Dept_m)] = 1
  Dept_m[is.na(Dept_m)] = 0
  #save(Dept_data,file="Dept_matrix.RData")
  Dept_m <- cbind(VisitNumber = Condf$VisitNumber,Dept_m)  #  visiternumber, department
  
  if(ifFineLine){
    ##### Fineline matrix #####
    Finedf <- data.frame(VisitNumber = visitnumber)
    l <- list()
    for(i in 1:length(Fineline)){
      sql_query <- paste0("SELECT VisitNumber, FinelineNumber AS '",Fineline[i],"' FROM train WHERE FinelineNumber = '", Fineline[i], "' GROUP BY VisitNumber")
      res =  dbSendQuery(con,sql_query) 
      l[[i]] <- dbFetch(res, n=-1)
      Finedf <- left_join(Finedf,l[[i]], by = "VisitNumber")
      print(i)
    }
    
    Fine_m <- Finedf[,-1]
    Fine_m[!is.na(Fine_m)] = 1
    Fine_m[is.na(Fine_m)] = 0
    #save(Fine_m,file="FinelineNumber.RData")
    #load("FinelineNumber.RData")
    
    # save data
    if( j == 1){
      ## new data
      newData <- cbind(Dept_m, Fine_m)
      newData <- right_join(number_v, newData, by = "VisitNumber")
      save(newData, file = "newData_train.RData")
    }else{
      ## new data
      testData <- cbind(Dept_m, Fine_m)
      testData <- right_join(number_v, newData, by = "VisitNumber")
      save(testData, file = "newData_test.RData")
    }
  }
  
  # save data
  if( j ==1){
    newData_without_F <- right_join(number_v, Dept_m, by = "VisitNumber")
    save( newData_without_F, file = "newData_train_without_F.RData")
  }else{
    testData_without_F <- right_join(number_v, Dept_m, by = "VisitNumber")
    save( testData_without_F, file = "newData_test_without_F.RData")
  }
  
  dbClearResult(res)
  dbDisconnect(con)  # Closing the connection
}

