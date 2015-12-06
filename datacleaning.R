# setwd("/Users/sheliaxin/Documents/Duke/Courses/STA523/Final Proj/Final_Project_STA523")
data = read.csv("train.csv")

## use sql
library(RSQLite)
con = dbConnect(RSQLite::SQLite(), ":memory:")
str(con)

dbWriteTable(con, "train", data)   # data frame -> database table

# group by department
# res_d = dbSendQuery(con,"SELECT DepartmentDescription, SUM(ScanCount) As NI, COUNT(*) AS n FROM train GROUP BY DepartmentDescription") 
# number_d <- dbFetch(res_d, n=-1)
# depart <- number_d$DepartmentDescription
# depart <- names(table(data$DepartmentDescription))
depart = as.character(unique(data$DepartmentDescription))[order(unique(data$DepartmentDescription))]
Fineline = as.character(unique(data$FinelineNumber))[order(unique(data$FinelineNumber))]


# group by visitnumber
res_v = dbSendQuery(con,"SELECT VisitNumber, 
                    SUM(CASE WHEN ScanCount > 0 THEN 1 ELSE 0 END) AS nBuy,
                    SUM(CASE WHEN ScanCount < 0 THEN 1 ELSE 0 END) AS nReturn FROM train GROUP BY VisitNumber") 
number_v <- dbFetch(res_v, n=-1)
visitnumber <- number_v$VisitNumber

# initial connection data frame
Condf <- data.frame(VisitNumber = visitnumber)

# construct connection data frame 
library(dplyr)
l <- list()
for(i in 1:length(depart)){
  sql_query <- paste0("SELECT VisitNumber, DepartmentDescription AS '",depart[i],"' FROM train WHERE DepartmentDescription = '", depart[i], "' GROUP BY VisitNumber")
  res =  dbSendQuery(con,sql_query) 
  l[[i]] <- dbFetch(res, n=-1)
  Condf <- left_join(Condf,l[[i]], by = "VisitNumber")
}


##### Department matrix ######
Dept_data <- Condf
Dept_tem <- Condf[,-1]
Dept_tem[!is.na(Connect)] = 1
Dept_tem[is.na(Connect)] = 0
Dept_data <- cbind(number_v[,1:2],Dept_tem)  # triptype, visiternumber, de
#save(Dept_data,file="Dept_matrix.RData")
########

# connection data frame without visitnumber
#Connect <- Condf[,-1]
#Connect[!is.na(Connect)] = 1

# Fineline
Finedf <- data.frame(VisitNumber = visitnumber)
l <- list()
for(i in 1:length(Fineline)){
  sql_query <- paste0("SELECT VisitNumber, FinelineNumber AS '",Fineline[i],"' FROM train WHERE FinelineNumber = '", Fineline[i], "' GROUP BY VisitNumber")
  res =  dbSendQuery(con,sql_query) 
  l[[i]] <- dbFetch(res, n=-1)
  Finedf <- left_join(Finedf,l[[i]], by = "VisitNumber")
  print(i)
}



## Fineline matrix

Fine_m <- Finedf[,-1]
Fine_m[!is.na(Fine_m)] = 1
Fine_m[is.na(Fine_m)] = 0
#save(Fine_m,file="FinelineNumber.RData")
#load("FinelineNumber.RData")

## new data
newData <- cbind(Dept_data,Fine_m)
newData <- right_join(data[,c("VisitNumber","Weekday")], newData,
                     by = "VisitNumber")
newData <- right_join(number_v, newData, by = "VisitNumber")
save(newData, file = "newData_train.RData")

dbClearResult(res)
dbClearResult(res_d)
dbClearResult(res_v)
dbDisconnect(con)  # Closing the connection
