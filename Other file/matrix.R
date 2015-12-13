data = read.csv("train.csv")


library(RSQLite)
con = dbConnect(RSQLite::SQLite(), ":memory:")
str(con)


dbWriteTable(con, "train", data)   # data frame -> database table
dbListTables(con)

# group by department
res_d = dbSendQuery(con,"SELECT DepartmentDescription, SUM(ScanCount) As NI, COUNT(*) AS n FROM train GROUP BY DepartmentDescription") 
number_d <- dbFetch(res_d, n=-1)
depart <- number_d$DepartmentDescription


# group by visitnumber
res_v = dbSendQuery(con,"SELECT VisitNumber, SUM(ScanCount) As NI, COUNT(*) AS n FROM train GROUP BY VisitNumber") 
number_v <- dbFetch(res_v, n=-1)
visitnumber <- number_v$VisitNumber


Condf <- data.frame(VisitNumber = visitnumber)


library(dplyr)
l <- list()
for(i in 1:length(depart)){
  sql_query <- paste0("SELECT VisitNumber, DepartmentDescription AS '",depart[i],"' FROM train WHERE DepartmentDescription = '", depart[i], "' GROUP BY VisitNumber")
  res =  dbSendQuery(con,sql_query) 
  l[[i]] <- dbFetch(res, n=-1)
  Condf <- left_join(Condf,l[[i]], by = "VisitNumber")
}

Connect <- Condf[,-1]
Connect[!is.na(Connect)] = 1

n= length(depart)
ConMatrix <- matrix(0, n, n)
colnames(ConMatrix) <- depart
rownames(ConMatrix) <- depart

for(i in 1:nrow(Connect)){
  dept.buy <- which(Connect[i,] == 1)
  for(j in 1:length(dept.buy)){
    for(k in 1:j){
      ConMatrix[dept.buy[k],dept.buy[j]] <- ConMatrix[dept.buy[k],dept.buy[j]] + 1
    }
  }
}

ConMatrix_1 <- t(ConMatrix)
diag(ConMatrix_1) <- 0

write.csv(ConMatrix_1, file = "relationshipMatrix_train.csv")


############ TEST ##############

data = read.csv("test.csv")


library(RSQLite)
con = dbConnect(RSQLite::SQLite(), ":memory:")
str(con)


dbWriteTable(con, "train", data)   # data frame -> database table
dbListTables(con)

# group by department
res_d = dbSendQuery(con,"SELECT DepartmentDescription, SUM(ScanCount) As NI, COUNT(*) AS n FROM train GROUP BY DepartmentDescription") 
number_d <- dbFetch(res_d, n=-1)
depart <- number_d$DepartmentDescription


# group by visitnumber
res_v = dbSendQuery(con,"SELECT VisitNumber, SUM(ScanCount) As NI, COUNT(*) AS n FROM train GROUP BY VisitNumber") 
number_v <- dbFetch(res_v, n=-1)
visitnumber <- number_v$VisitNumber


Condf <- data.frame(VisitNumber = visitnumber)


library(dplyr)
l <- list()
for(i in 1:length(depart)){
  sql_query <- paste0("SELECT VisitNumber, DepartmentDescription AS '",depart[i],"' FROM train WHERE DepartmentDescription = '", depart[i], "' GROUP BY VisitNumber")
  res =  dbSendQuery(con,sql_query) 
  l[[i]] <- dbFetch(res, n=-1)
  Condf <- left_join(Condf,l[[i]], by = "VisitNumber")
}

Connect <- Condf[,-1]
Connect[!is.na(Connect)] = 1

n= length(depart)
ConMatrix <- matrix(0, n, n)
colnames(ConMatrix) <- depart
rownames(ConMatrix) <- depart

for(i in 1:nrow(Connect)){
  dept.buy <- which(Connect[i,] == 1)
  for(j in 1:length(dept.buy)){
    for(k in 1:j){
      ConMatrix[dept.buy[k],dept.buy[j]] <- ConMatrix[dept.buy[k],dept.buy[j]] + 1
    }
  }
}

ConMatrix_1 <- t(ConMatrix)
diag(ConMatrix_1) <- 0

write.csv(ConMatrix_1, file = "relationshipMatrix_test.csv")
