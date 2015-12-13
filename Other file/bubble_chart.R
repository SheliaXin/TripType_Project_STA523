# setwd("/Users/sheliaxin/Documents/Duke/Courses/STA523/Final Proj/Final_Project_STA523")
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

##### Department matrix ######
Dept_data <- Condf
Dept_tem <- Condf[,-1]
Dept_tem[!is.na(Connect)] = 1
Dept_tem[is.na(Connect)] = 0
Dept_data <- cbind(Condf[,1],Dept_tem)
save(Dept_data,file="Dept_matrix.RData")
########

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



######### DEBUG  ###########
ConMatrix_1 <-as.matrix(read.csv("relationshipMatrix_train.csv"))
rownames(ConMatrix_1) = ConMatrix_1[, "X"]
ConMatrix_1 = ConMatrix_1[, -1]
class(ConMatrix_1) <- "numeric" 
colnames(ConMatrix_1) = rownames(ConMatrix_1)
#############################



# install package
doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2", "reshape2", "RColorBrewer")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)


library(plyr)


### INPUT  ###
depart_select <- c(1,2,3,4)

longData <- melt(ConMatrix_1[depart_select,depart_select])
head(longData, 20)

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

Top20 <- head(longData[order(longData$value,decreasing = TRUE),],n=20)
Top20




dbClearResult(res)
dbClearResult(res_d)
dbClearResult(res_v)
dbDisconnect(con)  # Closing the connection





