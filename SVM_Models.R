library(e1071)

# load data
load("newData_train_without_F.RData")
load("newData_test_without_F.RData")

# set input data for SVM model 
data <- newData_without_F
inputData <- data.frame(data[, -c(1:2)], response = as.factor(data[,"TripType"])) # response as factor

## linear SVM 
svmModel <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE)              
linear_compareTable <- table(inputData$response, predict(svmModel))  # tabulate
linear_misclassification = mean(inputData$response != predict(svmModel)) # 34.81% misclassification error

# Since there is one department shows up only in train set but not in test set, 
# we add the column and set its value to be 0 in test set 
outputData <- data.frame(testData_without_F[, -1])
latentDept <- which(names(inputData) %in% names(outputData) == FALSE)
latentDept <- latentDept[-length(latentDept)]
outputData <- cbind(outputData[,1:(latentDept-1)],0,outputData[,latentDept:ncol(outputData)])
names(outputData)[latentDept] <- names(inputData)[latentDept]

# predict the result in test set
pred <- predict(svmModel,outputData)
output <- cbind(testData_without_F[, 1], pred)
TripType <- as.character(unique(data[,"TripType"]))[order(unique(data[,"TripType"]))]

## set submission file to upload in Kaggle
submission <- matrix(0, nrow = nrow(output) , ncol = length(TripType))
submission <- cbind(output[,1], submission)
colnames(submission) <- rep(NA, length(TripType)+1)
colnames(submission)[1] <- "VisitNumber"
for(i in 1:length(TripType)){
  colnames(submission)[i+1] <- paste0("TripType_",TripType[i])
}

for(i in 1:nrow(output)){
  submission[i, which(TripType==output[i,2])+1] <- 1
}
class(submission) <- "integer"
write.csv(submission, file = "submission.csv", row.names = FALSE)


## radial SVM
svmModel_r <- svm(response ~ ., data = inputData, kernel = "radial", 
                cost = 10, scale = FALSE) # radial svm, scaling turned OFF
radial_compareTable <- table (inputData$response, predict(svmModel_r))  # tabulate
mean(inputData$response != predict(svmModel_r)) # 33.87% misclassification error

# predict the result in test set
pred_r <- predict(svmModel_r,outputData)
output_r <- cbind(testData_without_F[, 1], pred_r)

## set submission file to upload in Kaggle
submission_r <- matrix(0, nrow = nrow(output_r) , ncol = length(TripType))
submission_r <- cbind(output_r[,1], submission_r)
colnames(submission_r) <- rep(NA, length(TripType)+1)
colnames(submission_r)[1] <- "VisitNumber"
for(i in 1:length(TripType)){
  colnames(submission_r)[i+1] <- paste0("TripType_",TripType[i])
}

for(i in 1:nrow(output)){
  submission_r[i, which(TripType==output[i,2])+1] <- 1
}
class(submission_r) <- "integer"
write.csv(submission_r, file = "submission_r.csv", row.names = FALSE)

 
