library(e1071)
load("newData_train_without_F.RData")
load("newData_test_without_F.RData")
# triptype <- unique(Dept_data[,"TripType"])

#data <- as.matrix(newData_without_F[1:1000,])
data <- newData_without_F
#class(data) <- "numeric"
inputData <- data.frame(data[, -c(1:2)], response = as.factor(data[,"TripType"])) # response as factor

# linear SVM
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", 
              cost = 10, scale = FALSE) # linear svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table(inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 19.44% misclassification error

# Since one department shows up only in train set but not in test set, 
# we add the column and set the number in test set as 0
outputData <- data.frame(testData_without_F[, -1])
latentDept <- which(names(inputData) %in% names(outputData) == FALSE)
latentDept <- latentDept[-length(latentDept)]
outputData <- cbind(outputData[,1:(latentDept-1)],0,outputData[,latentDept:ncol(outputData)])
names(outputData)[latentDept] <- names(inputData)[latentDept]

pred <- predict(svmfit,outputData)

output <- cbind(testData_without_F[, 1], pred)
TripType <- as.character(unique(data[,"TripType"]))[order(unique(data[,"TripType"]))]


# initial submission
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
write.csv(submission, file = "submission.csv")




# radial SVM
svmfit <- svm(response ~ ., data = inputData, kernel = "radial", 
              cost = 10, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 18.75% misclassification error


### Tuning
# Prepare training and test data
set.seed(100) # for reproducing results
rowIndices <- 1:nrow(inputData) # prepare row indices
sampleSize <- round(0.8 * length(rowIndices)) # training sample size
trainingRows <- sample(1:nrow(inputData), sampleSize) # random sampling 
trainingData <- inputData[trainingRows,] # training data
testData <- inputData[-trainingRows, ] # test data
tuned <- tune.svm(response ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune
summary (tuned) # to select best gamma and cost


svmfit <- svm (response ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.001, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, trainingData)
compareTable <- table (testData$response, predict(svmfit, testData))  # comparison table
mean(testData$response != predict(svmfit, testData)) # 13.79% misclassifica‐ tion error
