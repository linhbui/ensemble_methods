# Part 1
rm(list=ls())
install.packages("gbm")
data <- read.table(file='winequality-red.csv',sep=';',header=TRUE)
require('gbm')

model <- gbm(data$quality~.,
		distribution="gaussian",
		data=data, 
		n.trees = 3000,
		interaction.depth=1,
		cv.folds=10,
		shrinkage=0.01)

gbm.perf(model,method="cv") #[1] 2620 the best number of trees
min(model$cv.error)         #[1] 0.4019845

# Part 2
# Ridge on wine data
rm(list=ls())
install.packages('ridge')
library(ridge)
data <- read.csv('winequality-red.csv', header=TRUE, sep = ';')
xTrain <- data[1:1400,1:11]
yTrain <- data[1:1400,12]
model <- linearRidge(yTrain ~.,data=as.data.frame(xTrain))
xTest <- data[1401:1499,1:11]
yTest <- data[1401:1499,12]
yTestEst <- predict(model,xTest)
errorTest <- sqrt((sum((yTest-yTestEst)^2))/length(yTest))
errorTest #[1] 0.7962109

# Random Forest on wine data
install.packages('randomForest')
library(randomForest)
rm(list=ls())
data <- read.csv('winequality-red.csv', header=TRUE, sep = ';')
xTrain <- data[1:1400,1:11]
yTrain <- data[1:1400,12]
model<-randomForest(xTrain,yTrain)
xTest <- data[1401:1499,1:11]
yTest <- data[1401:1499,12]
yTestEst <- predict(model,xTest)
errorTest <- sqrt((sum((yTest-yTestEst)^2))/length(yTest))
errorTest

# Part 3
#a
rm(list=ls())
train <- read.table("sonar_train.csv",sep = ",",header = FALSE)
test <- read.table("sonar_test.csv",sep = ",",header = FALSE)
sonar <- rbind(train,test)

trainErr <- 0.0
testErr <- 0.0
I <- seq(from = 1, to = nrow(sonar))
for(ixval in seq(from = 1, to = 5)){
  Iout <- which(I%%5 == ixval%%5)
  SonarIn <- sonar[-Iout,]
  SonarOut <- sonar[Iout,]
  
  lmSonar <- lm(V61~., data = SonarIn)
  sonarFit <- SonarIn[,-61]
  lmFit <- predict(lmSonar, newdata = sonarFit)
  
  correct <- sum(lmFit*SonarIn$V61 >0)
  trainErr <- trainErr + (1-correct/length(SonarIn$V61))/5
  
  sonarFit <- SonarOut[,-61]
  lmFit <- predict(lmSonar, newdata = sonarFit)
  
  correct <- sum(lmFit*SonarOut$V61 >0)
  testErr <- testErr + (1-correct/length(SonarOut$V61))/5
}
trainErr                   # [1] 0.04087007
testErr                    # [1] 0.1275261

#b reduce the number of observations gradually
trainErrorList <- c(trainErr)
testErrorList <- c(testErr)
nrow <- nrow(sonar)
ncol <- ncol(sonar)
dataSetSize <- c(nrow)

subtractionFactor <- 10
while ((nrow - subtractionFactor) > (ncol + 20)) {
  nrow <- nrow - subtractionFactor
  dataSetSize <- c(dataSetSize, nrow)
  data <- sonar[1:nrow,]
  
  trainError <- 0.0
  testError <- 0.0
  I <- seq(from = 1, to = nrow(data))
  for(ixval in seq(from = 1, to = 5)){
    Iout <- which(I%%5 == ixval%%5)
    SonarIn <- data[-Iout,]
    SonarOut <- data[Iout,]
    
    lmSonar <- lm(V61~., data = SonarIn)
    sonarFit <- SonarIn[,-61]
    lmFit <- predict(lmSonar, newdata = sonarFit)
    
    correct <- sum(lmFit*SonarIn$V61 >0)
    trainError <- trainError + (1-correct/length(SonarIn$V61))/5
    
    sonarFit <- SonarOut[,-61]
    lmFit <- predict(lmSonar, newdata = sonarFit)
    
    correct <- sum(lmFit*SonarOut$V61 >0)
    testError <- testError + (1-correct/length(SonarOut$V61))/5
  }
  trainErrorList <- c(trainErrorList, trainError)
  testErrorList <- c(testErrorList, testError)
}
trainErrorList
testErrorList
# c plot
plot(dataSetSize, trainErrorList,col='black', main='Training Error & Test Error vs. Data Set Size')
par(new=TRUE)
plot(dataSetSize, testErrorList, col='red')
legend('bottomright', c('Train Error', 'Test Error'), col=c('black', 'red'),bty='n',pch=16)
lines(trainErrorList~dataSetSize,lwd=2)
par(new=TRUE)
plot(trainErrorList~dataSetSize,ann=FALSE,type='n')
lines(trainErrorList~dataSetSize,lwd=2)
par(new=TRUE)
plot(testErrorList~dataSetSize,ann=FALSE,type='n')
lines(testErrorList~dataSetSize,lwd=2,col='red')

# Part 4
# a
rm(list=ls())
train <- read.table("sonar_train.csv",sep = ",",header = FALSE)
test <- read.table("sonar_test.csv",sep = ",",header = FALSE)
sonar <- rbind(train,test)
nrow <- 88 # most overfitted model from 3
createEnsemble <- function(n, nrow, data) {
  predictedValuesTrain <- NULL
  predictedValuesTest <- NULL
  for (i in seq(from=1, to=10)) {
    randomCols = sample((ncol(data)-1), n)
    randomCols <- c(randomCols, ncol(data))
    randomData <- data[randomCols] 
    
    dataIn <- randomData[1:nrow,]
    dataOut <- randomData[-nrow,]
    
    lm <- lm(V61~., data=dataIn)
    dataFit <- dataIn[,-(n+1)]
    lmFit <- predict(lm, newdata = dataFit)
    matrix <- matrix(lmFit,nrow=nrow,ncol=1)
    predictedValuesTrain <- cbind(predictedValuesTrain, matrix)
    
    dataFit <- dataOut[,-(n+1)]
    lmFit <- predict(lm, newdata = dataFit)
    matrix <- matrix(lmFit,nrow=nrow(dataOut),ncol=1)
    predictedValuesTest <- cbind(predictedValuesTest, matrix)
  }
  
  predictedValuesTrain <- cbind(predictedValuesTrain,dataIn[,(n+1)])
  predictedValuesTest <- cbind(predictedValuesTest,dataOut[,(n+1)])
  
  ensembleDataTrain <- as.data.frame(predictedValuesTrain)
  ensembleDataTest <- as.data.frame(predictedValuesTest)
  
  ensembledModel <- lm(V11~., data=ensembleDataTrain)
  
  ensembleFitTrain <- predict(ensembledModel, newdata=ensembleDataTrain[,-11])
  correctTrain <- sum(ensembleFitTrain*ensembleDataTrain$V11 >0)
  trainError <- (1-correctTrain/length(ensembleDataTrain$V11))
  
  ensembleFitTest <- predict(ensembledModel, newdata=ensembleDataTest[,-11])
  correctTest <- sum(ensembleFitTest*ensembleDataTest$V11 >0)
  testError <- (1-correctTest/length(ensembleDataTest$V11))
  
  returnResult <- matrix(c(n,trainError,testError),nrow=3,ncol=1)
  return(returnResult)
}

model <- createEnsemble(11, 88, sonar)
