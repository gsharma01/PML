library(caret, randomForest)

# load data
trainRawData <- read.csv("pml-training.csv",na.strings=c("NA",""))
# discard NAs
NAs <- apply(trainRawData,2,function(x) {sum(is.na(x))}) 
validData <- trainRawData[,which(NAs == 0)]
# make training set
trainIndex <- createDataPartition(y = validData$classe, p=0.2,list=FALSE) # 3927 rows
trainData <- validData[trainIndex,]
# discards unuseful predictors
removeIndex <- grep("timestamp|X|user_name|new_window",names(trainData))
trainData <- trainData[,-removeIndex]
training<-trainData

#Cross Validation and Out of Sample Error

k=10
foldSize <- floor(nrow(training)/k)

errVect<- rep(NA,k)

for (i in 1:k)
{
		set1<- ((i-1)*foldSize+1)
		set2<- (i*foldSize)

		subSet <- set1:set2

		cvTrain = training[-subSet,]
		cvTest = training[subSet,]
		
		cvModFit <- train(cvTrain[,-1], y = as.factor(cvTrain[,1]) ,data = cvTrain,method="rf")
		cvPredict<- predict(cvModFit,newData = cvTest[,1],type="prob")[,2] 

		errVect[i]<- roc.area(cvTest[,1],cvPredict)$A
		
		print(paste("Area under ROC for fold", i, ":", errVect[i]))
		
}

print(paste("Average Area Under ROC:", mean(errVect))



# make model
modFit <- train(classe ~.,data = training,method="rf")
modFit