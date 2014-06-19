# Predictive Machine Learning Model on Activity Data


## Synopisis

Following Model has been created as part on course project of Practical Machine Learning class. Below I will go through the various steps to explain the process.This model expects files to be in current working directories.

## Preprocessing and Data Cleaning

Data cleaning and Preprocessing involved.
1. Remove Columns with NAs.
2. Get rid of extra columns which is not required during model building


```r
library(caret, randomForest)

# load data
trainRawData <- read.csv("pml-training.csv",na.strings=c("NA",""))
NAs <- apply(trainRawData,2,function(x) {sum(is.na(x))}) 
validData <- trainRawData[,which(NAs == 0)]
trainData <- trainData[,-removeIndex]
training<-trainData
```

## Cross Validation and Out of Sample error

During model building I tried various approches when cross validating the data. Best approach was random forest and through K fold. sampling technique. K-fold sampling was applied after taking the 10 K fold samples and then out of sample error was calculated using the Area under ROC . Following code explains the process.



```r
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
		
		cvModFit <- train(cvTrain[,-1], y = as.factor(cvTrain[,1]) ,data = cvTrain,method="rf") #Random forest on Train
		cvPredict<- predict(cvModFit,newData = cvTest[,1],type="prob")[,2] 

		errVect[i]<- roc.area(cvTest[,1],cvPredict)$A #Area under ROC Vector
		
		print(paste("Area under ROC for fold", i, ":", errVect[i])) #Printing each out of sample error 
		
}

print(paste("Average Area Under ROC:", mean(errVect)) #Mean of sample errors
```

## Model Building 

After Cross Validation and Sample error calculation. Final model was build was random forest approach with 95.5% of accuracy. Follwing code explains it.


```r
# make model
modFit <- train(classe ~.,data = training,method="rf")
modFit
```

##Model Prediction accuracy 


```r
Random Forest 

3927 samples
  53 predictors
   5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Bootstrapped (25 reps) 

Summary of sample sizes: 3927, 3927, 3927, 3927, 3927, 3927, ... 

Resampling results across tuning parameters:

  mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
  2     0.964     0.954  0.00655      0.00828 
  27    0.973     0.966  0.00491      0.00618 
  53    0.959     0.948  0.00873      0.011  
```

## Predicting and Applying Model to testing data set

Here is code which was being to used to predict the activities using the test data.


```r
 testing<- read.csv("pml-testing.csv")
 modPredict<- predict(modFit,testing)
 modPredict
```

## Model Prediction output


```r
  [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
```

After Prediction 20 files were generated using the following function for 20 different cases.


```r
pml_write_files = function(x){
     n = length(x)
     for(i in 1:n){
         filename = paste0("problem_id_",i,".txt")
         write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
     }
 }

 pml_write_files(modPredict)
```


