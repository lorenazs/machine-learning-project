library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)

#load the data for training and the final 20 tests

 trData<- read.csv("data/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
 tstData <- read.csv("data/pml-testing.csv",na.strings=c("NA","#DIV/0!",""))
 
# transforming the data (both training and test), removing columns that are not necessary or relevant for the analysis
# these are the fist five columns: X, user_name,raw_timestamp_part1, raw_timestamp_part_2
# cvtd_timestamp and new_window

 dropColumns<-c('X', 'user_name','raw_timestamp_part_1', 'raw_timestamp_part_2','cvtd_timestamp','new_window')
 trData<- trData[,!(names(trData) %in% dropColumns)] 
 tstData<- tstData[,!(names(tstData) %in% dropColumns)]
 
# removing columns where the NAs percentaje is greater than 40%
 trData<-trData[,colSums(is.na(trData)) < nrow(trData) * 0.4]
 dim(trData)
 tstData<-tstData[,colSums(is.na(tstData)) < nrow(tstData) * 0.4]
 dim(tstData)

 
set.seed(1234)

#creating the training and test sets

 inTrain <- createDataPartition(y=trData$classe, p=0.7,list=FALSE)

 training <- trData[inTrain,]
 testing <- trData[-inTrain,]

 model <- train(training$classe ~ . , method='rpart',data=training)
 fancyRpartPlot(model$finalModel)
 
 predictionsDT <- predict(model,testing)
 
 confusionMatrix(predictionsDT,testing$classe)
 
 # using RandomForests
 
 #model2 <- train(training$classe ~. , data=training,method='rf',prox=TRUE)
 
 model2<-randomForest(classe ~ . ,data=training)
 
 # predictions with randomForest 
 predictionsRF <- predict(model2,testing)
 
 confusionMatrix(predictionsRF,testing$classe)

 
 # predicting the values for the 20 tests
 finalPredictionsRF <- predict(model2,tstData,type='class')

 # code provided to generate the files for submission
 pml_write_files <-function(x){
   n = length(x)
   for(i in 1:n)
   {
     filename = paste0("problem_id_",i,".txt")
     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names = FALSE)
   }
    
 } 
 
 # creating the files
 pml_write_files(finalPredictionsRF)
 