#Set working directory before beginning the script
setwd("H:\\Coursework\\Coursera-DataScience\\08_PracticalMachineLearning")

if(!file.exists("./CourseProject")){
        dir.create("./CourseProject")
}

fileUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrl,destfile="./CourseProject/pml-training.csv")
#MAC users must add [method="curl"] to the download.file() arguments list

fileUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrl,destfile="./CourseProject/pml-testing.csv")
#MAC users must add [method="curl"] to the download.file() arguments list

training <- read.csv("./CourseProject/pml-training.csv")
testing <- read.csv("./CourseProject/pml-testing.csv")

train.log<-grepl("max_|min_|var_|kurtosis_|skewness_|amplitude_|avg_|stddev_",
                 colnames(training))
train.sub<-training[,!train.log]
train.sub<-train.sub[,-c(1:7)]

library(caret); library(randomForest)

#Data splitting
inTrain<-createDataPartition(y=train.sub$classe,p=0.60,list=FALSE)
training2<-train.sub[inTrain,]
testing2<-train.sub[-inTrain,]

#Fit a model
modFit<-train(classe~.,data=training2,method="rf", 
            trControl=trainControl(method="cv",number=3),ntree=50)

#Prediction
predictions<-predict(modFit,newdata=testing2)

#Confusion Matrix
confusionMatrix(predictions,testing2$classe)


#Apply prediction model to the test set
test.log<-grepl("max_|min_|var_|kurtosis_|skewness_|amplitude_|avg_|stddev_",
                 colnames(testing))
test.sub<-testing[,!train.log]
test.sub<-test.sub[,-c(1:7)]

predictions<-predict(modFit,newdata=test.sub)
predictions
