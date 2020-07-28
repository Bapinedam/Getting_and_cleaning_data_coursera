### Getting and Cleaning Data Course Project ### 

## You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.

# The first step is set the working directory

setwd("C:/Users/Usuario/Desktop/Data science/John Hopkins University Course/3. Getting and Cleanning Data/Proyecto Final")

# Now we need to call the datasets

library(data.table)

features <- read.table("./UCI HAR Dataset/features.txt")
features <- as.character(features$V2)

activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabels <- as.character(activityLabels[, 2])

dataTrainX <-read.table("./UCI HAR Dataset/train/X_train.txt", header=F)
dataTrainY<-read.table("./UCI HAR Dataset/train/y_train.txt", header=F)
dataTrainSubject<-read.table("./UCI HAR Dataset/train/subject_train.txt", header=F)
dataTestX<-read.table("./UCI HAR Dataset/test/X_test.txt", header=F)
dataTestY<-read.table("./UCI HAR Dataset/test/y_test.txt", header=F)
dataTestSubject<-read.table("./UCI HAR Dataset/test/subject_test.txt", header=F)

# Now we need merge the latest data sets in one

train <- data.frame(dataTrainSubject, dataTrainY, dataTrainX)
test <- data.frame(dataTestSubject, dataTestY, dataTestX)

# Now we correctly rename the columns name of new data frames using the 
# "features" data set we have previously imported

names(train)<-c(c("subject", "activity"), features)
names(test)<-c(c("subject", "activity"), features)

# Finally we need to merge train data and test data in one data named "data1"

data1 <- rbind(train, test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

library(dplyr)

data2 <- data1[ , which(colnames(data1) %in% c("subject", "activity", grep("mean()|std()", colnames(data1), value = TRUE)))]

## 3. Utiliza nombres descriptivos de actividades para nombrar las actividades en el conjunto de datos

activity <- data2$activity
activity <- gsub(1, activityLabels[1], activity)
activity <- gsub(2, activityLabels[2], activity)
activity <- gsub(3, activityLabels[3], activity)
activity <- gsub(4, activityLabels[4], activity)
activity <- gsub(5, activityLabels[5], activity)
activity <- gsub(6, activityLabels[6], activity)
unique(activity)
data3 <- data2
data3$activity <- activity ## In this, is better create other data frame to compare in the case of an error

## 4. Etiqueta adecuadamente el conjunto de datos con nombres de variables descriptivas.

unique(gsub("-(mean|std)().*", "", names(data3)[-c(1:2)])) ## Here we can know which are thats names that we should to re label

names(data3)[-c(1:2)]<-gsub("^t", "time", names(data3)[-c(1:2)])
names(data3)[-c(1:2)]<-gsub("^f", "frequency", names(data3)[-c(1:2)])
names(data3)[-c(1:2)]<-gsub("Acc", "Accelerometer", names(data3)[-c(1:2)])
names(data3)[-c(1:2)]<-gsub("Gyro", "Gyroscope", names(data3)[-c(1:2)])
names(data3)[-c(1:2)]<-gsub("Mag", "Magnitude", names(data3)[-c(1:2)])
names(data3)[-c(1:2)]<-gsub("BodyBody", "Body", names(data3)[-c(1:2)])

names(data3)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.

data4 <- data3

data4 <- aggregate(. ~ subject + activity, data4, mean)

data4 <- data4[order(data4$subject,data4$activity),]

finalData <- data4
str(finalData)