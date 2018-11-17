## Load required packages
library(dplyr)
library(data.table)
library(tidyr)

## Downloading and Unzipping Data

#download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

fileName <- "UCIdata.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dirFile <- "UCI HAR Dataset"

# verification of file download 
if(!file.exists(fileName)){
    download.file(fileURL, fileName, mode = "wb") 
}

# verification of unzipped file.
if(!file.exists(dirFile)){
    unzip("UCIdata.zip", files = NULL, exdir=".")
}

# Read Training Data

trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_X <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
train_Y <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Read Test Data

testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
test_Y <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)


activityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)  

alldataSubject <- rbind(trainSubject, testSubject)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(train_Y, test_Y)
setnames(alldataActivity, "V1", "activityNum")

#  Merge training and test data

data <- rbind(train_X, test_X)

#  Extracts only the measurements on the mean and standard deviation for each measurement.
setnames(features, names(features), c("featureNum", "featureName"))
colnames(data) <- features$featureName

#column names for activity labels
 
setnames(activityLabel, names(activityLabel), c("activityNum","activityName"))

# Merge columns
dataSubject<- cbind(alldataSubject, alldataActivity)
data <- cbind(dataSubject, data)

# Reading "features.txt" and extracting only the mean and standard deviation
MeanStd <- grep("mean\\(\\)|std\\(\\)", features$featureName,value=TRUE) #var name
MeanStd <- union(c("subject","activityNum"), MeanStd)
data<- subset(data,select = MeanStd) 

##enter name of activity into data
data <- merge(activityLabel, data , by="activityNum", all.x=TRUE)
data$activityName <- as.character(data$activityName)

## create data with variable means sorted by subject and Activity
data$activityName <- as.character(data$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = data, mean) 
data<- tbl_df(arrange(dataAggr,subject,activityName))

 
names(data)<-gsub("std()", "SD", names(data))
names(data)<-gsub("mean()", "MEAN", names(data))
names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

 

##write to text file on disk

write.table(data, "tidy_data.txt", row.name=FALSE)
