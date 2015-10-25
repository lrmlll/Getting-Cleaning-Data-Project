# Getting-Cleaning-Data-Project
Coursera Getting&amp;Cleaning Data Project 

library(dplyr)
library(reshape2)
##download data
##dataurl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download.file(url= dataurl,destfile = "datafile.zip")

##unzip file
unzip("./datafile.zip")

##Merges the training and the test sets to create one data set
##Extracts only the measurements on the mean and standard deviation for each measurement  (features)
##Uses descriptive activity names to name the activities in the data set    (labels)
##Appropriately labels the data set with descriptive variable names    features
##Creates a second, independent tidy data set with the average of each variable for each activity and each subject

##Labels:  activity
##x_test:   test set
##y_test:  test activity label
##Subject:   who took the test
##Features: variable name

##read label, features
labels<- read.table("UCI HAR Dataset/activity_labels.txt")
features<- read.table("UCI HAR Dataset/features.txt")


##read data
subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt",col.names= "subjectnum")
x_test<- read.table("UCI HAR Dataset/test/x_test.txt")
y_test<- read.table("UCI HAR Dataset/test/y_test.txt")

subject_train<- read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "subjectnum")
x_train<- read.table("UCI HAR Dataset/train/x_train.txt")
y_train<-  read.table("UCI HAR Dataset/train/y_train.txt")

##create column name
featurelist<- features[,2]

##rename dataframe columns
colnames(x_test)<-(featurelist)
colnames(y_test)<- c("activity")

colnames(x_train)<- (featurelist)
colnames(y_train)<- c("activity")


##append activity name column to test/train data
temptest<- cbind(x_test,y_test,subject_test)
temptrain<- cbind(x_train,y_train,subject_train)

##merge test and train dataset
all<- rbind(temptest,temptrain)

##extract measurements on mean and standard deviation
##get index for subset
meaninex<- grep("mean",features[,2])
stdindex<- grep("std",features[,2])
index<- sort(c(meaninex,stdindex))

##subset data
cleanedall<- all[,c(index,562,563)]

##label dataset with activity name
cleanedall$activity[cleanedall$activity == 1] <- "WALKING"
cleanedall$activity[cleanedall$activity == 2] <- "WALKING_UPSTAIRS"
cleanedall$activity[cleanedall$activity == 3] <- "WALKING_DOWNSTAIRS"
cleanedall$activity[cleanedall$activity == 4] <- "SITTING"
cleanedall$activity[cleanedall$activity == 5] <- "STANDING"
cleanedall$activity[cleanedall$activity == 6] <- "LAYING"


#Create tidy dataset

##prepare the dataframe
##rearrange dataframe
df1<- arrange(cleanedall, subjectnum)
##convert to factor vector
df1$activity <- as.factor(df1$activity)
##convert to int vector
df1$subjectnum <- as.integer(df1$subjectnum)

##reshape dataframe
## library(reshape2)
##  create meltdata 
df1melt<- melt(df1,id=c("subjectnum","activity"),measure.vars = names(df1)[1:79])

##create tidy data
tidydata<- dcast(df1melt, activity + subjectnum ~ variable,mean)

##output to txt file
write.table(tidydata,file = "tidy_data_sum.txt",row.name=FALSE)
