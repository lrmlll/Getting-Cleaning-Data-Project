# Getting-Cleaning-Data-Project
Coursera Getting&amp;Cleaning Data Project 

library(dplyr)  
library(reshape2)

#unzip file
unzip("./datafile.zip")

#read label, features
labels<- read.table("UCI HAR Dataset/activity_labels.txt")  
features<- read.table("UCI HAR Dataset/features.txt")

#read data
subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt",col.names= "subjectnum")  
x_test<- read.table("UCI HAR Dataset/test/x_test.txt")  
y_test<- read.table("UCI HAR Dataset/test/y_test.txt")  

subject_train<- read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "subjectnum")  
x_train<- read.table("UCI HAR Dataset/train/x_train.txt") 
y_train<-  read.table("UCI HAR Dataset/train/y_train.txt")

#create column name, rename columns
featurelist<- features[,2]  
colnames(x_test)<-(featurelist) 
colnames(y_test)<- c("activity")

colnames(x_train)<- (featurelist) 
colnames(y_train)<- c("activity") 


#append activity name column to test/train data
temptest<- cbind(x_test,y_test,subject_test)  
temptrain<- cbind(x_train,y_train,subject_train)  

##merge test and train dataset
all<- rbind(temptest,temptrain)

##extract measurements on mean and standard deviation 

#get index and subset
meaninex<- grep("mean",features[,2])  
stdindex<- grep("std",features[,2]) 
index<- sort(c(meaninex,stdindex))  

cleanedall<- all[,c(index,562,563)]

#label dataset with activity name
cleanedall$activity[cleanedall$activity == 1] <- "WALKING"  
cleanedall$activity[cleanedall$activity == 2] <- "WALKING_UPSTAIRS" 
cleanedall$activity[cleanedall$activity == 3] <- "WALKING_DOWNSTAIRS" 
cleanedall$activity[cleanedall$activity == 4] <- "SITTING"  
cleanedall$activity[cleanedall$activity == 5] <- "STANDING" 
cleanedall$activity[cleanedall$activity == 6] <- "LAYING" 


##Create tidy dataset

#prepare the dataframe
df1<- arrange(cleanedall, subjectnum) 
df1$activity <- as.factor(df1$activity) 
df1$subjectnum <- as.integer(df1$subjectnum)  

#reshape dataframe
df1melt<- melt(df1,id=c("subjectnum","activity"),measure.vars = names(df1)[1:79]) 

#create tidy data
tidydata<- dcast(df1melt, activity + subjectnum ~ variable,mean)  

##output to txt file
write.table(tidydata,file = "tidy_data_sum.txt",row.name=FALSE)
