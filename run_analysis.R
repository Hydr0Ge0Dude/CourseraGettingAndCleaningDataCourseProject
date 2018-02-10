################################################################################
#This script performs the following actions for the Course Project in
#Coursera.org's Getting and Cleaning Data course:
#
#0) Download and unzip required data
#
#1) Merge the training and the test sets to create one data set
#
#2) Extract only the measurements on the mean and standard deviation for each
#   measurement
#
#3) Use descriptive activity names to name the activities in the data set
#
#4) Appropriately label the data set with descriptive variable names
#
#5) Create a second, independent tidy data set from the data set in Step 4, with
#   the average of each variable for each activity and each subject
#
#Load required libraries and set working directory
library(dplyr); library(readr); library(reshape2)
#
#
#0) Download and unzip required data############################################
#Specify, download, and unzip compressed dataset if it doesn't already exist in
#current working directory
myzip <- "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!myzip %in% list.files()){
      myurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(myurl,myzip)
      unzip(myzip,overwrite = T)
      }
#
#
#1)Merge the training and the test sets to create one data set#################
#
#read training and test data sets
traindata <- read_table("./UCI HAR Dataset/train/X_train.txt",col_names = F)
testdata <- read_table("./UCI HAR Dataset/test/X_test.txt",col_names = F)
#
#read labels and bind to corresponding training and test data sets
trainlab <- read_table("./UCI HAR Dataset/train/y_train.txt",col_names = F)
testlab <- read_table("./UCI HAR Dataset/test/y_test.txt",col_names = F)
#
#read subject information for training and test data sets
trainsubj <- read_table("./UCI HAR Dataset/train/subject_train.txt",col_names = F)
testsubj <- read_table("./UCI HAR Dataset/test/subject_test.txt",col_names = F)
#
training <- bind_cols(trainsubj, trainlab, traindata)
testing <- bind_cols(testsubj, testlab, testdata)
#
#add column indicating type of data
training$Type <- "Training"
testing$Type <- "Testing"
#
#merge data into single data set
mergedata <- rbind(training,testing)
#
#read names of features and apply as column names to merged data
features <- read_table("./UCI HAR Dataset/features.txt",col_names = F)
features <- gsub("\\W|\\.","",features[[1]])
names(mergedata) <- c("Subject","Activity",features,"Type")
#
#
#2)Extract only the measurements on the mean and standard deviation for each measurement####
#
exstats <-
      mergedata %>%
      select(grep("subject|mean|std|activity|type",names(.),ignore.case = T))
#
#
#3)Use descriptive activity names to name the activities in the data set########
#
#read activity_labels for use as names for the data set activities in each row;
#set names of tibble to match other columns
actlabs <-
      read_table("./UCI HAR Dataset/activity_labels.txt",
                 col_names = c("Activity","Activity_Description"))
#
#join labels to data sets and drop Activity column
exstats <- left_join(actlabs,exstats,by="Activity") %>%
      select(-c(Activity))
#
#
#4)Appropriately label the data set with descriptive variable names#############
#
#replace column names with descriptive variable names based on pair of columns
#in "replist" data frame (created below)
replist <- data.frame(stringsAsFactors = F,
      c("^[0-9]","^[0-9]","^[0-9]","Jerk","Body","Acc","Gravity","gravity",
        "Gyro", "Mag","Freq","angle","^ "," $","  "," "),
      c("","",""," Jerk "," Body "," Acceleration "," Gravity "," Gravity ",
        " Gyroscope "," Magnitude "," Frequency "," Angle ","","","_","_"))
#
#replace text in names of dataset with descriptive variable names
for(i in (1:nrow(replist))){
      names(exstats) <-
            gsub(as.character(replist[i,1]),as.character(replist[i,2]),names(exstats))
}
#
#
#5)Create a second, independent tidy data set from the data set in Step 4, with
# the average of each variable for each activity and each subject####
# 
# drop unnecessary "Type" column, group by Subject and Activity, calculate mean
# values for numeric columns
exstats_tidy <- 
      exstats %>%
      select(-c(Type)) %>%
      group_by(Subject,Activity_Description) %>%
      summarize_if(is.numeric,mean)
#
#Write output table for submission to Coursera.org
write.table(exstats_tidy, "my_submission.txt", row.name=FALSE)
