##########################################################################################################
# Coursera Getting and Cleaning Data Course Project 2014-10-26
# runAnalysis.r File Description:
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##########################################################################################################


# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset
setwd('D:/R/Projects/coursera_ds03_project/UCI HAR Dataset');

# Read in the data from files
feature_set     <- read.table('./features.txt',header=FALSE); #imports features.txt
activityType <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTraining <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTraining       <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTraining       <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assign column names to the data imported above
colnames(activityType)  <- c('activityId','activityType');
colnames(subjectTraining)  <- "subjectId";
colnames(xTraining)        <- feature_set[,2]; 
colnames(yTraining)        <- "activityId";

# cCreate the final training set by merging yTraining, subjectTraining, and xTraining
trainingData <- cbind(yTraining,subjectTraining,xTraining);

# Read in the test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) <- "subjectId";
colnames(xTest)       <- feature_set[,2]; 
colnames(yTest)       <- "activityId";


# Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  <- colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData_selected <- finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData_selected_merged <- merge(finalData_selected,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  <- colnames(finalData_selected_merged); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])
        colNames[i] <- gsub("-std$","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","time",colNames[i])
        colNames[i] <- gsub("^(f)","freq",colNames[i])
        colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData_selected_merged) <- colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  <- finalData_selected_merged[,names(finalData_selected_merged) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData_merged    <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
#write.table(tidyData_merged, './tidyData.txt',sep='|');
write.table(tidyData_merged, './tidyData_wo_names.txt',row.names=FALSE,sep='|');