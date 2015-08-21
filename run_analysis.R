# This script solves the project for Getting and Cleaning Data Course
# Date: 7/26/2015
#
# Project Statement:
# The purpose of this project is to demonstrate your ability to collect, work 
# with, and clean a data set. The goal is to prepare tidy data that can be used 
# for later analysis. You will be graded by your peers on a series of yes/no 
# questions related to the project. You will be required to submit: 
#     1) a tidy data set as described below, 
#     2) a link to a Github repository with your script for performing the analysis, and 
#     3) a code book that describes the variables, the data, and any transformations 
#        or work that you performed to clean up the data called CodeBook.md. 
# 
# You should also include a README.md in the repo with your scripts. This repo 
# explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable computing 
# - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are 
# racing to develop the most advanced algorithms to attract new users. The data linked 
# to from the course website represent data collected from the accelerometers from 
# the Samsung Galaxy S smartphone. A full description is available at the site where 
# the data was obtained: 
#     
#     http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#     
#     https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
# each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set. 
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

# Solution Script:

# Set Working Directory
setwd("~/Coursera/GettingAndCleaningData/Project")

# Downloading the data
fileName <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileName,"UCIHARData.zip")

# Extacting the data from the zip file was done using gunzip

# Loading features and activity labels
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Loading the training data sets
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")

# labeling the training data frames

names(x_train) <- features$V2
names(y_train) <- "Activity"
names(subject_train) <- "SubjectID"

# Now we combine the training data frames

TrainingSet <- cbind(subject_train,y_train,x_train)

# Loading the testing data sets
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")

# labeling the testing data frames

names(x_test) <- features$V2
names(y_test) <- "Activity"
names(subject_test) <- "SubjectID"

# Now we combine the testing data frames

TestingSet <- cbind(subject_test,y_test,x_test)

# 1. Merges the training and the test sets to create one data set.

TrainingAndTestingSet <- rbind(TrainingSet,TestingSet)

# 2. Extracts only the measurements on the mean and standard deviation for 
# each measurement.

# Note that the "+2" occurs because the features data frame does not include the
# two columns added to the data set to make it tidy.

meanIndex <- grep("mean",as.character(features$V2)) +2
STDIndex <- grep("std",as.character(features$V2)) +2

# We included every type of mean and standard deviation, including in the 
# Fourier Domain

meanAndSTDIndex <- sort(c(meanIndex,STDIndex))
meanAndSTDData <- cbind(TrainingAndTestingSet[,c(1,2)],
                        TrainingAndTestingSet[,meanAndSTDIndex])

# 3. Uses descriptive activity names to name the activities in the data set.

# We also change it into a factor
meanAndSTDData$Activity <- factor(meanAndSTDData$Activity,
                                         labels=as.character(activity_labels$V2))
meanAndSTDData$SubjectID <- factor(meanAndSTDData$SubjectID)


# 4. Appropriately labels the data set with descriptive variable names.

# We make all the variable names lower case
names(meanAndSTDData) <- tolower(names(meanAndSTDData))

# We remove all of the unnecessary () in the variable names
names(meanAndSTDData) <- gsub("\\()","",names(meanAndSTDData))

# We remove all of the unnecessary commas in the variable names
names(meanAndSTDData) <- gsub("\\,","",names(meanAndSTDData))

# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

# I will use simple aggregation.  I had difficulties using the dplyr package.

attach(meanAndSTDData)
aggdata <-aggregate(meanAndSTDData, list(subjectid,activity), 
                    FUN=mean, na.rm=TRUE)

# This method aggregated the data but created two extra columns associated with 
# the activity and ID.  Averaging the factors resulted in NAs (and Warnings).
# We create a tidy data set removing the two columns of NAs and appropriately
# labeling the first two columns.
# Note to Self: Figure out a cleaner way of doing this.

tidyMeanAndSTData <- cbind(aggdata[,c(1,2)],aggdata[,seq(5,83,by=1)])
names(tidyMeanAndSTData)[1] <- "subjectid"
names(tidyMeanAndSTData)[2] <- "activity"

# We save the tidy data set
write.table(tidyMeanAndSTData, "tidyMeanAndSTData.txt", row.name=FALSE)

# For the code book, we export the variable names
# write.table(names(tidyMeanAndSTData), "variableNames.txt", sep="\t", quote=FALSE)