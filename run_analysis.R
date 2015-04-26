#------------------------------------------------------------------------------------
# Part 1:
#
# The purpose of step one is to merge the training and the test sets 
# to create one tidy data set, called "tidydata"
#
#------------------------------------------------------------------------------------

setwd("UCI HAR Dataset/")

#Creating all of the data frames needed before stitched together:
features <- read.table("features.txt")
testxtest <- read.table("test/X_test.txt")
testsubtest <- read.table("test/subject_test.txt")
testytest <- read.table("test/y_test.txt")

#and train data
trainytrain <- read.table("train//y_train.txt")
trainxtrain <- read.table("train/X_train.txt")
trainsubtrain <- read.table("train/subject_train.txt")

#put the training and test data together
testtrainsub <- rbind(testsubtest, trainsubtrain)

#get rid of old variables eating memory
rm(testsubtest)
rm(trainsubtrain)

#put together x data
testtrainx <- rbind(testxtest, trainxtrain)
rm(testxtest)
rm(trainxtrain)

#put together y data
testtrainy <- rbind(testytest, trainytrain)
rm(testytest)
rm(trainytrain)

#need to make features into a character vector so we can bind it
features <- as.character(features[,2])

#then label the testxtest data.table with the features labels
colnames(testtrainx) <- features
rm(features)

#now we need to add the testtrainy as the first column of our big data set
tidydata <- cbind(testtrainy, testtrainx)
rm(testtrainx)
rm(testtrainy)

#Add the test and training subject number to tidy data
tidydata <- cbind(testtrainsub, tidydata)
rm(testtrainsub)

#rename the first two columns
colnames(tidydata)[c(1,2)] <- c("Subject_ID_Number", "Activity_ID")

#------------------------------------------------------------------------------------
# Part 2:
#
# Now, I will extract only the mean and standard deviation for each measurement
# from the tidydata data frame.
#
#------------------------------------------------------------------------------------

# The select function in the dplyr package will not work with duplicated names. 
# So first, we can remove these rows:

tidydataMeanSTD <- tidydata[!duplicated(names(tidydata))]


# Select only columns with mean or std and also the activity and subject columns.
library(dplyr)
tidydataMeanSTD <- select(tidydataMeanSTD, contains("mean"), contains("std"), 
                           contains("Activity"), contains("Subject"))

#------------------------------------------------------------------------------------
# Part 3:
#
# Name the activities in the data set.
#
#------------------------------------------------------------------------------------

#First we need to extract the activity label key from the UCI folder
actlabels <- read.table("activity_labels.txt")
actlabels <- actlabels[,2]
actlabels <- as.character(actlabels)

# Now we can convert the integers in the Activity_ID column into factors and
# name those levels as the appropriate labels.
tidydataMeanSTD$Activity_ID <- factor(tidydataMeanSTD$Activity_ID,
                    levels = c(1,2,3,4,5,6),
                    labels = actlabels)

#------------------------------------------------------------------------------------
# Part 5:
#
# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#
#------------------------------------------------------------------------------------
tidyaverages <- apply(tidydataMeanSTD[,1:86], 2, mean)

tidyBySubject <- arrange(tidydataMeanSTD, Subject_ID_Number)


mdata <- melt(tidyBySubject, id.vars = c("Activity_ID", "Subject_ID_Number"))

tidyDataFinal <- dcast(mdata, Activity_ID + Subject_ID_Number ~ variable, mean)


write.table(tidyDataFinal, file = "TidyData.txt", row.name = FALSE)
