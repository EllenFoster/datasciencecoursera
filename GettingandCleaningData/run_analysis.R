#Coursera - Getting and Cleaning Data - Course project
#Date: 7/25/15
#Programmer: E. Foster
#Purpose: Create a tidy dataset that combines, relabels and computes summary stats on the data files located here:
#       https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#Set the directory where the files are located
mainDir <- "C:\\Users\\ellen.foster\\Documents\\Analytic Projects\\Writeups and Examples\\Getting and Cleaning Data"
subDir <- "UCI HAR Dataset"

dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

library(dplyr)

#Step 1 - Merge all files to create 1 dataset
#Read in all test data files
testdat <- read.table("./test/X_test.txt")
       testdat <- rename(testdat, tBodyAcc_mean_X=V1)
       
testlab <- read.table("./test/y_test.txt")
       testlab <- rename(testlab, Activity=V1)
       
testsub <- read.table("./test/subject_test.txt")
       testsub <- rename(testsub, ID=V1)
       
test<- cbind(testsub,testdat,testlab)

#Read in all training data files
traindat <- read.table("./train/X_train.txt")
       traindat <- rename(traindat, tBodyAcc_mean_X=V1)
       
trainlab <- read.table("./train/y_train.txt")
       trainlab <- rename(trainlab, Activity=V1)
       
trainsub <- read.table("./train/subject_train.txt")
       trainsub <- rename(trainsub, ID=V1)
       
train<- cbind(trainsub,traindat,trainlab)

#Combine training and test datasets
combo <- rbind(train,test)

#Step 2 - Extract only mean & std dev measurements
smalldata <- select(combo, ID:V6, V41:V46, V81:V86, V121:V126, V161:V166, V201:V202,V214:V215,
                      V227:V228,V240:V241,V253:V254,V266:V271,V345:V350,V424:V429,V503:V504,V516:V517,
                      V529:V530,V542:V543,Activity)

#Step 3 - Use descriptive names to name the activities in the data set
smalldata$Activity <- factor(smalldata$Activity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Walk", "Walk.Upstairs", "Walk.Downstairs", "Sit","Stand", "LayDown"))

#Step 4 - Relabel variables with descriptive names
smalldata <- rename(smalldata, tBodyAcc_mean_X=tBodyAcc_mean_X,
                      tBodyAcc_mean_Y=V2,
                      tBodyAcc_mean_Z=V3,
                      tBodyAcc_std_X=V4,
                      tBodyAcc_std_Y=V5,
                      tBodyAcc_std_Z=V6,
                      tGravityAcc_mean_X=V41,
                      tGravityAcc_mean_Y=V42,
                      tGravityAcc_mean_Z=V43,
                      tGravityAcc_std_X=V44,
                      tGravityAcc_std_Y=V45,
                      tGravityAcc_std_Z=V46,
                      tBodyAccJerk_mean_X=V81,
                      tBodyAccJerk_mean_Y=V82,
                      tBodyAccJerk_mean_Z=V83,
                      tBodyAccJerk_std_X=V84,
                      tBodyAccJerk_std_Y=V85,
                      tBodyAccJerk_std_Z=V86,
                      tBodyGyro_mean_X=V121,
                      tBodyGyro_mean_Y=V122,
                      tBodyGyro_mean_Z=V123,
                      tBodyGyro_std_X=V124,
                      tBodyGyro_std_Y=V125,
                      tBodyGyro_std_Z=V126,
                      tBodyGyroJerk_mean_X=V161,
                      tBodyGyroJerk_mean_Y=V162,
                      tBodyGyroJerk_mean_Z=V163,
                      tBodyGyroJerk_std_X=V164,
                      tBodyGyroJerk_std_Y=V165,
                      tBodyGyroJerk_std_Z=V166,
                      tBodyAccMag_mean=V201,
                      tBodyAccMag_std=V202,
                      tGravityAccMag_mean=V214,
                      tGravityAccMag_std=V215,
                      tBodyAccJerkMag_mean=V227,
                      tBodyAccJerkMag_std=V228,
                      tBodyGyroMag_mean=V240,
                      tBodyGyroMag_std=V241,
                      tBodyGyroJerkMag_mean=V253,
                      tBodyGyroJerkMag_std=V254,
                      fBodyAcc_mean_X=V266,
                      fBodyAcc_mean_Y=V267,
                      fBodyAcc_mean_Z=V268,
                      fBodyAcc_std_X=V269,
                      fBodyAcc_std_Y=V270,
                      fBodyAcc_std_Z=V271,
                      fBodyAccJerk_mean_X=V345,
                      fBodyAccJerk_mean_Y=V346,
                      fBodyAccJerk_mean_Z=V347,
                      fBodyAccJerk_std_X=V348,
                      fBodyAccJerk_std_Y=V349,
                      fBodyAccJerk_std_Z=V350,
                      fBodyGyro_mean_X=V424,
                      fBodyGyro_mean_Y=V425,
                      fBodyGyro_mean_Z=V426,
                      fBodyGyro_std_X=V427,
                      fBodyGyro_std_Y=V428,
                      fBodyGyro_std_Z=V429,
                      fBodyAccMag_mean=V503,
                      fBodyAccMag_std=V504,
                      fBodyBodyAccJerkMag_mean=V516,
                      fBodyBodyAccJerkMag_std=V517,
                      fBodyBodyGyroMag_mean=V529,
                      fBodyBodyGyroMag_std=V530,
                      fBodyBodyGyroJerkMag_mean=V542,
                      fBodyBodyGyroJerkMag_std=V543,
                      Activity_Level=Activity)

#Step 5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject.
#Remove ID and activity level for aggregation
smalldata2 <- smalldata[,2:67]

#Average all variables over ID and activity level
tidy<-aggregate(smalldata2, by = smalldata[c("Activity_Level","ID")], mean, na.rm = TRUE)

#Create the final dataset
write.table(tidy, file ="./Tidy_Data.txt", row.name=FALSE)