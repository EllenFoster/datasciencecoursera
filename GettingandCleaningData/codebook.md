#Coursera Getting and Cleaning Data Course Project
##Project Description

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. [A full description is available here](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

##Course assignment
Create one R script called run_analysis.R that does the following:
...1. Merges the training and the test sets to create one data set.
...2. Extracts only the measurements on the mean and standard deviation for each measurement. 
...3. Uses descriptive activity names to name the activities in the data set
...4. Appropriately labels the data set with descriptive variable names. 
...5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Variable Names and Descriptions
###Note: FFT = Fast Fourier Transform
...*ID:	Participant ID, numbered 1-30
...*tBodyAcc_mean_X: Time Body Acceleration Signal, Mean, X-Axis
...*tBodyAcc_mean_Y: Time Body Acceleration Signal, Mean, Y-Axis
...*tBodyAcc_mean_Z: Time Body Acceleration Signal, Mean, Z-Axis
...*tBodyAcc_std_X: Time Body Acceleration Signal, St. Deviation, X-Axis
...*tBodyAcc_std_Y: Time Body Acceleration Signal, St. Deviation, Y-Axis
...*tBodyAcc_std_Z: Time Body Acceleration Signal, St. Deviation, Z-Axis
...*tGravityAcc_mean_X: Time Gravity Acceleration Signal, Mean, X-Axis
...*tGravityAcc_mean_Y: Time Gravity Acceleration Signal, Mean, Y-Axis
...*tGravityAcc_mean_Z: Time Gravity Acceleration Signal, Mean, Z-Axis
...*tGravityAcc_std_X: Time Gravity Acceleration Signal, St. Deviation, X-Axis
...*tGravityAcc_std_Y: Time Gravity Acceleration Signal, St. Deviation, Y-Axis
...*tGravityAcc_std_Z: Time Gravity Acceleration Signal, St. Deviation, Z-Axis
...*tBodyAccJerk_mean_X: Time Body Linear Acceleration Jerk Signal, Mean, X-Axis
...*tBodyAccJerk_mean_Y: Time Body Linear Acceleration Jerk Signal, Mean, Y-Axis
...*tBodyAccJerk_mean_Z: Time Body Linear Acceleration Jerk Signal, Mean, Z-Axis
...*tBodyAccJerk_mean_X: Time Body Linear Acceleration Jerk Signal, St. Deviation, X-Axis
...*tBodyAccJerk_mean_Y: Time Body Linear Acceleration Jerk Signal, St. Deviation, Y-Axis
...*tBodyAccJerk_mean_Z: Time Body Linear Acceleration Jerk Signal, St. Deviation, Z-Axis
...*tBodyGyro_mean_X: Time Body Gyroscope Signal, Mean, X-Axis
...*tBodyGyro_mean_Y: Time Body Gyroscope Signal, Mean, Y-Axis
...*tBodyGyro_mean_Z: Time Body Gyroscope Signal, Mean, Z-Axis
...*tBodyGyro_mean_X: Time Body Gyroscope Signal, St. Deviation, X-Axis
...*tBodyGyro_mean_Y: Time Body Gyroscope Signal, St. Deviation, Y-Axis
...*tBodyGyro_mean_Z: Time Body Gyroscope Signal, St. Deviation, Z-Axis
...*tBodyGyroJerk_mean_X: Time Body Angular Velocity Jerk Signal, Mean, X-Axis
...*tBodyGyroJerk_mean_Y: Time Body Angular Velocity Jerk Signal, Mean, Y-Axis
...*tBodyGyroJerk_mean_Z: Time Body Angular Velocity Jerk Signal, Mean, Z-Axis
...*tBodyGyroJerk_mean_X: Time Body Angular Velocity Jerk Signal, St. Deviation, X-Axis
...*tBodyGyroJerk_mean_Y: Time Body Angular Velocity Jerk Signal, St. Deviation, Y-Axis
...*tBodyGyroJerk_mean_Z: Time Body Angular Velocity Jerk Signal, St. Deviation, Z-Axis
...*tBodyAccMag_mean: Time Body Acceleration Magnitude, Mean
...*tBodyAccMag_std: Time Body Acceleration Magnitude, St. Deviation
...*tGravityAccMag_mean: Time Gravity Acceleration Magnitude, Mean
...*tGravityAccMag_std: Time Gravity Acceleration Magnitude, St. Deviation
...*tBodyAccJerkMag_mean: Time Body Linear Acceleration Jerk Signal Magnitude, Mean
...*tBodyAccJerkMag_std: Time Body Linear Acceleration Jerk Signal Magnitude, St. Deviation
...*tBodyGyroMag_mean: Time Body Gyroscope Magnitude, Mean
...*tBodyGyroMag_std: Time Body Gyroscope Magnitude, St. Deviation
...*tBodyGyroJerkMag_mean: Time Body Angular Velocity Jerk Signal Magnitude, Mean
...*tBodyGyroJerkMag_std: Time Body Angular Velocity Jerk Signal Magnitude, St. Deviation
...*fBodyAcc_mean_X: FFT of Time Body Acceleration Signal, Mean, X-Axis
...*fBodyAcc_mean_Y: FFT of Time Body Acceleration Signal, Mean, Y-Axis
...*fBodyAcc_mean_Z: FFT of Time Body Acceleration Signal, Mean, Z-Axis
...*fBodyAcc_std_X: FFT of Time Body Acceleration Signal, St. Deviation, X-Axis
...*fBodyAcc_std_Y: FFT of Time Body Acceleration Signal, St. Deviation, Y-Axis
...*fBodyAcc_std_Z: FFT of Time Body Acceleration Signal, St. Deviation, Z-Axis
...*fBodyAccJerk_mean_X: FFT of Time Body Linear Acceleration Jerk Signal, Mean, X-Axis
...*fBodyAccJerk_mean_Y: FFT of Time Body Linear Acceleration Jerk Signal, Mean, Y-Axis
...*fBodyAccJerk_mean_Z: FFT of Time Body Linear Acceleration Jerk Signal, Mean, Z-Axis
...*fBodyAccJerk_mean_X: FFT of Time Body Linear Acceleration Jerk Signal, St. Deviation, X-Axis
...*fBodyAccJerk_mean_Y: FFT of Time Body Linear Acceleration Jerk Signal, St. Deviation, Y-Axis
...*fBodyAccJerk_mean_Z: FFT of Time Body Linear Acceleration Jerk Signal, St. Deviation, Z-Axis
...*fBodyGyro_mean_X: FFT of Time Body Gyroscope Signal, Mean, X-Axis
...*fBodyGyro_mean_Y: FFT of Time Body Gyroscope Signal, Mean, Y-Axis
...*fBodyGyro_mean_Z: FFT of Time Body Gyroscope Signal, Mean, Z-Axis
...*fBodyGyro_mean_X: FFT of Time Body Gyroscope Signal, St. Deviation, X-Axis
...*fBodyGyro_mean_Y: FFT of Time Body Gyroscope Signal, St. Deviation, Y-Axis
...*fBodyGyro_mean_Z: FFT of Time Body Gyroscope Signal, St. Deviation, Z-Axis
...*fBodyAccMag_mean: FFT of Time Body Acceleration Magnitude, Mean
...*fBodyAccMag_std: FFT of Time Body Acceleration Magnitude, St. Deviation
...*fBodyBodyAccJerkMag_mean: FFT of Time Body Linear Acceleration Jerk Signal Magnitude, Mean
...*fBodyBodyAccJerkMag_std: FFT of Time Body Linear Acceleration Jerk Signal Magnitude, St. Deviation
...*fBodyGyroMag_mean: FFT of Time Body Gyroscope Magnitude, Mean
...*fBodyGyroMag_std: FFT of Time Body Gyroscope Magnitude, St. Deviation
...*fBodyBodyGyroJerkMag_mean: FFT of Time Body Angular Velocity Jerk Signal Magnitude, Mean
...*fBodyBodyGyroJerkMag_std: FFT of Time Body Angular Velocity Jerk Signal Magnitude, St. Deviation
...*Activity_Level: Records activity being done: Walking, Walking upstairs, Walking Downstairs, Sitting, Standing, Laying Down