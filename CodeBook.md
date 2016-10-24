Getting and Cleaning Data Course Week 4 Project

Dataset Description:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Data Description:
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) – both using a low pass Butterworth filter.
The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).

Description of abbreviations of measurements

leading t or f is based on time or frequency measurements.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = accelerometer measurement
Gyro = gyroscopic measurements
Jerk = sudden movement acceleration
Mag = magnitude of movement
mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

tBodyAcc-XYZ
  *amended to TimeBodyAcc plus Mean or StdDev
tGravityAcc-XYZ
  *amended to TimeGravityAcc plus Mean or StdDev
tBodyAccJerk-XYZ
  *amended to TimeBodyAccJerk plus Mean or StdDev
tBodyGyro-XYZ
  *amended to TimeBodyGyro plus Mean or StdDev
tBodyGyroJerk-XYZ
  *amended to TimeBodyGyroJerk plus Mean or StdDev
tBodyAccMag
  *amended to TimeBodyAccMagnitude plus Mean or StdDev
tGravityAccMag
  *amended to TimeGravityAccMagnitude plus Mean or StdDev
tBodyAccJerkMag
  *amended to TimeBodyAccJerkMagnitude plus Mean or StdDev
tBodyGyroMag
  *amended to TimeBodyGyroMagnitude plus Mean or StdDev
tBodyGyroJerkMag
  *amended to TimeBodyGyroJerkMagnitude plus Mean or StdDev
fBodyAcc-XYZ
  *amended to FreqBodyAcc plus Mean or StdDev
fBodyAccJerk-XYZ
  *amended to FreqBodyAccJerk plus Mean or StdDev
fBodyGyro-XYZ
  *amended to FreqBodyGyro plus Mean or StdDev
fBodyAccMag
  *amended to FreqBodyAccMagnitude plus Mean or StdDev
fBodyAccJerkMag
  *amended to FreqBodyAccJerkMagnitude plus Mean or StdDev
fBodyGyroMag
  *amended to FreqBodyGyroMagnitude plus Mean or StdDev
fBodyGyroJerkMag
  *amended to FreqBodyGyroJerkMagnitude plus Mean or StdDev

The set of variables that were estimated from these signals are:
mean(): Mean value
std(): Standard deviation
