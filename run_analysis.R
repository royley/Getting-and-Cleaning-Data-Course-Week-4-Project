#Step 1 - Merge the Training and Test datasets to create one data set.
  # Download and unzip the dataset:
    fileName <- "getdata_dataset.zip"

    if (!file.exists(fileName)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, fileName)
    }  
    if (!file.exists("UCI HAR Dataset")) { 
    unzip(fileName) 
    }

  # Read in training files from unzipped folder
    subjecttrain = read.table('./train/subject_train.txt',header=FALSE);
    xtrain = read.table('./train/x_train.txt',header=FALSE);
    ytrain = read.table('./train/y_train.txt',header=FALSE);

  # Read in test files from unzipped folder
    subjecttest = read.table('./test/subject_test.txt',header=FALSE);
    xtest = read.table('./test/x_test.txt',header=FALSE);
    ytest = read.table('./test/y_test.txt',header=FALSE);
  
  # Read in remaining required files from unzipped folder
    features = read.table('./features.txt',header=FALSE);
    activitylabels = read.table('./activity_labels.txt',header=FALSE);
    
  # Assign column names to the imported data
    colnames(activitylabels) = c('activityid','activitytype');
    colnames(subjecttrain) = "subjectid";
    colnames(xtrain) = features[,2]; 
    colnames(yTrain) = "activityid";
    colnames(subjecttest) = "subjectid";
    colnames(xtest) = features[,2]; 
    colnames(ytest) = "activityid";

  # Merge ytrain, xtrain and subjecttrain to create combined training dataset
    traindata = cbind(ytrain,subjecttrain,xtrain);

  # Merge ytest, xtest and subjecttest to create the combined test dataset
    testdata = cbind(ytest,subjecttest,xtest);
    
  # Merge combined training and test datasets to create a final combined dataset
    combineddata = rbind(traindata,testdata);

#Step 2 - Extract only the measurements on the mean and standard deviation for each measurement.
  # Using combineddata, create a column name vector, used to select the mean and stddev columns
    colNames  = colnames(combineddata); 

  # Create a logical vector to identify the ID, mean and stddev columns
    log_vector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

  # Select required columns from combineddata table
    finaldata = combineddata[log_vector==TRUE];

#Step 3 - Use descriptive activity names to name the activities in the dataset

  # Merge the combineddata dataset with the acitivitytype table including descriptive activity names created earlier
    finaldata2 = merge(finalData,activityType,by='activityId',all.x=TRUE);
############################################
  # Updating the colNames vector to include the new column names after merge
    colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
