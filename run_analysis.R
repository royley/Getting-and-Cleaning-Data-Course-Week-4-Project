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
    subjecttrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);
    xtrain = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE);
    ytrain = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);

  # Read in test files from unzipped folder
    subjecttest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE);
    xtest = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE);
    ytest = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE);
  
  # Read in remaining required files from unzipped folder
    features = read.table('./UCI HAR Dataset/features.txt',header=FALSE);
    activitylabels = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);
    
  # Assign column names to the imported data
    colnames(activitylabels) = c('activityid','activitytype');
    colnames(subjecttrain) = "subjectid";
    colnames(xtrain) = features[,2]; 
    colnames(ytrain) = "activityid";
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
    columnnames  = colnames(combineddata); 

  # Create a logical vector to identify the ID, mean and stddev columns
    log_vector = (grepl("activity..",columnnames) | grepl("subject..",columnnames) | grepl("-mean..",columnnames) & !grepl("-meanFreq..",columnnames) & !grepl("mean..-",columnnames) | grepl("-std..",columnnames) & !grepl("-std()..-",columnnames));

  # Select required columns from combineddata table
    finaldata = combineddata[log_vector==TRUE];

#Step 3 - Use descriptive activity names to name the activities in the dataset
  # Merge the combineddata dataset with the acitivitylabels table including descriptive activity names created earlier
    finaldata2 = merge(finalData,activitylabels,by='activityid',all.x=TRUE);

# 4. Appropriately label the data set with descriptive activity names. 
  # Update the colNames vector to include the new column names after merge
    columnnames  = colnames(finaldata2); 

  # Cleaning up the variable names
    for (i in 1:length(columnnames)) {
    columnnames[i] = gsub("\\()","",columnnames[i])
    columnnames[i] = gsub("-std$","StdDev",columnnames[i])
    columnnames[i] = gsub("-mean","Mean",columnnames[i])
    columnnames[i] = gsub("^(t)","Time",columnnames[i])
    columnnames[i] = gsub("^(f)","Freq",columnnames[i])
    columnnames[i] = gsub("([Gg]ravity)","Gravity",columnnames[i])
    columnnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnnames[i])
    columnnames[i] = gsub("[Gg]yro","Gyro",columnnmes[i])
    columnnames[i] = gsub("AccMag","AccMagnitude",columnnames[i])
    columnnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columnnames[i])
    columnnames[i] = gsub("JerkMag","JerkMagnitude",columnnames[i])
    columnnames[i] = gsub("GyroMag","GyroMagnitude",columnnames[i])
    };

    # Assign the descriptive column names to the finaldata2 dataset
    colnames(finaldata2) = columnnames;

#Step 5 - Using the dataset created in Step 4, create a second, tidy dataset showing the average of each variable for each activity and each subject
    #Build new dataset
    tidydataset <- aggregate(. ~subjectid + activityid, finaldata2, mean)
    tidydataset2 <- tidydataset[order(tidydataset$subjectid, tidydataset$activityid),]

    # Export tidydataset2 
    write.table(tidydataset2, './UCI HAR Dataset/tidyData.txt',row.names=TRUE,sep='\t');
