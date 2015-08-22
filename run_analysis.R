setwd("~/UCI HAR Dataset")

# read the files from the location cantaining features, activity labels and subject train and x_train and y_train

# reading the activity label and assigning the new names to the columns
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
colnames(activityType)  = c('activityId','activityType');

#reading the subject train file and assigning the column name to the dataset
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
colnames(subjectTrain)  = "subjectId";

#reading the xTrain and feature file -- the second column and assigning that value to the column name
features     = read.table('./features.txt',header=FALSE); #imports features.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
colnames(xTrain)        = features[,2];

#reading the YTrain File and assinging the column name to the dataset
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
colnames(yTrain)        = "activityId";



# binding the columns of imported activity id, subject id and the other features that has been imported
trainingData = cbind(yTrain,subjectTrain,xTrain);

#Same steps as above for test data to read the files and assign the column names to the data sets

#reading the subject files and assigning the column name
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
colnames(subjectTest) = "subjectId";

#reading the test x_test file and use above feature file to assing the second column value to column name
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
colnames(xTest)       = features[,2];

#reading the ytest file and assinging the column name to the value
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt
colnames(yTest)       = "activityId";


# combining the activity id, subject id and the features from the test files
testData = cbind(yTest,subjectTest,xTest);


# Mergin the test and training data using the row bind and assigning that to variable finaldata
finalData = rbind(trainingData,testData);




# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData);

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

#function to select the names of the columns which are required
grepMeanStd <- function(x,y) names(y[grep(x, names(y))])

# Create a columns list that contains ID, mean() & stddev() columns
selectedcolnames = c(grepMeanStd("mean",finalData) , grepMeanStd("activity",finalData) , grepMeanStd("subject",finalData) , grepMeanStd("std",finalData));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[selectedcolnames];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData);

# 4. Appropriately label the data set with descriptive activity names.

# renaming the valirables
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, NoActivityData without the activityType column
NoActivityData  = finalData[,names(finalData) != 'activityType'];

# Summarizing the NoActivityData table to include just the mean of each variable for each activity and each subject
finaltidydata    = aggregate(NoActivityData[,names(NoActivityData) != c('activityId','subjectId')],by=list(activityId=NoActivityData$activityId,subjectId = NoActivityData$subjectId),mean);

# Merging the finaltidydata with activityType to include descriptive acitvity names
finaltidydata    = merge(finaltidydata,activityType,by='activityId',all.x=TRUE);

# Export the finaltidydata set
write.table(finaltidydata, './finaltidydata.txt',row.names=TRUE,sep='\t');

#End of the script