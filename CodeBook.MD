
### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


### Variables Used

activityType <- imports activity_labels.txt

subjectTrain <- imports subject_train.txt  

features <- imports features.txt

xTrain <- imports x_train.txt

yTrain <- Imports y_train file

trainingData <- contains the training data

subjectTest <- reading the subject files and assigning the column name

xTest <- Reading the test x_test file and use above feature file to assing the second column value to column name

yTest <- reading the ytest file and assinging the column name to the value

testData <- combining the activity id, subject id and the features from the test files

finalData <- Merging the test and training data using the row bind and assigning that to variable finaldata

selectedcolnames <- selecting only the required columns of mean and standard deviation

NoActivityData <- getting columns except activitytype

finaltidydata <- variable to store the final data

colNames <- Storing the column names
