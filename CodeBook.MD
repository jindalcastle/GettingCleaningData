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
