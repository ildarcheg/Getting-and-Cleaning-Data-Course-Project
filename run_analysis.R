# STEP 0 Read and prepare the data
# Clear memory
rm(list = ls())
# Load packages
packages <- c("dplyr")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
# Read general data
features <- read.table("./features.txt",header=FALSE)
activity <-  read.table("./activity_labels.txt",header=FALSE) 
# Add names for columns
colnames(activity) <- c('activityId','activityName')
# Read train data
subjectTrain <-  read.table("./train/subject_train.txt",header=FALSE)
xTrain <-  read.table("./train/x_train.txt",header=FALSE) 
yTrain <-  read.table("./train/y_train.txt",header=FALSE)
# Add names for columns
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2] 
colnames(yTrain) <- "activityId"
# Reat test data
subjectTest <-  read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
xTest <-  read.table("./test/x_test.txt",header=FALSE) 
yTest <-  read.table("./test/y_test.txt",header=FALSE)
# Add names for columns
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"

# STEP 1 Merges the training and the test sets to create one data set.
# Combine train data
train <- cbind(yTrain,subjectTrain,xTrain)
# Combine test data
test <- cbind(yTest,subjectTest,xTest)
# Combine data
df <- rbind(train,test)

# STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement.
selectedNames <- as.character(features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)])
selectedNames <- c("subjectId", "activityId", selectedNames)
tidyData <- subset(df, select = selectedNames)

# STEP 3 Uses descriptive activity names to name the activities in the data set
tidyData$activityId <- factor(tidyData$activityId, levels = activity$activityId, labels = activity$activityName)
tidyData$subjectId <- factor(tidyData$subjectId)

# STEP 4 Appropriately labels the data set with descriptive variable names. 
names(tidyData) <- gsub("^t", "time", names(tidyData))
names(tidyData) <- gsub("^f", "frequency", names(tidyData))
names(tidyData) <- gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData) <- gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData) <- gsub("Mag", "Magnitude", names(tidyData))
names(tidyData) <- gsub("BodyBody", "Body", names(tidyData))
names(tidyData) <- gsub("-mean()-", "Mean", names(tidyData))
names(tidyData) <- gsub("-std()-", "Std", names(tidyData))

# STEP 5 From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and each subject.
tidyDataFinal <- aggregate(tidyData[, names(tidyData)[-c(1, 2)]], by = list(activityId = tidyData$activityId, subjectId = tidyData$subjectId), mean)
# Saving data set as a txt file for uploading
write.table(tidyDataFinal, "./tidyData.txt", row.names = TRUE)
