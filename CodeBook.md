## Getting and Cleaning Data Project

### Source Data
Human Activity Recognition Using Smartphones Data Set. Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors. A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project: 
        https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
Data Set Characteristics:  Multivariate, Time-Series
Number of Instances: 10299
Area: Computer
Attribute Characteristics: N/A
Number of Attributes: 561
Date Donated: 2012-12-10
Associated Tasks: Classification, Clustering
Missing Values: N/A
Number of Web Hits: 224110

For each record in the dataset it is provided: 
        - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Step 0 
Reads and prepares the data from sources in working directory:
- ./features.txt
- ./activity_labels.txt
- ./train/subject_train.txt
- ./train/x_train.txt
- ./train/y_train.txt
- ./test/subject_test.txt
- ./test/x_test.txt
- ./test/y_test.txt

Features, activity, subjectTrain, xTrain, yTrain, subjectTest, xTest, yTest - contain the data from the data set and will be used for analysis.

### STEP 1 
Merges the training and the test sets to create one data set.
Combine the data from step 0 to one data frame named df.

### STEP 2 
Extracts only the measurements on the mean and standard deviation for each measurement and keeps result in 'tidyData'

### STEP 3 
Uses descriptive activity names to name the activities in the data set 'tidyData'. Sets 'activityId' as a factor.

### STEP 4 
Appropriately labels the data set with descriptive variable names in 'tidyData'
For example '^t' to 'time', 'Gyro' to 'Gyroscope'.

### STEP 5 
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject, keeps in 'tidyDataFinal' and saved in './tidyData.txt'
