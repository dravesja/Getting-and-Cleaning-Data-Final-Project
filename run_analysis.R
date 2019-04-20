##### Getting and Cleaning Data - Final Project #####

######## Loading necessary libraries         ########
library(dplyr)

######## Downloading and naming files        ########

fname <- "G&CData_Final_Project,zip"  # file name is fname

# Downloading and checking to see if file exists

if (!file.exists(fname)){
  fURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fURL, fname, method="curl") #file URL is fURL
}  

# Checking folder 
if (!file.exists("UCI HAR Dataset")) { 
  unzip(fname) 
}

#######        ORGANIZING DATA                  ########
### 1. Examine files placed in UCI HAR Dataset folder ###
### 2. Begin with README file.  All pertient files    ###
###    appear to be  .txt files divides as "Test" and ###
###    tain files.                                    ###
### 3. Examine features.info file for information     ###
###    about all variables in the features.txt file   ###
### 4. Assign names to those matching files           ###

feature <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","variable"))
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

### Each row of subject_test containts the subject id ###
### Test data is 30% of data                          ###
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = feature$variable)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

### Each row of subject_test containts the subject id  ###
### Training data is 70% of data                       ###
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = feature$variable)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

##########################################################
#                                                        #
#               ASSIGNMENT BEGINS HERE                   #
#                                                        #
##########################################################

# Step 1: Merger the traning and the test sets to    ###
# create one data set.                               ###

Xbind <- rbind(X_train, X_test)      #row binding test data
ybind <- rbind(y_train, y_test)      #row binding labels
Subid <- rbind(subject_train, subject_test) #row binding id's

### Merged file is called Working_Data and is constructed ###
### by column binding by subject id as follows:           ###
Working_Data <- cbind(Subid, ybind, Xbind)

### Step 2: Extract only the mean and the                     ###
### standard deviation for each measurement                   ###

### File containing means and std's is called Summary_Data
Summary_Data <- Working_Data %>% select(subject, code, contains("mean"), contains("std"))


### Step 3: Use desciptive activitiy names to name the        ###
### activities in the data set                                ###

Summary_Data$code <- activity[Summary_Data$code, 2]

### Step 4: Appropriatley label the data set with desciptive  ###
### names                                                     ###

names(Summary_Data)[2] = "activity"
names(Summary_Data)<-gsub("Acc", "Accelerometer", names(Summary_Data))
names(Summary_Data)<-gsub("Gyro", "Gyroscope", names(Summary_Data))
names(Summary_Data)<-gsub("BodyBody", "Body", names(Summary_Data))
names(Summary_Data)<-gsub("Mag", "Magnitude", names(Summary_Data))
names(Summary_Data)<-gsub("^t", "Time", names(Summary_Data))
names(Summary_Data)<-gsub("^f", "Frequency", names(Summary_Data))
names(Summary_Data)<-gsub("tBody", "TimeBody", names(Summary_Data))
names(Summary_Data)<-gsub("-mean()", "Mean", names(Summary_Data), ignore.case = TRUE)
names(Summary_Data)<-gsub("-std()", "STD", names(Summary_Data), ignore.case = TRUE)
names(Summary_Data)<-gsub("-freq()", "Frequency", names(Summary_Data), ignore.case = TRUE)
names(Summary_Data)<-gsub("angle", "Angle", names(Summary_Data))
names(Summary_Data)<-gsub("gravity", "Gravity", names(Summary_Data))

### From the data set in Step 4, create a second, independent ###
### tidy data set with the average of each variable for each  ###
### activity and each subject                                 ###

### Second data set is called TidyData                        ###
TidyData <- Summary_Data %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(TidyData, "TidyData.txt", row.name=FALSE)
