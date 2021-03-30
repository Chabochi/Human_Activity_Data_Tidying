#You should create one R script called run_analysis.R that does the following. 
library(dplyr)
library(Hmisc)

#1.Merges the training and the test sets to create one data set.

#Test data tidyng and joining
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")

testData <- cbind(test_subject,test_labels, test_set)

#Training data tidyng and joining
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_set <- read.table("UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt")

trainData <- cbind(train_subject,train_labels, train_set)

#Joining data from test and train
jointData <- rbind(testData, trainData)

#4.Appropriately labels the data set with descriptive variable names.

#Selecting descriptive names for variable
features <- t(read.table("UCI HAR Dataset/features.txt")[2])
names(jointData) <- c("Subject", "Activity", features)


#2,Extracts only the measurements on the mean and standard deviation for 
#each measurement. 

#Obtaining colnames of std and mean variables
std_mean_Names <- unique(grep(paste(c("mean", "std"),collapse="|"), colnames(jointData), value=TRUE))
jointData <- jointData[c(1:304, 347:563)]

filterData <- jointData[,c("Subject", "Activity",std_mean_Names)]


#3.Uses descriptive activity names to name the activities in the data set

filterData <- mutate(filterData, Activity = replace(Activity, Activity == '1', "WALK"))
filterData <- mutate(filterData, Activity = replace(Activity, Activity == '2', "WALKUP"))
filterData <- mutate(filterData, Activity = replace(Activity, Activity == '3', "WALKDOWN"))
filterData <- mutate(filterData, Activity = replace(Activity, Activity == '4', "SIT"))
filterData <- mutate(filterData, Activity = replace(Activity, Activity == '5', "STAND"))
filterData <- mutate(filterData, Activity = replace(Activity, Activity == '6', "LAY"))

 
#5.From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

#Obtaining mean values for activities

var_Walk <- filter(filterData, Activity == "WALK")
Walk <- c()
for (i in 3:81){
  Walk <- append(Walk,mean(var_Walk[,i]))
}

Var_Walkup <- filter(filterData, Activity == "WALKUP")
Walkup <- c()
for (i in 3:81){
  Walkup <- append(Walkup,mean(Var_Walkup[,i]))
}

Var_Walkdown <- filter(filterData, Activity == "WALKDOWN")
Walkdown <- c()
for (i in 3:81){
  Walkdown <- append(Walkdown,mean(Var_Walkdown[,i]))
}

Var_Sit <- filter(filterData, Activity == "SIT")
Sit <- c()
for (i in 3:81){
  Sit <- append(Sit,mean(Var_Sit[,i]))
}

Var_Stand <- filter(filterData, Activity == "STAND")
Stand <- c()
for (i in 3:81){
  Stand <- append(Stand,mean(Var_Stand[,i]))
}

Var_Lay <- filter(filterData, Activity == "LAY")
Lay <- c()
for (i in 3:81){
  Lay <- append(Lay,mean(Var_Lay[,i]))
}

#Creating dataframe with mean values of activities
avgActTable <- data.frame()
avgActTable <- rbind(Walk, Walkup, Walkdown, Sit, Stand, Lay)
MeanVar <- paste("Average", std_mean_Names, sep = "")
colnames(avgActTable) <- MeanVar


#Obtaining mean values from Subject variables
subjects <- paste("Subject", 1:30, sep = "")
avgSubjectTable <- data.frame(matrix(NA, nrow = 30, ncol = 79), row.names = subjects)
colnames(avgSubjectTable) <- MeanVar


for (i in 1:30){
  Var_Act <- filter(filterData, Subject == i)
  avgSubject <- c()
  for (j in 3:81){
    avgSubject <- append(avgSubject,mean(Var_Act[,j]))
  } 
  avgSubjectTable[i,]   <- rbind(avgSubject)
}

#Joining mean values of subjects and activities into tidy dataset
TidyData <- data.frame(matrix(NA, nrow = 36, ncol = 79))
TidyData <- rbind(avgActTable, avgSubjectTable)



