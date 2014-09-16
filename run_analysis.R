library(plyr)

## Read  test data set
test_X <- read.table("UCI HAR Dataset/test/X_test.txt")
test_Y <- read.table("UCI HAR Dataset/test/Y_test.txt")
test_Subject<- read.table("UCI HAR Dataset/test/subject_test.txt")

## Read train data set
train_X <- read.table("UCI HAR Dataset/train/X_train.txt")
train_Y<- read.table("UCI HAR Dataset/train/Y_train.txt")
train_Subject<- read.table("UCI HAR Dataset/train/subject_train.txt") 

## Merge test and train data sets to create one data set
X_Data <- rbind(test_X,train_X)
Y_Data <- rbind(test_Y,train_Y)
subject_Data <- rbind(test_Subject,train_Subject)

## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Read feature labels 
features <- read.table("UCI HAR Dataset/features.txt")
## Create a filter of mean and std 
filter_cols <- subset(features,  grepl("(mean\\(\\)|std\\(\\))", features$V2) )
## Subet X data by the filter  
X_Data <- X_Data[,filter_cols$V1]


## Uses descriptive activity names to name the activities in the data set
## Read feature labels 
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
Y_Data <- join(Y_Data, activity_labels, by="V1")

## Combine subject, activity and measurements 
tidy <- cbind(subject_Data, Y_Data, X_Data)

## Replace activity value with names
## Appropriately labels the data set with descriptive activity names. 
## Set column names 
names(tidy) <- c("subject","activity","activityName", sapply(filter_cols[,2],function(x) gsub(pattern="\\(|\\)|-",replacement="",x)))


##  Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy$subject <- factor(tidy$subject)
tidy$activity <- factor(tidy$activity)
tidy$activityName <- factor(tidy$activityName)

## Average by subject and activity 
tidy_Average <- aggregate(tidy[,4:69], by=list(tidy$subject, tidy$activity, tidy$activityName), FUN=mean)
names(tidy_Average) <- c("subject","activity","activityName", sapply(filter_cols[,2],function(x) gsub(pattern="\\(|\\)|-",replacement="",x)))
tidy_Average <- tidy_Average[order(tidy_Average$subject,tidy_Average$activity),]

## Write to output file 
write.table(tidy_Average, "tidy.txt", row.name=FALSE,sep="\t")
