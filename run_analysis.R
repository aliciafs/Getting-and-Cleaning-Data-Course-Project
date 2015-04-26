
#R script called run_analysis.R that does the following:

#download raw data

getwd()
setwd("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data")

Url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data <- download.file (Url, destfile="Dataset.zip")



#Merges the training and the test sets to create one data set.

features <- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/features.txt", colClasses = c("character"))
activity_labels <- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityId", "Activity"))
test1<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/test/X_test.txt")
test2<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/test/y_test.txt")
test3<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/test/subject_test.txt")

library(plyr)

#dflist <- list(test1, test2, test3)
#test_a <- join_all(dflist)
#str(test_a)

test_set <- cbind (test1, test2, test3)
str(test_set)

testa<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/train/X_train.txt")
testb<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/train/y_train.txt")
testc<- read.table("C:/Users/Alicia/Desktop/Ali/Coursera/Getting&cleaning data/Dataset/UCI HAR Dataset/train/subject_train.txt")

train_set <- cbind (testa, testb, testc)
str(train_set)

set <- rbind(test_set, train_set)
str(set)

# Label columns

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(set) <- sensor_labels

#Extracts only the measurements on the mean and standard deviation for each measurement.
# Search for matches to argument mean or standard deviation (sd)  within each element of character vector

set_mean_std <- set[,grepl("mean|std|Subject|ActivityId", names(set))]
str(set_mean_std)

#Uses descriptive activity names to name the activities in the data set

library(plyr)
set_mean_std <- join(set_mean_std, activity_labels, by = "ActivityId", match = "first")
set_mean_std <- set_mean_std[,-1]

#Appropriately labels the data set with descriptive names.

# Remove parentheses
names(set_mean_std) <- gsub('\\(|\\)',"",names(set_mean_std), perl = TRUE)

# Make syntactically valid names

names(set_mean_std) <- make.names(names(set_mean_std))

# Make clearer names
names(set_mean_std) <- gsub('Acc',"Acceleration",names(set_mean_std))
names(set_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(set_mean_std))
names(set_mean_std) <- gsub('Gyro',"AngularSpeed",names(set_mean_std))
names(set_mean_std) <- gsub('Mag',"Magnitude",names(set_mean_std))
names(set_mean_std) <- gsub('^t',"TimeDomain.",names(set_mean_std))
names(set_mean_std) <- gsub('^f',"FrequencyDomain.",names(set_mean_std))
names(set_mean_std) <- gsub('\\.mean',".Mean",names(set_mean_std))
names(set_mean_std) <- gsub('\\.std',".StandardDeviation",names(set_mean_std))
names(set_mean_std) <- gsub('Freq\\.',"Frequency.",names(set_mean_std))
names(set_mean_std) <- gsub('Freq$',"Frequency",names(set_mean_std))


# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


set_final <- ddply(set_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(set_final, file = "set_final.txt")
