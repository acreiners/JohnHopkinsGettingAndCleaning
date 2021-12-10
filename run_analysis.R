#Launch Libraries
library(dplyr)
library(data.table)

#Get Data Set
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- getwd()
download.file(URL, "dataFiles.zip")
unzip("dataFiles.zip")

#Read Activity Labels 
ActivityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Read in features list and clean it up
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("index", "featuresNames"))
features_list <- simplify2array(features)
features_list_clean = subset(features_list, select = -c(index))

#Read in train data and add features list to columns
train_set <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features_list_clean)
train_sub <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "Subject ID")
train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt", col.name = "Activites ID")

#Read in test data and add features list to column
test_set <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features_list_clean)
test_sub <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "Subject ID")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt", col.name = "Activites ID")

#Combine 3 Training Tables to 1 table and 3 Test tables to 1 table
train_comb <- cbind(train_sub, train_labels, train_set)
test_comb <- cbind(test_sub, test_labels, test_set)

#Combine Training and Test Tables
training_test_combined <- rbind(train_comb, test_comb)

#Remove all columns that are not Subject ID, Activity ID, all mean and std data
col_keep <- c("ID|mean|std")
merged_tables_clean <- merged_tables[, grep(col_keep, colnames(merged_tables))]
merged_tables_clean <- merged_tables_clean[, -grep("Freq", colnames(merged_tables_clean))]

#Rename Activities from number to action
merged_tables_clean$Labels.ID <- factor(merged_tables_clean$Labels.ID, labels = ActivityLabels$V2)
colnames(merged_tables_clean)[colnames(merged_tables_clean) == "Labels.ID"] = "Activity"

#Rename Columns Headers
names(merged_tables_clean) <- gsub("tBody","TimeDomainBody", names(merged_tables_clean), fixed = TRUE)
names(merged_tables_clean) <- gsub("tGravity","TimeDomainGravitiy", names(merged_tables_clean), fixed = TRUE)
names(merged_tables_clean) <- gsub("fBody","FastFourierTransformBody", names(merged_tables_clean), fixed = TRUE)
names(merged_tables_clean) <- gsub("Acc","Acceleration", names(merged_tables_clean), fixed = TRUE)
names(merged_tables_clean) <- gsub("Gyro","Gyroscope", names(merged_tables_clean), fixed = TRUE)
names(merged_tables_clean) <- gsub("Mag","Magnitude", names(merged_tables_clean), fixed = TRUE)

#Write file to folder
fwrite(merged_tables_clean, "Cleaned_Data.csv")

#Create table with tidy data average and std
oldwarn <- getOption("warn")
options(warn = -1)
data_final <- aggregate(merged_tables_clean, by = list(merged_tables_clean$Subject.ID, merged_tables_clean$Activity), mean, drop = TRUE)
data_final$Subject.ID = NULL
data_final$Activity = NULL
colnames(data_final)[colnames(data_final) == "Group.1"] = "Subject"
colnames(data_final)[colnames(data_final) == "Group.2"] = "Activity"
fwrite(data_final, "./Data_Subject_Activity.csv")
options(warn = oldwarn)