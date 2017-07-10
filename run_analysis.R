# Getting and cleaning data course project

####################### Load train data

## load train base data
X_train <- read.table("~/Coursera/3. Getting and cleaning data/train/X_train.txt", header = FALSE)
dim(X_train)
## load train activities data
Y_train <- read.table("~/Coursera/3. Getting and cleaning data/train/Y_train.txt", header = FALSE)
dim(Y_train)
### load train subject data
subject_train <- read.table("~/Coursera/3. Getting and cleaning data/train/subject_train.txt", header = FALSE)
dim(subject_train)

####################### Load test data

## load test base data
X_test <- read.table("~/Coursera/3. Getting and cleaning data/test/X_test.txt", header = FALSE)
dim(X_test)
## load test activities data
Y_test <- read.table("~/Coursera/3. Getting and cleaning data/test/Y_test.txt", header = FALSE)
dim(Y_test)
### load train subject data
subject_test <- read.table("~/Coursera/3. Getting and cleaning data/test/subject_test.txt", header = FALSE)
dim(subject_test)

####################### Load generic files

## load variable names
features <- read.table("~/Coursera/3. Getting and cleaning data/features.txt", header = FALSE)
dim(features)
## load activity names
activities <- read.table("~/Coursera/3. Getting and cleaning data/activity_labels.txt", header = FALSE)
dim(activities)

####################### Organize train data

## rename variables in train set
names(X_train) <- features$V2
valid_names <- make.names(names = names(X_train), unique = TRUE, allow_ = TRUE) # remove double column names (https://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name)
names(X_train) <- valid_names

names(Y_train) <- "activity"
names(subject_train) <- "subject"

## bind subject and train data
train_bind <- cbind(X_train, Y_train, subject_train)

####################### Organize test data

## rename variables in test set
names(X_test) <- features$V2
valid_names <- make.names(names = names(X_test), unique = TRUE, allow_ = TRUE) # remove double column names (https://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name)
names(X_test) <- valid_names

names(Y_test) <- "activity"
names(subject_test) <- "subject"

## bind subject and test data
test_bind <- cbind(X_test, Y_test, subject_test)

####################### Q1: merge test and train data

dim(train_bind)
dim(test_bind)
merged <- rbind(train_bind, test_bind)
dim(merged)

####################### Q2: Extracts measurements on mean and standard deviation

## select mean and std
library(dplyr)
merged_sel <- select(merged, subject, activity, grep("mean", names(merged)), grep("std", names(merged)))
dim(merged_sel)

####################### Q3: Uses descriptive activity names to name the activities

table(merged_sel$activity)
activities

## merge activity labels into data
colnames(activities)[1] <- "activity"
Merged_sel2 <- merge(merged_sel, activities, by = "activity")

## remove old activity variables, reorder columnposition and rename new variable
Merged_sel2 <- Merged_sel2[, c(82, 2:81)] #http://www.sthda.com/english/wiki/reordering-data-frame-columns-in-r
colnames(Merged_sel2)[1] <- "activity"
levels(Merged_sel2$activity) <- tolower(levels(Merged_sel2$activity))

####################### Q4: Appropriately label the data set with descriptive variable names.

# https://stackoverflow.com/questions/22625314/renaming-similarly-named-multiple-columns-in-r-quickly
# cheatsheet: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

names(Merged_sel2) <- tolower(names(Merged_sel2))
names(Merged_sel2) <- gsub("^t", "time of ", names(Merged_sel2))
names(Merged_sel2) <- gsub("^f", "frequency of ", names(Merged_sel2))
names(Merged_sel2) <- gsub("acc", " acceleration", names(Merged_sel2))
names(Merged_sel2) <- gsub("mag", " magnitude", names(Merged_sel2))
names(Merged_sel2) <- gsub("gyro", " gyroscope", names(Merged_sel2))
names(Merged_sel2) <- gsub("."," ", names(Merged_sel2), fixed = TRUE)
names(Merged_sel2) <- gsub("   "," ", names(Merged_sel2), fixed = TRUE)
names(Merged_sel2) <- gsub("meanfreq","mean", names(Merged_sel2))
names(Merged_sel2) <- gsub("std","standard deviation", names(Merged_sel2))
names(Merged_sel2) <- gsub("bodybody","body", names(Merged_sel2))
names(Merged_sel2) <- gsub("accelerationjerk", "acceleration jerk", names(Merged_sel2))
names(Merged_sel2) <- gsub("gyroscopejerk", "gyroscope jerk", names(Merged_sel2))

colnames(Merged_sel2)


####################### Q5: Create independent tidy data set with the average of each variable for each activity and each subject.

## aggregate data over activty and subject http://www.statmethods.net/management/aggregate.html
attach(Merged_sel2)
tidy_data <- aggregate(Merged_sel2[, 3:81], by = list(subject, activity), FUN = mean)
detach(Merged_sel2)

colnames(tidy_data)[1] <- "subject"
colnames(tidy_data)[2] <- "activity"

## create data set as txt file
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)

## test
tidy_data_test <- read.table("~/tidy_data.txt", header = TRUE)
View(tidy_data_test)
