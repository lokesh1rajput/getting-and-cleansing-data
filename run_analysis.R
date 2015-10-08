# This R script does the following: 

 3

 

 4
# 1. Merges the training and the test sets to create one data set. 

 5

 

 6
tmp1 <- read.table("train/X_train.txt") 

 7
tmp2 <- read.table("test/X_test.txt") 

 8
X <- rbind(tmp1, tmp2) 

 9

 

 10
tmp1 <- read.table("train/subject_train.txt") 

 11
tmp2 <- read.table("test/subject_test.txt") 

 12
S <- rbind(tmp1, tmp2) 

 13

 

 14
tmp1 <- read.table("train/y_train.txt") 

 15
tmp2 <- read.table("test/y_test.txt") 

 16
Y <- rbind(tmp1, tmp2) 

 17

 

 18
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

 19

 

 20
features <- read.table("features.txt") 

 21
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) 

 22
X <- X[, indices_of_good_features] 

 23
names(X) <- features[indices_of_good_features, 2] 

 24
names(X) <- gsub("\\(|\\)", "", names(X)) 

 25
names(X) <- tolower(names(X)) 

 26

 

 27
# 3. Uses descriptive activity names to name the activities in the data set. 

 28

 

 29
activities <- read.table("activity_labels.txt") 

 30
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2]))) 

 31
Y[,1] = activities[Y[,1], 2] 

 32
names(Y) <- "activity" 

 33

 

 34
# 4. Appropriately labels the data set with descriptive activity names. 

 35

 

 36
names(S) <- "subject" 

 37
cleaned <- cbind(S, Y, X) 

 38
write.table(cleaned, "merged_clean_data.txt") 

 39

 

 40
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject. 

 41

 

 42
uniqueSubjects = unique(S)[,1] 

 43
numSubjects = length(unique(S)[,1]) 

 44
numActivities = length(activities[,1]) 

 45
numCols = dim(cleaned)[2] 

 46
result = cleaned[1:(numSubjects*numActivities), ] 

 47

 

 48
row = 1 

 49
for (s in 1:numSubjects) { 

 50
    for (a in 1:numActivities) { 

 51
        result[row, 1] = uniqueSubjects[s] 

 52
        result[row, 2] = activities[a, 2] 

 53
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ] 

 54
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols]) 

 55
        row = row+1 

 56
    } 

 57
} 

 58
write.table(result, "data_set_with_the_averages.txt") 
