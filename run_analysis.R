# 1.Merges the training and the test sets to create one data set.
file1<-read.table("./train/X_train.txt")
file2<-read.table("./test/X_test.txt")
X<-rbind(file1,file2)

file1<-read.table("./train/subject_train.txt")
file2<-read.table("./test/subject_test.txt")
subject<-rbind(file1,file2)

file1<-read.table("./train/Y_train.txt")
file2<-read.table("./test/Y_test.txt")
Y<-rbind(file1,file2)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
selected_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, selected_features]
names(X) <- features[selected_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

#3. Uses descriptive activity names to name the activities in the data set.
activity<-read.table("activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
Y[,1] = activity[Y[,1], 2]
names(Y) <- "activity"

#4. Appropriately labels the data set with descriptive variable names.
names(subject) <- "subject"
completed <- cbind(subject, Y, X)
write.table(completed, "merged_clean_data.txt")

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(subject)[,1]
numSubjects = length(uniqueSubjects)
numActivities = length(activity[,1])
numCols = dim(completed)[2]
result = completed[1:(numSubjects*numActivities), ]

row = 1
for (i in 1:numSubjects) {
   for (j in 1:numActivities) {
      result[row, 1] = uniqueSubjects[i]
      result[row, 2] = activity[j, 2]
      tmp <- completed[completed$subject==i & completed$activity==activity[j, 2], ]
      result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
      row = row+1
   }
}
write.table(result, "data_set_with_the_averages.txt", row.name=FALSE)

