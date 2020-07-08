features <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/features.txt", col.names = c("n","functions"), header = FALSE)
x_test <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/test/X_test.txt", col.names = features$functions, header = FALSE)
y_test <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/test/y_test.txt", col.names = "code", header = FALSE) 
x_train <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/train/X_train.txt", col.names = features$functions, header = FALSE) 
y_train <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/train/y_train.txt", col.names = "code", header = FALSE) 
subj_train <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/train/subject_train.txt", col.names = "subject", header = FALSE) 
subj_test <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/test/subject_test.txt", col.names = "subject", header = FALSE) 
activity_label <- read.table("./Getting_and_Cleaning_Data/UCIHARDataset/activity_labels.txt", col.names = c("code","activity"), header = FALSE)

#combining tables by rows
x <- rbind(x_test,x_train)
y <- rbind(y_test,y_train)
subj <- rbind(subj_test,subj_train)

#merging tables by columns
data_merged <- cbind(subj,y,x)

#selecting all mean and std columns
recorte <- select(data_merged, subject, code, contains("mean"), contains("std"))

#naming the activities
recorte$code <- activity_label[recorte$code,2]

#labels with descriptive variable names
names(recorte)[2] = "activity"
names(recorte)<-gsub("Acc", "Accelerometer", names(recorte))
names(recorte)<-gsub("Gyro", "Gyroscope", names(recorte))
names(recorte)<-gsub("BodyBody", "Body", names(recorte))
names(recorte)<-gsub("Mag", "Magnitude", names(recorte))
names(recorte)<-gsub("^t", "Time", names(recorte))
names(recorte)<-gsub("^f", "Frequency", names(recorte))
names(recorte)<-gsub("tBody", "TimeBody", names(recorte))
names(recorte)<-gsub("-mean()", "Mean", names(recorte), ignore.case = TRUE)
names(recorte)<-gsub("-std()", "STD", names(recorte), ignore.case = TRUE)
names(recorte)<-gsub("-freq()", "Frequency", names(recorte), ignore.case = TRUE)
names(recorte)<-gsub("angle", "Angle", names(recorte))
names(recorte)<-gsub("gravity", "Gravity", names(recorte))


#final data with average of each variable of each activity and subject

Final <- recorte %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(Final, "./Getting_and_Cleaning_Data/UCIHARDataset/Final.txt", row.name=FALSE)
