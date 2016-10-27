

library("dplyr")

## read txt into individual data
x_train<-read.table("./train/X_train.txt")
y_train<-read.table("./train/y_train.txt")
subject_train<-read.table("./train/subject_train.txt")

x_test<-read.table("./test/X_test.txt")
y_test<-read.table("./test/y_test.txt")
subject_test<-read.table("./test/subject_test.txt")

## combine all train/test data into one data set
data_train<-cbind(subject_train, y_train, x_train)
data_test<-cbind(subject_test, y_test, x_test)

## combine train and test data together
data_full<-rbind(data_train, data_test)

## complte task 1
#..........................................................................

## read feature names into R, only take 2nd column as characters of the raw data
features<-read.table("./features.txt", stringsAsFactors = FALSE)[,2]

## rename data_full with names in features
#colnames(data_full)<-c("subject", "activity", features)

## extract data with name including mean or std
#extracteddata<-select(data_full, contains("mean|std")) ##cause R to terminate, seems due to huge data set

## looks like rename all the columns then select from them occupies too much memory
## need other way to do this

## get index of columns with name containing mean or std in features
index <- grep(("mean\\(\\)|std\\(\\)"), features)

## extact data based on extected column index, and keep first two columns 
data_final <- data_full[, c(1, 2, index+2)]

## rename columns with names in the features
colnames(data_final) <- c("subject", "activity", features[index])

##complete step 2
#.........................................................................

## read activity labels into R, only take 2nd columns as names, convert to character
activitylabel<-read.table("./activity_labels.txt", stringsAsFactors = FALSE)[,2]

## replace numbers in activity column with actural activity names
for (i in 1:6){
  data_final$activity<-gsub(i, activitylabel[i], data_final$activity )
}

## complete step 3
#..........................................................................

## replace the names with decriptive name
names(data_final)<-gsub("^t", "time", names(data_final))
names(data_final)<-gsub("^f", "frequency", names(data_final))

## complete step 4
#...........................................................................

## group final data by subject and activity and summarize mean of each groups
newdata<-summarize_each(group_by(data_final, subject, activity), funs(mean)
)
## summarise cause program to terminate again

#newdata <- data_final %>%
#  group_by(subject, activity) %>%
#  summarise_each(funs(mean))

#complete step 5

write.table(newdata, file = "./XXA_submission.txt", row.names = FALSE)






