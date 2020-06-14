#library(data.table)
#library(reshape2)
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#path<-getwd()
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url, file.path(path, "dataFile.zip"))
#unzip(zipfile = 'dataFile.zip')
activitylabels<- fread('UCI HAR Dataset/activity_labels.txt',
                       col.names = c('classLabel','activityName'))
features<- fread('UCI HAR Dataset/features.txt',
                 col.names = c('index', 'featureName'))
desiredfeatures<-grep("(mean|std)\\(\\)", features[,featureName])
reqmeasure<- features[desiredfeatures,featureName]
reqmeasure<-gsub("[()]","", reqmeasure) #cleaning data by removing brackets

#reading train data
train<-fread("UCI HAR Dataset/train/X_train.txt")[,desiredfeatures,with=FALSE]
setnames(train,colnames(train),reqmeasure)
trActivities<-fread("UCI HAR Dataset/train/y_train.txt",
                   col.names = c("Activity"))
trSubjects<- fread("UCI HAR Dataset/train/subject_train.txt",
                   col.names = c("SubNum"))
train<-cbind(train,trActivities,trSubjects)

#reading test data
test<-fread("UCI HAR Dataset/test/X_test.txt")[,desiredfeatures, with=FALSE]
setnames(test,colnames(test),reqmeasure)
testActivities<-fread("UCI HAR Dataset/test/y_test.txt",
                    col.names = c("Activity"))
testSubjects<- fread("UCI HAR Dataset/test/subject_test.txt",
                   col.names = c("SubNum"))
test<-cbind(test,testActivities,testSubjects)

#merging data
mergeDT<- rbind(train,test)

#renaming classLabel with explicit activity name
mergeDT[["Activity"]]<-factor(mergeDT[,Activity], 
                              levels = activitylabels[['classLabel']],
                              labels = activitylabels[['activityName']])
mergeDT[['SubNum']]<-as.factor(mergeDT[,SubNum])

mergeDT<- melt(data = mergeDT, id=c("SubNum","Activity"))
mergeDT<-dcast(data = mergeDT, SubNum + Activity ~ variable, fun.aggregate = mean)

fwrite(x=mergeDT, file = 'tidydata.txt', quote = FALSE)
