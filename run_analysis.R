library(plyr)

dir<-"E://program//TidyDataCourse//UCI HAR Dataset"

setwd(dir)


#Load names and labels


labels<-read.table("activity_labels.txt",stringsAsFactors=F)
features<-read.table("features.txt")

#Load training and the test sets to create one data set.

setwd(paste(dir,"test",sep="//"))
testx<-read.table("X_test.txt")
testy<-read.table("Y_test.txt")
testsubj<-read.table("subject_test.txt")

setwd(paste(dir,"train",sep="//"))
      
trainx<-read.table("X_train.txt")
trainy<-read.table("Y_train.txt")
trainsubj<-read.table("subject_train.txt")
      
#Merges the training and the test sets to create one data set.


trainx<-rbind(trainx,testx)
trainy<-rbind(trainy,testy)
trainsubj<-rbind(trainsubj,testsubj)

rm(testx)
rm(testy)
rm(testsubj)

#Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std <- grep( "mean|std", features$V2) 

#Appropriately labels the data set with descriptive variable names. 

names(trainx)<-features$V2
names(trainsubj)<-"subject"

trainx<-trainx[,mean_std]

#Uses descriptive activity names to name the activities in the data set
trainy<-inner_join(trainy,labels)
names(trainy)<-c("num_activity","activity")


all<-cbind(trainsubj,activity=trainy$activity)
all<-cbind(all,trainx)
              
#From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- ddply( all, .(subject, activity), numcolwise(mean) )

#Write result to table


setwd(dir)
write.table(tidy_dataset,"tidyDataSet.txt",row.name=FALSE )

