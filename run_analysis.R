dir<-"E://program//TidyDataCourse//UCI HAR Dataset"


calcMeanAndSd<-function (all) 
{
  
  ret<-data.frame()
  ret=matrix(nrow=nrow(all),ncol=12)
  dimnames(ret)<-list(1:nrow(all),
                      c("tBodyAcc_mean_X","tBodyAcc_mean_Y","tBodyAcc_mean_Z",
                        "tBodyAcc_std_X","tBodyAcc_std_Y","tBodyAcc_std_Z",
                        "tBodyGyro_mean_X","tBodyGyro_mean_Y","tBodyGyro_mean_Z",
                        "tBodyGyro_std_X","tBodyGyro_std_Y","tBodyGyro_std_Z"))
                        
  acc_x<-select(all,acc_x1:acc_x128)
  acc_y<-select(all,acc_y1:acc_y128)
  acc_z<-select(all,acc_z1:acc_z128)
  gyro_x<-select(all,gyro_x1:gyro_x128)
  gyro_y<-select(all,gyro_y1:gyro_y128)
  gyro_z<-select(all,gyro_z1:gyro_z128)
  
  
  for (i in 1:nrow(all)) {
    
    ret[i,"tBodyAcc_mean_X"]<-mean(as.numeric(acc_x[i,]))
    ret[i,"tBodyAcc_mean_Y"]<-mean(as.numeric(acc_y[i,]))
    ret[i,"tBodyAcc_mean_Z"]<-mean(as.numeric(acc_z[i,]))
    
    ret[i,"tBodyGyro_mean_X"]<-mean(as.numeric(gyro_x[i,]))
    ret[i,"tBodyGyro_mean_Y"]<-mean(as.numeric(gyro_y[i,]))
    ret[i,"tBodyGyro_mean_Z"]<-mean(as.numeric(gyro_z[i,]))
    
    ret[i,"tBodyAcc_std_X"]<-sd(as.numeric(acc_x[i,]))
    ret[i,"tBodyAcc_std_Y"]<-sd(as.numeric(acc_y[i,]))
    ret[i,"tBodyAcc_std_Z"]<-sd(as.numeric(acc_z[i,]))
    
    ret[i,"tBodyGyro_std_X"]<-sd(as.numeric(gyro_x[i,]))
    ret[i,"tBodyGyro_std_Y"]<-sd(as.numeric(gyro_y[i,]))
    ret[i,"tBodyGyro_std_Z"]<-sd(as.numeric(gyro_z[i,]))
    
    
  }

  ret<-cbind(all[,1:3],ret)
  ret
}




library(dplyr)


setwd(dir)


label<-read.table("activity_labels.txt")

#Load activitis in  data set.

setwd(paste(dir,"test",sep="//"))

act<-read.table("y_test.txt")

#Load subjects in  data set.

all<-read.table("subject_test.txt")


#Uses descriptive activity names to name the activities in the data set

act<-inner_join(act,label)
all<-cbind(all,act)

#Appropriately labels the data set with descriptive variable names. 

names(all)<-c("subject","num_action","action")


setwd(paste(dir,"test","Inertial Signals",sep="//"))


#Load whole test sets to create one data set.
test<-read.table ("body_acc_x_test.txt")
names(test)<-paste("acc_x",seq(1:128),sep="")
all<-cbind(all,test)

test<-read.table ("body_acc_y_test.txt")
names(test)<-paste("acc_y",seq(1:128),sep="")
all<-cbind(all,test)

test<-read.table("body_acc_z_test.txt")
names(test)<-paste("acc_z",seq(1:128),sep="")
all<-cbind(all,test)

test<-read.table("body_gyro_x_test.txt")
names(test)<-paste("gyro_x",seq(1:128),sep="")
all<-cbind(all,test)

test<-read.table("body_gyro_y_test.txt")
names(test)<-paste("gyro_y",seq(1:128),sep="")
all<-cbind(all,test)

test<-read.table("body_gyro_z_test.txt")
names(test)<-paste("gyro_z",seq(1:128),sep="")
all<-cbind(all,test)






#Extracts only the measurements on the mean and standard deviation for each measurement.

all<-calcMeanAndSd(all)

#From the data set creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject.


tidy<-tbl_df(all)

tidy<-summarize(group_by(tidy,subject,action),mean(tBodyAcc_mean_X),mean(tBodyAcc_mean_Y),mean(tBodyAcc_mean_Z),
                mean(tBodyAcc_std_X),mean(tBodyAcc_std_Y),mean(tBodyAcc_std_Z),
                mean(tBodyGyro_mean_X),mean(tBodyGyro_mean_Y),mean(tBodyGyro_mean_Z),
                mean(tBodyGyro_std_X),mean(tBodyGyro_std_Y),mean(tBodyGyro_std_Z))

#Write result to table


setwd(dir)
write.table(tidy,"tidyDataSet.txt",row.name=FALSE )

