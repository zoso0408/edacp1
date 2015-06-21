run_analysis<-function(){
  
  ########## Merging X_Train and X_Test ##########
  xtrainfile<-"UCI HAR Dataset/train/X_train.txt"
  xtraindata<-read.table(xtrainfile)
    
  xtestfile<-"UCI HAR Dataset/test/X_test.txt"
  xtestdata<-read.table(xtestfile)
  
  xall<-rbind(xtraindata, xtestdata)
  
  ######## End Merging X_Train and X_Test ########
  
  
  ########## Merging y_Train and X_Test ##########
  ytrainfile<-"UCI HAR Dataset/train/y_train.txt"
  ytraindata<-read.table(ytrainfile)
   
  ytestfile<-"UCI HAR Dataset/test/y_test.txt"
  ytestdata<-read.table(ytestfile)
   
  ally<-rbind(ytraindata, ytestdata)
  names(ally)<-"Activity";
  ######## End Merging X_Train and X_Test ########
  
  
  ####### Merging Subject Train and X_Test #######
  filesubjecttrain<- "UCI HAR Dataset/train/subject_train.txt"
  TrainsubjectData<-read.table(filesubjecttrain)
  
  filesubjecttest<- "UCI HAR Dataset/test/subject_test.txt"
  TestsubjectData<-read.table(filesubjecttest)
  
  totalsubject<-rbind(TrainsubjectData, TestsubjectData)
  names(totalsubject)<-"Subject";
  ##### End Merging Subject Train and X_Test #####
  
  
  ############## Reading Feature Data ############
  filefeatures<-"UCI HAR Dataset/features.txt" 
  FeatureData<-read.table(filefeatures)
  ########## End Reading Feature Data ############
  
  
  FeatureData<-FeatureData[,2] ## Subset Feature data to exclude rownames
  checkgrep<- grep("mean|std", FeatureData); ## Get indices of vector where we see mean or std 

  names(xall)<- FeatureData; ## Give descriptive column names

  totalselectx<- xall[,checkgrep] ## Subsetting total X data for mean and std measurements

  
  ###### Creating Complete Data set by merging all data sets prepared earlier ######  
    completeset<-cbind(totalselectx, totalsubject, ally) 
  #### End Creating Complete Data set by merging all data sets prepared earlier ####

  getnames<-names(totalselectx) ## Extract column names from the completeset
  
  ##### Use melt within reshape2 library to create variables and measurements ####
  library(reshape2)
  completemelt<- melt(completeset, id=c("Subject", "Activity"), measure.vars=getnames) 
  ### End Use melt within reshape2 library to create variables and measurements ###
  
  
  ############ Creating tidy Data ############
  tidydata<-dcast(completemelt, Subject+Activity ~ variable, mean)
  ########## End Creating tidy Data ########## 
  
  
  ########## Reading Activity Labels #########
  labelfile<-"UCI HAR Dataset/activity_labels.txt"
  labeldata<-read.table(labelfile)

  ######## End Reading Activity Labels #######
  names(labeldata)<-c("Activity", "Activity_Desc") # Giving column name Activity Description and Activity to help us merging
  ########## Merging Activity Labels #########  
  mergedata<-merge(labeldata, tidydata, all=FALSE, all.x=TRUE )
  mergedata<- mergedata[,-1]
  ######## End Merging Activity Labels #######

  ########### Writing Tidy Data Set ##########   
  write.table(mergedata, "tidydataset.txt", row.names=FALSE)
  ######### End Writing Tidy Data Set ########   
}