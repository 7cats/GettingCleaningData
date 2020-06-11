####  W4 peer-review assignment for Get and cleaning data course   ####
####                       by Zixin                                ####

### load package
library(dplyr)
library(tidyr)
library(mgsub)

### download and unzip data
if(!exists("./getdata_projectfiles")) {dir.create("./getdata_projectfiles")}
setwd("./getdata_projectfiles")
fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "./projectData.zip", method = "curl")
unzip("./projectData.zip", junkpaths = T, exdir = getwd())

### read data # SigVal = signal value # Act = activity
testSubject <- read.table("subject_test.txt")
testSigVal <- read.table("X_test.txt")
testAct  <- read.table("y_test.txt")

trainSubject <- read.table("subject_train.txt")
trainSigVal <- read.table("X_train.txt")
trainAct  <- read.table("y_train.txt")

features <- read.table("features.txt")
actLabel <- read.table("activity_labels.txt")

### put test and train data together
allSigVal <- rbind(testSigVal,trainSigVal)
allSubject <- rbind(testSubject,trainSubject)
allAct <- rbind(testAct,trainAct)
colnames(allSigVal) <- features[,2]
colnames(allSubject) <- "Subject"
colnames(allAct) <- "Activity"
allData <- cbind(allSubject,allAct,allSigVal)

#### tidy data
meanStd <- cbind(Subject = allData$Subject,Activity = allData$Activity,
                 select(allData, contains(c("mean","std")))) # select mean and std

colnames(meanStd) <-   # simplify the name of signals
    names(meanStd) %>%
    gsub("mean","Mean",.) %>%
    gsub("[[:punct:]]","",.) %>%
    gsub("-","",.)

meanStd$Activity <-  # modify the name of activity
    meanStd$Activity %>%
    mgsub(actLabel$V1,actLabel$V2) %>%
    tolower() %>%
    gsub("_","",.)

aveMeanStd <-    ## final result of step 5
    meanStd %>%
    group_by(Activity, Subject) %>%   # group the data by activity and subject
    summarise_if(is.numeric,mean,na.rm = T)  # get mean by group

### save the result
write.csv(meanStd, file = "MeanSTD.csv")
write.csv(aveMeanStd, file = "AveofMeanSTD.csv")


