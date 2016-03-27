#Loading and preprocessing the data
MyData <- read.csv(file="/Users/wolfram/Dropbox/WD/reproducible research/activity.csv", header=TRUE, sep=",")
library(data.table)
StructData <- as.data.frame(MyData)
head(StructData)
CleanData <- StructData[complete.cases(StructData),]
head(CleanData)

#Mean total number of steps taken per day
CleanData <- data.table(CleanData)
Stepsperday <- CleanData[,sum(steps),by = date]

names(Stepsperday) <- c("date","sum")
Stepsperday$date <- as.POSIXct(Stepsperday$date)

install.packages("ggplot2")
library(ggplot2)
library(scales)

ggplot(Stepsperday, aes(date, sum)) + geom_bar(stat = "sum")+theme(legend.position = "none", plot.title = element_text(lineheight=.8, face="bold"))+ggtitle("Numbers of Steps per Day")
meanstepsperday <- mean(Stepsperday$sum)  
medianstepsperday <- as.numeric(median(Stepsperday$sum))

#Average daily activity pattern
Avgstepsperinterval <- CleanData[,mean(steps), by = interval]
names(Avgstepsperinterval) <- c("interval","average")
Avgstepsperinterval
ggplot(Avgstepsperinterval, aes(interval, average)) + geom_bar(stat = "identity")+theme(legend.position = "none", plot.title = element_text(lineheight=.8, face="bold"))+ggtitle("Average Steps per Interval")
higheststeps <- max(Avgstepsperinterval$average)
higeststepinterval <- as.numeric(Avgstepsperinterval$interval[Avgstepsperinterval$average == higheststeps])

#Imputing missing values
missingcount <- sum(is.na(StructData$steps))

TotalAvg <- mean(Avgstepsperinterval$average)
for(i in 1:nrow(StructData)){
  if(is.na(StructData$steps[i])){
    StructData$steps[i] <- TotalAvg
  }
}

StructData2 <- data.table(StructData)

Stepsperday2 <- StructData2[,sum(steps),by = date]
names(Stepsperday2) <- c("date","sum")
Stepsperday2$date <- as.POSIXct(Stepsperday2$date)
str(Stepsperday2)

#Histogram with the NA values replaced
ggplot(Stepsperday2, aes(date, sum)) + geom_bar(stat = "sum")+theme(legend.position = "none", plot.title = element_text(lineheight=.8, face="bold"))+ggtitle("Number of Steps per Day (NAs replaced)")
meanstepsperday2 <- mean(Stepsperday2$sum)
medianstepsperday2 <- as.numeric(median(Stepsperday2$sum))

#Difference in activity patterns b/w weekdays and weekends
library(timeDate)
StructData2$date <- as.POSIXct(StructData2$date)
StructData2$daytype <- ifelse(isWeekday(StructData2$date, wday = 1:5),"weekday","weekend")

install.packages("lattice")
library(lattice)
str(StructData2)
xyplot(steps~interval | factor(daytype), data = StructData2, pch = 13, type= c("l","r"),main = "Steps - Weekdays vs. Weekends")

install.packages("knitr")
library(knitr)
knit2html("/Users/wolfram/Desktop/Working Directory/Work Files/RepResearch/PA_Template1.R")
getwd()
