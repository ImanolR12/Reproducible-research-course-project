---
title: "RSProject"
author: "Thomas Imanol Rodriguez"
date: "3/4/2022"
output: 
  html_document: 
    keep_md: yes
---

#1 Read the dataset

```{r, echo = TRUE}
setwd("C:/Users/imano/Downloads/repdata_data_activity")
activity<-read.csv("activity.csv")
```

#2 Histogram

I made a histogram

```{r, echo = TRUE}
library(dplyr)
library(plyr)

steps <- activity[!(is.na(activity$steps)),]
newdata<-ddply(steps,.(date),summarise, sum =sum(steps))

hist(newdata$sum, xlab="Steps per day", breaks=53,
     main="Histogram of the Total Number of Steps Taken per Day", col = "blue")

```

# 3 Mean and median number of steps taken each day

I calculate and report the mean and median of the total number of steps taken per day
```{r, echo = TRUE}
media<-mean(newdata$sum)
median<-median(newdata$sum)
table(media, median)

```
```{r, echo = TRUE}
library(plotly)
plot_ly(y=newdata$sum, type="box")

```
# Time series plot of the average number of steps take


```{r, echo = TRUE}

stepsinterval <- aggregate( steps~interval, steps,mean)

plot(steps~interval, data=stepsinterval, type="l")
```

# The 5-minute interval that, on average, contains the maximum number of steps

```{r, echo = TRUE}

maxsteps <- stepsinterval[which.max(stepsinterval$steps),]$interval
maxsteps


```
# Code to describe and show a strategy for imputing missing data

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s

```{r, echo = TRUE}

str(activity)
misingvalues<-sum(is.na(activity$steps))

```

Devise a strategy for filling in all of the missing values in the dataset.
I use as  strategy, i completed  the missing values in the dataset with the mean per interval. 
```{r, echo = TRUE}

meanstepsinterval<-function(interval){
    stepsinterval[stepsinterval$interval==interval,]$steps
}


```
I buid a dataset where the mising values are completed with the mean of the mean per interval

```{r, echo = TRUE}

activitydata<-activity 
for(i in 1:nrow(activitydata)){
    if(is.na(activitydata[i,]$steps)){
        activitydata[i,]$steps <- meanstepsinterval(activitydata[i,]$interval)
    }
}
```

The new dataset doesnt have missing values

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r, echo = TRUE}
totalsteps <- aggregate(steps ~ date, data=activitydata, sum)
hist(totalsteps$steps)


```

```{r, echo = TRUE}
mediasteps <- mean(totalsteps$steps)
medianSsteps <- median(totalsteps$step)

```


# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r, echo = TRUE}

activitydata$date <- as.Date(strptime(activitydata$date, format="%Y-%m-%d"))
activitydata$day <- weekdays(activitydata$date)
for (i in 1:nrow(activitydata)) {
    if (activitydata[i,]$day %in% c("Saturday","Sunday")) {
        activitydata[i,]$day<-"weekend"
    }
    else{
        activitydata[i,]$day<-"weekday"
    }
}
stepsday <- aggregate(activitydata$steps ~ activitydata$interval + activitydata$day, activitydata, mean)

```
I made a graph with the pattern
```{r, echo = TRUE}

names(stepsday) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsday, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
    
```