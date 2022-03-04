---
title: "RSProject"
author: "Thomas Imanol Rodriguez"
date: "3/4/2022"
output: 
  html_document: 
    keep_md: yes
---

#1 Read the dataset


```r
setwd("C:/Users/imano/Downloads/repdata_data_activity")
activity<-read.csv("activity.csv")
```

#2 Histogram
I made a histogram


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(plyr)
```

```
## ------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
steps <- activity[!(is.na(activity$steps)),]
newdata<-ddply(steps,.(date),summarise, sum =sum(steps))

hist(newdata$sum, xlab="Steps per day", breaks=53,
     main="Histogram of the Total Number of Steps Taken per Day", col = "blue")
```

![](rr_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# 3 Mean and median number of steps taken each day

I calculate and report the mean and median of the total number of steps taken per day

```r
media<-mean(newdata$sum)
median<-median(newdata$sum)
table(media, median)
```

```
##                   median
## media              10765
##   10766.1886792453     1
```

```r
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.1.2
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 4.1.2
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, mutate, rename, summarise
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
plot_ly(y=newdata$sum, type="box")
```

```{=html}
<div id="htmlwidget-48f684d0fe30e5a94466" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-48f684d0fe30e5a94466">{"x":{"visdat":{"fa147d8b6b1a":["function () ","plotlyVisDat"]},"cur_data":"fa147d8b6b1a","attrs":{"fa147d8b6b1a":{"y":[126,11352,12116,13294,15420,11015,12811,9900,10304,17382,12426,15098,10139,15084,13452,10056,11829,10395,8821,13460,8918,8355,2492,6778,10119,11458,5018,9819,15414,10600,10571,10439,8334,12883,3219,12608,10765,7336,41,5441,14339,15110,8841,4472,12787,20427,21194,14478,11834,11162,13646,10183,7047],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"yaxis":{"domain":[0,1],"automargin":true,"title":[]},"xaxis":{"domain":[0,1],"automargin":true},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"rgba(31,119,180,0.5)","y":[126,11352,12116,13294,15420,11015,12811,9900,10304,17382,12426,15098,10139,15084,13452,10056,11829,10395,8821,13460,8918,8355,2492,6778,10119,11458,5018,9819,15414,10600,10571,10439,8334,12883,3219,12608,10765,7336,41,5441,14339,15110,8841,4472,12787,20427,21194,14478,11834,11162,13646,10183,7047],"type":"box","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
# Time series plot of the average number of steps take



```r
stepsinterval <- aggregate( steps~interval, steps,mean)

plot(steps~interval, data=stepsinterval, type="l")
```

![](rr_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# The 5-minute interval that, on average, contains the maximum number of steps


```r
maxsteps <- stepsinterval[which.max(stepsinterval$steps),]$interval
maxsteps
```

```
## [1] 835
```
# Code to describe and show a strategy for imputing missing data

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
misingvalues<-sum(is.na(activity$steps))
```

Devise a strategy for filling in all of the missing values in the dataset.
I use as  strategy, i completed  the missing values in the dataset with the mean per interval. 

```r
meanstepsinterval<-function(interval){
    stepsinterval[stepsinterval$interval==interval,]$steps
}
```
I buid a dataset where the mising values are completed with the mean of the mean per interval


```r
activitydata<-activity 
for(i in 1:nrow(activitydata)){
    if(is.na(activitydata[i,]$steps)){
        activitydata[i,]$steps <- meanstepsinterval(activitydata[i,]$interval)
    }
}
```

The new dataset doesnt have missing values

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
totalsteps <- aggregate(steps ~ date, data=activitydata, sum)
hist(totalsteps$steps)
```

![](rr_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mediasteps <- mean(totalsteps$steps)
medianSsteps <- median(totalsteps$step)
```


# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
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

```r
names(stepsday) <- c("interval", "day", "steps")
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 4.1.2
```

```r
xyplot(steps ~ interval | day, stepsday, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](rr_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
