# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Fork data files from https://github.com/rdpeng/RepData_PeerAssessment1
# unzip("activity.zip", exdir = "data")
act <- read.csv("data/activity.csv", header = T)
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")

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


## What is mean total number of steps taken per day?


```r
meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
names(meanStepPerDay) <- c("Date","Mean.Steps")
totalStepPerDay <- aggregate(act[,1], list(act$date), sum)
names(totalStepPerDay) <- c("Date", "Total.Steps")

act_filtered <- filter(act, steps!= 0 & !is.na(steps))
medianStepPerDay <- aggregate(act_filtered[,1], list(act_filtered$date), median)
names(medianStepPerDay) <- c("Date","Median.Steps")
```


## What is the average daily activity pattern?


```r
totalSteps <- sum(act_filtered$steps)
```



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
