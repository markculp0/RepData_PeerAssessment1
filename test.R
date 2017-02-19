# Reproducible Research: Peer Assessment 1
# Test Script


# unzip("activity.zip", exdir = "data")
act <- read.csv("data/activity.csv", header = T)
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")

library(dplyr)

# ---

meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
names(meanStepPerDay) <- c("Date","Mean.Steps")
totalStepPerDay <- aggregate(act[,1], list(act$date), sum)
names(totalStepPerDay) <- c("Date", "Total.Steps")

act_filtered <- filter(act, steps!= 0 & !is.na(steps))
medianStepPerDay <- aggregate(act_filtered[,1], list(act_filtered$date), median)
names(medianStepPerDay) <- c("Date","Median.Steps")

# ---

totalSteps <- sum(act_filtered$steps)

# Maximum Number of steps 
act_filtered[which.max(act_filtered$steps),]











