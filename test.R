# Reproducible Research: Peer Assessment 1
# Test Script

library(dplyr)

# unzip("activity.zip", exdir = "data")
act <- read.csv("data/activity.csv", header = T)
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")

# ---
# 1. Mean Total Steps 

# Calculate mean steps per day
#meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
#names(meanStepPerDay) <- c("date","mean.steps")

# Calculate total steps per day
totalStepPerDay <- aggregate(act[,1], list(act$date), sum)
names(totalStepPerDay) <- c("date", "total.steps")

# Filter out NA's and 0's, then calculate median steps per day
#act_filtered <- filter(act, steps!= 0 & !is.na(steps))
#medianStepPerDay <- aggregate(act_filtered[,1], list(act_filtered$date), median)
#names(medianStepPerDay) <- c("date","median.steps")

# Plot a histogram of the total steps taken per day
hist(totalStepPerDay$total.steps, xlab = "Total Steps Per Day")

# Calculate mean of the total steps taken per day
mean(totalStepPerDay$total.steps, na.rm = T)

# Calculate median of the total steps taken per day
median(totalStepPerDay$total.steps, na.rm = T)

# ---
# 2.Av Daily Activity Pattern

# Grouped - Not needed
# byDate <- group_by(act, date)
# dailyAv <- mutate(byDate, dailyTotal=cumsum(steps), dailyCnt=cumsum(steps>=0))


# Calculate 5 minute interval means
byInterval <- group_by(act, interval)
intervalAv <- summarize(byInterval, intrvalMean=mean(steps, na.rm = T))


# Ungrouped, dailyCnt s/b?? interval * (as.numeric(act[4454,2] - act[1,2]))
# act_NoNA <- filter(act, !is.na(steps))
# allDayAv <- mutate(act_NoNA, avSteps=cumsum(steps), dailyCnt=(cumsum(steps>=0) - 1) * 5, avg=avSteps/dailyCnt)

#  allDayAv$dateInterval <- paste(allDayAv$date,allDayAv$interval)

# adaTS <- ts(allDayAv$avg)

# Create time series of interval averages 
iaTS <- ts(intervalAv$intrvalMean)

# Plot time time series of interval averages
plot.ts(iaTS, type="l")

# Calculate interval w highest average maximum
avgMax <- which.max(intervalAv$intrvalMean)
intervalAv[avgMax,]

# avgMax <- which.max(allDayAv$avg)
# allDayAv[avgMax,c(2,3,6)]


#---
# 3. Imputed values

# Should load before loading dplyr
library(plyr)

# Calculate total missing values in dataset
sum(is.na(act$steps))

# Calculate mean steps per day
meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
names(meanStepPerDay) <- c("date","mean.steps")

# Fill in missing daily mean calculations with mean over all days
meanStepPerDay$mean.steps[is.na(meanStepPerDay$mean.steps)] = mean(meanStepPerDay$mean.steps, na.rm = T)

# Add column with daily mean values to new activity data frame
act_imputed <- arrange(join(act,meanStepPerDay), date)

# Fill NA step values with daily mean values
act_imputed$steps[is.na(act_imputed$steps)] = act_imputed$mean.steps

totalStepPerDay2 <- aggregate(act_imputed[,1], list(act_imputed$date), sum)
names(totalStepPerDay2) <- c("date", "total.steps")

# Create histogram from data frame with imputed values
hist(totalStepPerDay2$total.steps)

#meanStepPerDay2 <- aggregate(act_imputed[,1], list(act_imputed$date), mean)
#names(meanStepPerDay2) <- c("date","mean.steps")

#act_filtered2 <- filter(act_imputed, steps!= 0 & !is.na(steps))
#medianStepPerDay2 <- aggregate(act_filtered2[,1], list(act_filtered2$date), median)
#names(medianStepPerDay2) <- c("date","median.steps")

#-
# 4. Weekday and Weekend comparisons

# Add column to describe measurement date as weekday or weekend
act_imputed$weekday <- weekdays(act_imputed$date)
act_imputed$weekday  <- ifelse(act_imputed$weekday %in% c("Sunday","Saturday"), "weekend", "weekday")

# Subset the weekend data
act_weekend <- filter(act_imputed, weekday == "weekend")

# Calculate 5 minute interval means on imputed dataset
byInterval2 <- group_by(act_weekend, interval)
intervalAv2 <- summarize(byInterval2, intrvalMean=mean(steps, na.rm = T))

# Create time series of 5 minute interval averages 
iaTS2 <- ts(intervalAv2$intrvalMean)

# Plot time time series of interval averages
plot.ts(iaTS2, type="l")


# avWeekEndSteps <- mutate(act_weekend, avSteps=cumsum(steps), dailyCnt=cumsum(steps>=0), avg=avSteps/dailyCnt)

# avweTS <- ts(avWeekEndSteps$avg)

# plot.ts(avweTS, type="l")
