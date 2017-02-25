# Reproducible Research: Peer Assessment 1
# Test Script

library(plyr)
library(dplyr)

# unzip("activity.zip", exdir = "data")
act <- read.csv("data/activity.csv", header = T)
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")

# ---
# 1. Mean Total Steps 

# Calculate mean steps per day
#meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
#names(meanStepPerDay) <- c("date","mean.steps")

# Calculate total days
totalDays <- length(unique(sort(act$date)))

# Calculate total steps per day
totalSteps <- aggregate(act[,1], list(act$date), sum)
totalStepPerDay <- totalSteps$x / totalDays

# names(totalStepPerDay) <- c("date", "total.steps")

# Filter out NA's and 0's, then calculate median steps per day
#act_filtered <- filter(act, steps!= 0 & !is.na(steps))
#medianStepPerDay <- aggregate(act_filtered[,1], list(act_filtered$date), median)
#names(medianStepPerDay) <- c("date","median.steps")

# Plot a histogram of the total steps taken per day
hist(totalStepPerDay, ylim = c(0, 20), main = "Histogram of Total Steps Per Day", xlab = "Total Steps Per Day")

# Calculate mean of the total steps taken per day
mean(totalStepPerDay, na.rm = T)

# Calculate median of the total steps taken per day
median(totalStepPerDay, na.rm = T)

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
plot.ts(iaTS, main = "Daily 5 Minute Intervals", ylab = "Average Steps", type="l")

# Calculate interval w highest average maximum
avgMax <- which.max(intervalAv$intrvalMean)
intervalAv[avgMax,]

# avgMax <- which.max(allDayAv$avg)
# allDayAv[avgMax,c(2,3,6)]


#---
# 3. Imputed values

# Should load before loading dplyr
#library(plyr)

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


totalSteps2 <- aggregate(act_imputed[,1], list(act_imputed$date), sum)
totalStepPerDay2 <- totalSteps2$x / totalDays
# names(totalStepPerDay2) <- c("date", "total.steps")

# Create histogram from data frame with imputed values
hist(totalStepPerDay2, ylim = c(0, 25), main = "Histogram of Total Steps Per Day\n(With Imputed Values)", xlab = "Total Steps Per Day")

# The introduction of imputed values raised the median value.  The median value was skewed in the original calculation by removing 0 values from the calculation.  The mean value remained fairly constant with higher total values in the 150 to 200 five minute interval periods, and overall less dispersion.


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

# Subset the weekend and weekday data
act_weekend <- filter(act_imputed, weekday == "weekend")
act_weekday <- filter(act_imputed, weekday == "weekday")

# Calculate 5 minute interval means on imputed weekday datasets
# By weekend: 
byInterval2 <- group_by(act_weekend, interval)
intervalAv2 <- summarize(byInterval2, intrvalMean=mean(steps, na.rm = T))
# By weekday:
byInterval3 <- group_by(act_weekday, interval)
intervalAv3 <- summarize(byInterval3, intrvalMean=mean(steps, na.rm = T))

# Create time series of 5 minute interval averages 
iaTS2 <- ts(intervalAv2$intrvalMean)
iaTS3 <- ts(intervalAv3$intrvalMean)

# Panel plot w 2 row, 1 column
par(mfrow = c(2,1), mar = c(4,4,2,1))

# Plot time time series of interval averages
plot.ts(iaTS2, type="l", main = "Weekend", ylab = "No. of Steps")
plot.ts(iaTS3, type="l", main = "Weekday", ylab = "No. of Steps")

# Weekend activity levels were overall higher.  Weekday activity levels showed 
# a more pronounced spike in the approximate 110 minute interval period.

# avWeekEndSteps <- mutate(act_weekend, avSteps=cumsum(steps), dailyCnt=cumsum(steps>=0), avg=avSteps/dailyCnt)

# avweTS <- ts(avWeekEndSteps$avg)

# plot.ts(avweTS, type="l")
