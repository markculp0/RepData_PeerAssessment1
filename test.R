# Reproducible Research: Peer Assessment 1
# Test Script


# unzip("activity.zip", exdir = "data")
act <- read.csv("data/activity.csv", header = T)
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")

library(dplyr)

# ---
# 1. Mean Total Steps

meanStepPerDay <- aggregate(act[,1], list(act$date), mean)
names(meanStepPerDay) <- c("date","mean.steps")
totalStepPerDay <- aggregate(act[,1], list(act$date), sum)
names(totalStepPerDay) <- c("date", "total.steps")

act_filtered <- filter(act, steps!= 0 & !is.na(steps))
medianStepPerDay <- aggregate(act_filtered[,1], list(act_filtered$date), median)
names(medianStepPerDay) <- c("date","median.steps")

hist(totalStepPerDay$total.steps, xlab = "Total Steps Per Day")

# ---
# 2.Av Daily Activity Pattern

# Grouped - Not needed
# byDate <- group_by(act, date)
# dailyAv <- mutate(byDate, dailyTotal=cumsum(steps), dailyCnt=cumsum(steps>=0))


# 5 minute interval means
byInterval <- group_by(act, interval)
intervalAv <- summarize(byInterval, intrvalMean=mean(steps, na.rm = T))


# Ungrouped, dailyCnt s/b?? interval * (as.numeric(act[4454,2] - act[1,2]))
# act_NoNA <- filter(act, !is.na(steps))
# allDayAv <- mutate(act_NoNA, avSteps=cumsum(steps), dailyCnt=(cumsum(steps>=0) - 1) * 5, avg=avSteps/dailyCnt)

#  allDayAv$dateInterval <- paste(allDayAv$date,allDayAv$interval)

# adaTS <- ts(allDayAv$avg)
iaTS <- ts(intervalAv$intrvalMean)

plot.ts(iaTS, type="l")

avgMax <- which.max(allDayAv$avg)
allDayAv[avgMax,c(2,3,6)]



#---
# 3. Imputed values


library(plyr)

# Fill in missing daily mean calculations with mean over all days
meanStepPerDay$mean.steps[is.na(meanStepPerDay$mean.steps)] = mean(meanStepPerDay$mean.steps, na.rm = T)

# Add column with daily mean values to new activity data frame
act_imputed <- arrange(join(act,meanStepPerDay), date)

# Fill NA step values with daily mean values
act_imputed$steps[is.na(act_imputed$steps)] = act_imputed$mean.steps

totalStepPerDay2 <- aggregate(act_imputed[,1], list(act_imputed$date), sum)
names(totalStepPerDay2) <- c("date", "total.steps")

hist(totalStepPerDay2$total.steps)

meanStepPerDay2 <- aggregate(act_imputed[,1], list(act_imputed$date), mean)
names(meanStepPerDay2) <- c("date","mean.steps")

act_filtered2 <- filter(act_imputed, steps!= 0 & !is.na(steps))
medianStepPerDay2 <- aggregate(act_filtered2[,1], list(act_filtered2$date), median)
names(medianStepPerDay2) <- c("date","median.steps")

#-
# 4. Weekday and Weekend comparisons


act_imputed$weekday <- weekdays(act_imputed$date)
act_imputed$weekday  <- ifelse(act_imputed$weekday %in% c("Sunday","Saturday"), "weekend", "weekday")

act_weekend <- filter(act_imputed, weekday == "weekend")

avWeekEndSteps <- mutate(act_weekend, avSteps=cumsum(steps), dailyCnt=cumsum(steps>=0), avg=avSteps/dailyCnt)

avweTS <- ts(avWeekEndSteps$avg)

plot.ts(avweTS, type="l")
