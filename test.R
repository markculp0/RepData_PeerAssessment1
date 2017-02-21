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

# ---
# 2.Av Daily Activity Pattern

# Grouped - Not needed
byDate <- group_by(act, date)
dailyAv <- mutate(byDate, dailyTotal=cumsum(steps), dailyCnt=cumsum(steps>=0))

# Ungrouped
act_NoNA <- filter(act, !is.na(steps))
allDayAv <- mutate(act_NoNA, avSteps=cumsum(steps), dailyCnt=cumsum(steps>=0), avg=avSteps/dailyCnt)


#---
# 3. Imputed values


library(plyr)

# Fill in missing daily mean calculations with mean over all days
meanStepPerDay$mean.steps[is.na(meanStepPerDay$mean.steps)] = mean(meanStepPerDay$mean.steps, na.rm = T)

# Add column with daily mean values to new activity data frame
act_imputed <- arrange(join(act,meanStepPerDay), date)

# Fill NA step values with daily mean values
act_imputed$steps[is.na(act_imputed$steps)] = act_imputed$mean.steps


#-
# 4. Weekday and Weekend comparisons


act_imputed$weekday <- weekdays(act_imputed$date)
act_imputed$weekday  <- ifelse(act_imputed$weekday %in% c("Sunday","Saturday"), "weekend", "weekday")

act_weekend <- filter(act_imputed, weekday == "weekend")

avWeekEndSteps <- mutate(act_weekend, avSteps=cumsum(steps), dailyCnt=cumsum(steps>=0), avg=avSteps/dailyCnt)


