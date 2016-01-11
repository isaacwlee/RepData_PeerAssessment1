# Reproducible Research: Peer Assessment 1

library(lattice)

## Loading and preprocessing the data
activityData <- read.csv('activity.csv')
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")


## What is mean total number of steps taken per day?
totalSteps <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
hist(totalSteps$steps, col = "red", xlab = "Steps", ylab = "Days", main = "")
mean(totalSteps$steps)
median(totalSteps$steps)


## What is the average daily activity pattern?
stepIntervals <- aggregate(steps ~ interval, data = activityData, FUN = mean)
plot(stepIntervals, type = "l", col = "red", xlab = "Interval", ylab = "Steps")
stepIntervals$interval[which.max(stepIntervals$steps)]


## Imputing missing values
sum(is.na(activityData))
activityDataImputed <- activityData
activityDataImputed$date <- as.Date(activityDataImputed$date, "%Y-%m-%d")
activityDataImputed$steps[which(is.na(activityDataImputed$steps))] <- mean(stepIntervals$steps)
totalStepsImputed <- aggregate(steps ~ date, data = activityDataImputed, sum, na.rm = TRUE)
hist(totalStepsImputed$steps, col = "red", xlab = "Steps", ylab = "Days", main = "")
mean(totalStepsImputed$steps)
median(totalStepsImputed$steps)


## Are there differences in activity patterns between weekdays and weekends?
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activityDataImputed$daytype <- as.factor(sapply(activityDataImputed$date, daytype))
stepsWkWknd <- aggregate(steps ~ interval + daytype, data = activityDataImputed, mean)
xyplot(steps ~ interval | daytype, stepsWkWknd, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Steps")

