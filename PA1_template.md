# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

###1.Load the data

```r
library(lattice)
activityData <- read.csv('activity.csv')
```

###2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

###1.Calculate the total number of steps taken per day

```r
totalSteps <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
```

###2.Make a histogram of the total number of steps taken each day

```r
hist(totalSteps$steps, col = "red", xlab = "Steps", ylab = "Days", main = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

###3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###1.Calculate and report the mean and median of the total number of steps taken per day

```r
stepIntervals <- aggregate(steps ~ interval, data = activityData, FUN = mean)

plot(stepIntervals, type = "l", col = "red", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\
###2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepIntervals$interval[which.max(stepIntervals$steps)]
```

```
## [1] 835
```

## Imputing missing values

###1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activityData))
```

```
## [1] 2304
```

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy here is to fill the missing values with mean value of total steps taken per day using the aggregation calculated above stepIntervals

###3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityDataImputed <- activityData

activityDataImputed$date <- as.Date(activityDataImputed$date, "%Y-%m-%d")

activityDataImputed$steps[which(is.na(activityDataImputed$steps))] <- mean(stepIntervals$steps)
```
###4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalStepsImputed <- aggregate(steps ~ date, data = activityDataImputed, sum, na.rm = TRUE)

hist(totalStepsImputed$steps, col = "red", xlab = "Steps", ylab = "Days", main = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\

```r
mean(totalStepsImputed$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsImputed$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

###1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}

activityDataImputed$daytype <- as.factor(sapply(activityDataImputed$date, daytype))
```
###2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
stepsWkWknd <- aggregate(steps ~ interval + daytype, data = activityDataImputed, mean)

xyplot(steps ~ interval | daytype, stepsWkWknd, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)\
