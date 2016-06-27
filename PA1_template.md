# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```


## What is mean total numbers of steps taken per day?
(Ignore missing values in the dataset.)

1. Calculate the total number steps taken per day

```r
stepsbyday<-aggregate(steps~date, activity, na.action = na.omit, FUN = sum)
```

2. Make a histogram of the total number of steps taken each day

```r
barplot(stepsbyday$steps, names.arg = stepsbyday$date, xlab = "Date", ylab = "Steps")
```

![plot of chunk Make a histogram](figure/Make a histogram-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsbyday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsbyday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
meanstepsbyinterval<-aggregate(steps~interval, activity, na.action = na.omit, FUN = mean)
plot(meanstepsbyinterval, type = "l")
```

![plot of chunk Calulate Average Number Steps in intervals](figure/Calulate Average Number Steps in intervals-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanstepsbyinterval[which.max(meanstepsbyinterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
**I will use average number steps for 5-minutes intervals.**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity <- merge(activity, meanstepsbyinterval, by = "interval", suffixes = c("","-m"))
naa<-is.na(activity$steps)
activity$steps[naa]<-activity$`steps-m`[naa]
activity <- activity[,1:3]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsbyday<-aggregate(steps~date, activity, FUN = sum)
barplot(stepsbyday$steps, names.arg = stepsbyday$date, xlab = "Date", ylab = "Steps")
```

![plot of chunk Make a histogram and calculate mean and median of new activity](figure/Make a histogram and calculate mean and median of new activity-1.png)

```r
mean(stepsbyday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsbyday$steps)
```

```
## [1] 10766.19
```
Impact of imputing missing data on the estimates of the total daily number of steps is **low**.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dayofweek <- function(date) {
  if (weekdays(as.Date(date)) %in% c("sobota", "niedziela"))
       "weekend"
  else "weekday"
}
activity$dayofweek <- as.factor(sapply(activity$date, dayofweek))
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsbydayofweek<-aggregate(steps~interval+dayofweek, activity, FUN = mean)
xyplot(steps ~ interval | dayofweek, stepsbydayofweek, type = "l", layout = c(1, 2),
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk Make a plot weekends and weekdays](figure/Make a plot weekends and weekdays-1.png)
