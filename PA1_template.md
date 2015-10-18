# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Format the date column.

```r
data <- read.csv("activity.csv", header = TRUE)
data$date <- as.Date(data$date, format = "%m/%d/%Y")
```
## What is mean total number of steps taken per day?
For this part of the assignment, we wil ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.
3. Calculate and report the mean and median of the total number of steps taken per day.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
stepsperday <- data %>% 
    filter(!is.na(steps)) %>%
    group_by(date) %>% 
    summarize("totalsteps" = sum(steps))

hist(stepsperday$totalsteps, main = "Histogram of Steps Per Day", 
     xlab = "Total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(stepsperday$totalsteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsperday$totalsteps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsperinterval <- data %>% 
    filter(!is.na(steps)) %>%
    group_by(interval) %>% 
    summarize("meanint"= mean(steps))

plot(stepsperinterval$interval, stepsperinterval$meanint, type = "l",
     xlab = "5 minute interval", ylab = "Number of steps", 
     main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxint <- which.max(stepsperinterval$meanint)
stepsperinterval[[maxint, 1]]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalna <- sum(is.na(data$steps))
mergedata <- merge(data, stepsperinterval)
mergedata$steps[is.na(mergedata$steps)] <- mergedata$meanint[is.na(mergedata$steps)]

replacedsteps <- mergedata %>% group_by(date) %>% summarize("totsteps" = sum(steps))

hist(replacedsteps$totsteps, main = "Histogram of Steps Per Day (with NA's replaced)", 
     xlab = "Total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(replacedsteps$totsteps)
```

```
## [1] 10766.19
```

```r
median(replacedsteps$totsteps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
mergedata$day <- ifelse(grepl('^S', weekdays(mergedata$date)), "weekend", "weekday")

meanperday <- mergedata %>%
    group_by(interval, day) %>%
    summarize("meanday" = mean(steps))

library(ggplot2)
ggplot(meanperday, aes(interval, meanday)) + geom_line() + facet_grid(day~.) + 
    xlab("Interval") + ylab("Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
