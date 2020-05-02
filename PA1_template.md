---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
dat <- read.csv("data/activity.csv")
dat$date <- as.Date(as.character(dat$date), "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

### 1. Total number of steps per day

```r
daily.steps <- aggregate(steps ~ date, data=dat, FUN=sum, na.rm=T)
```
### 2. Histogram of the total number of steps taken each day


```r
hist(daily.steps$steps, col="blue", xlab="Steps per day", ylab="Step count", main="")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

### 3. Summary statistics of daily steps


```r
summary(daily.steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?

### 1. 5 minute interval plot against step count across all days


```r
interval.data <- aggregate(steps ~ interval, data=dat, FUN=mean, na.rm=T)
with(interval.data, plot(steps~interval, type="l", col="blue", xlab="Interval Number", ylab="Step count averaged over days", main="Averaged step counts for every time interval"))
```

![](PA1_template_files/figure-html/time.series-1.png)<!-- -->

### 2. 5-minute interval with maximum average step count.


```r
max.interval <- interval.data[which(interval.data$steps == max(interval.data$steps)),]
```

The max interval is interval number 835 having a maximum average step count value of 206.17.

## Imputing missing values

### 1. Total number of missing values in the datase


```r
na.count <- sum(is.na(dat))
```

There are 2304 rows with missing values.

## Are there differences in activity patterns between weekdays and weekends?
