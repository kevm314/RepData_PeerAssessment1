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

### 2. Strategy for imputing missing values


```r
dat$steps <- as.numeric(is.na(dat$steps))
missing.values <- aggregate(steps ~ date, data=dat, FUN=sum)

barplot(missing.values$steps, names=missing.values$date, main="Missing values per day")
```

![](PA1_template_files/figure-html/missing-1.png)<!-- -->

```r
missing.values <- aggregate(steps ~ interval, data=dat, FUN=sum)
barplot(missing.values$steps, names=missing.values$interval, main="Missing values per interval")
```

![](PA1_template_files/figure-html/missing-2.png)<!-- -->

Most days have no missing values, however, 8 days have over 250 missing values. In comparison, every interval has a very similar number of missing values.

A simple strategy to try will involve replacing a missing value with the mean for that time interval.

### 3. Imputing missing values


```r
dat <- read.csv("data/activity.csv")
dat$date <- as.Date(as.character(dat$date), "%Y-%m-%d")

mean.steps <- aggregate(steps ~ interval, data=dat, FUN=mean, na.rm=T)

dat$steps <- as.numeric(apply(dat, 1, function(x) if(is.na(x["steps"])) {mean.steps[mean.steps$interval == as.numeric(x["interval"]),"steps"]} else {x["steps"]} ))
```

### 4. Histogram with imputed missing values



```r
daily.steps <- aggregate(steps ~ date, data=dat, FUN=sum, na.rm=T)
hist(daily.steps$steps, col="blue", xlab="Steps per day", ylab="Step count", main="")
```

![](PA1_template_files/figure-html/imputed-1.png)<!-- -->

```r
summary(daily.steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```




## Are there differences in activity patterns between weekdays and weekends?
