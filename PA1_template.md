# Reproducible Research: Peer Assessment 1 
## Loading and preprocessing the data

### Library

```r
library(downloader)
library(knitr)
library(lattice)
```


### Loading

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip.FileName <- "activity.zip"
download(fileUrl, zip.FileName, mode = "wb")
extracted.filename <- unzip(zip.FileName)
activityData <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
```


### Preprocessing the data

```r
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")  #Convert date to column with date type
```



## What is mean total number of steps taken per day?

### Histogram of the total number of steps taken each day

```r
totStepsByDay <- aggregate(steps ~ date, data = activityData, sum)  #Total steps by day

hist(totStepsByDay$steps, main = "Histogram Total Steps per Day", xlab = "total number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### The mean and median total number of steps taken per day

```r
mean(totStepsByDay$steps)
```

```
## [1] 10766
```

```r
median(totStepsByDay$steps)
```

```
## [1] 10765
```


* The MEAN total number of steps by day is 1.0766 &times; 10<sup>4</sup> steps.
* The MEDIAN total number of steps taken per day is 10765 steps.    

## What is the average daily activity pattern?

### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r

meanStepsByInt <- aggregate(steps ~ interval, data = activityData, mean, na.rm = TRUE)  #Mean by Interval
names(meanStepsByInt)[2] <- "meanSteps"


plot(meanStepsByInt$interval, meanStepsByInt$meanSteps, type = "n", main = "Plot: Time Series per 5-minute interval", 
    xlab = "5-minute intervals", ylab = "Average number of steps taken")
lines(meanStepsByInt$interval, meanStepsByInt$meanSteps, type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanStepsByInt[which.max(meanStepsByInt$meanSteps), ]$interval
```

```
## [1] 835
```


* The maximum number of steps = 835

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

How many missing value are there in the dataset?

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

Total number of missing values in the dataset: `r sum(is.na(activityData$steps))

### Devise a strategy for filling in all of the missing values in the dataset. 

### Use dataframe meanStepsByInt with mean steps per interval to merge it with original dataset


```r
activityDataFilled <- merge(activityData, meanStepsByInt, by = "interval", sort = FALSE)  # merge activityData and meanStepsByInt dataframes

### replace steps column == NA with value in meanSteps column
activityDataFilled$steps[is.na(activityDataFilled$steps)] <- activityDataFilled$meanSteps[is.na(activityDataFilled$steps)]
activityDataFilled$stMean <- NULL  # remove the column with the mean since it is no longer needed
```



### Verifying that no more NA are present in the dataset

* Total number of missing values yet in the filled dataset: 0


### Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Histogram of the total number of steps taken each day


```r

totStepsByDayFilled <- aggregate(steps ~ date, data = activityDataFilled, sum)
hist(totStepsByDayFilled$steps, main = "Total number of steps per day", xlab = "total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


### The mean and median total number of steps taken per day


```r
mean(totStepsByDayFilled$steps)
```

```
## [1] 10766
```

```r
median(totStepsByDayFilled$steps)
```

```
## [1] 10766
```


* The MEAN total number of steps by day is 1.0766 &times; 10<sup>4</sup> steps.
* The MEDIAN total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> steps.    

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
Sys.setlocale(locale = "C")  # set the locale in RStudio
```

```
## [1] "C"
```

```r

activityDataFilled$weekDays <- weekdays(activityDataFilled$date)

activityDataFilled$wdWeek <- as.factor(ifelse(activityDataFilled$weekDays %in% 
    c("Saturday", "Sunday"), "weekend", "weekday"))


meanStepsByDayFilled <- aggregate(activityDataFilled[, c("steps"), drop = FALSE], 
    list(interval = activityDataFilled$interval, wdWeek = activityDataFilled$wdWeek), 
    mean, na.rm = TRUE)
```


### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
xyplot(steps ~ interval | wdWeek, data = meanStepsByDayFilled, type = "l", layout = c(1, 
    2), main = "", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

