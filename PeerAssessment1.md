# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
>Show any code that is needed to
 1. Load the data (i.e. read.csv())
 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
originalData <- read.csv("activity.csv")
```


A portion of the original dataset is as follows:

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## 11    NA 2012-10-01       50
## 12    NA 2012-10-01       55
## 13    NA 2012-10-01      100
## 14    NA 2012-10-01      105
## 15    NA 2012-10-01      110
## 16    NA 2012-10-01      115
## 17    NA 2012-10-01      120
## 18    NA 2012-10-01      125
## 19    NA 2012-10-01      130
## 20    NA 2012-10-01      135
```


## What is mean total number of steps taken per day?
>For this part of the assignment, you can ignore the missing values in the dataset.
 1. Make a histogram of the total number of steps taken each day
 2. Calculate and report the mean and median total number of steps taken per day

The goal of this section is to generate the overall mean (average) of the total number of steps taken per day.
There are a few steps taken to reach this goal.

1. A dataset containing the total number of steps taken each day is created.

  
  ```r
  dailyStepSum <- aggregate(originalData$steps, list(originalData$date), sum)
  ```

   A portion of the new dataset is as follows:
  
  ```
  ##          Date Steps
  ## 1  2012-10-01    NA
  ## 2  2012-10-02   126
  ## 3  2012-10-03 11352
  ## 4  2012-10-04 12116
  ## 5  2012-10-05 13294
  ## 6  2012-10-06 15420
  ## 7  2012-10-07 11015
  ## 8  2012-10-08    NA
  ## 9  2012-10-09 12811
  ## 10 2012-10-10  9900
  ## 11 2012-10-11 10304
  ## 12 2012-10-12 17382
  ## 13 2012-10-13 12426
  ## 14 2012-10-14 15098
  ## 15 2012-10-15 10139
  ## 16 2012-10-16 15084
  ## 17 2012-10-17 13452
  ## 18 2012-10-18 10056
  ## 19 2012-10-19 11829
  ## 20 2012-10-20 10395
  ```


2. A histogram of the above data is created as a form of visual representation.

  
  ```r
  with(dailyStepSum, {
      par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
      barplot(
        height=Steps,
        main="Graph of Total Steps taken per Day",
        xlab="Dates",
        ylab="Steps per Day",
        names.arg=Date,
        space=c(0)
      )
  })
  ```
  
  ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


3. Calculate the mean and median values (ignoring NA values) using the above dataset.

  1. Mean
      
      ```r
      dailyStepMean <- mean(dailyStepSum$Steps, na.rm = TRUE)
      ```

      
      ```
      ## [1] 10766
      ```

  2. Median
      
      ```r
      dailyStepMedian <- median(dailyStepSum$Steps, na.rm = TRUE)
      ```

      
      ```
      ## [1] 10765
      ```


## What is the average daily activity pattern?
>What is the average daily activity pattern?
 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

This goal of this section is to find the mean (average) steps taken for eatch 5-minute time interval averaged over all the days in the data.
Similar to the previous section, the steps taken to reach the above goal are as follows:

1. Generate the mean (average) number of steps taken (ignoring NA values) for each 5-minute interval, itself averaged across all days.
  
  
  ```r
  intervalSteps <- aggregate(
      data=originalData,
      steps~interval,
      FUN=mean,
      na.action=na.omit
  )
  colnames(intervalSteps) <- c("Interval", "AvgStepsAvgAcrossDay")
  ```

   A portion of the new dataset is as follows:
  
  ```
  ##    Interval AvgStepsAvgAcrossDay
  ## 1         0              1.71698
  ## 2         5              0.33962
  ## 3        10              0.13208
  ## 4        15              0.15094
  ## 5        20              0.07547
  ## 6        25              2.09434
  ## 7        30              0.52830
  ## 8        35              0.86792
  ## 9        40              0.00000
  ## 10       45              1.47170
  ## 11       50              0.30189
  ## 12       55              0.13208
  ## 13      100              0.32075
  ## 14      105              0.67925
  ## 15      110              0.15094
  ## 16      115              0.33962
  ## 17      120              0.00000
  ## 18      125              1.11321
  ## 19      130              1.83019
  ## 20      135              0.16981
  ```

  
2. A Time-Series plot is created from the above dataset

  
  ```r
  with(intervalSteps, {
      plot(
        x=Interval,
        y=AvgStepsAvgAcrossDay,
        type="l",
        main="Time-Series of Average Steps against Interval",
        xlab="5-minute Interval",
        ylab="Average Steps, Average across all Days"
        
      )
  })
  ```
  
  ![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

  
3. Finding the 5-minute interval with the maximum number of steps

  
  ```r
  intervalMax <- intervalSteps[intervalSteps$AvgStepsAvgAcrossDay==max(intervalSteps$AvgStepsAvgAcrossDay),]
  ```

  
  ```
  ##     Interval AvgStepsAvgAcrossDay
  ## 104      835                206.2
  ```

  Therefore, the interval between **835** and  **840** minutes has the maximum number of steps.


## Imputing missing values
>Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The goal of this section is to generate a new graph using the same data as from the first section but with its NA values replaced.

To achieve this goal the mean (average) 5-minunte interval values as from the previous section will be used to replace the NA values.

1. Total number of rows with NA values in original data.

  
  ```r
  countNA <- nrow(subset(originalData, is.na(originalData$steps)))
  ```

  
  ```
  ## [1] 2304
  ```


2. The average 5-minute interval values from the prevous section is used to replace the NA values of the original data and a new dataset will be generated from the latter.

 Decimal values will be rounded up to a whole number.
 
  
  ```r
  stepValues <- data.frame(originalData$steps)
  stepValues[is.na(stepValues),] <- ceiling(tapply(X=originalData$steps,INDEX=originalData$interval,FUN=mean,na.rm=TRUE))
  newData <- cbind(stepValues, originalData[,2:3])
  colnames(newData) <- c("Steps", "Date", "Interval")
  ```

  
  A portion of the new dataset is as follows:
  
  ```
  ##    Steps       Date Interval
  ## 1      2 2012-10-01        0
  ## 2      1 2012-10-01        5
  ## 3      1 2012-10-01       10
  ## 4      1 2012-10-01       15
  ## 5      1 2012-10-01       20
  ## 6      3 2012-10-01       25
  ## 7      1 2012-10-01       30
  ## 8      1 2012-10-01       35
  ## 9      0 2012-10-01       40
  ## 10     2 2012-10-01       45
  ## 11     1 2012-10-01       50
  ## 12     1 2012-10-01       55
  ## 13     1 2012-10-01      100
  ## 14     1 2012-10-01      105
  ## 15     1 2012-10-01      110
  ## 16     1 2012-10-01      115
  ## 17     0 2012-10-01      120
  ## 18     2 2012-10-01      125
  ## 19     2 2012-10-01      130
  ## 20     1 2012-10-01      135
  ```


3. The total number of steps taken each day is generated using this new dataset.

  
  ```r
  newDailyStepSum <- aggregate(newData$Steps, list(newData$Date), sum)
  ```

   A portion of the new dataset is as follows:
  
  ```
  ##          Date Steps
  ## 1  2012-10-01 10909
  ## 2  2012-10-02   126
  ## 3  2012-10-03 11352
  ## 4  2012-10-04 12116
  ## 5  2012-10-05 13294
  ## 6  2012-10-06 15420
  ## 7  2012-10-07 11015
  ## 8  2012-10-08 10909
  ## 9  2012-10-09 12811
  ## 10 2012-10-10  9900
  ## 11 2012-10-11 10304
  ## 12 2012-10-12 17382
  ## 13 2012-10-13 12426
  ## 14 2012-10-14 15098
  ## 15 2012-10-15 10139
  ## 16 2012-10-16 15084
  ## 17 2012-10-17 13452
  ## 18 2012-10-18 10056
  ## 19 2012-10-19 11829
  ## 20 2012-10-20 10395
  ```


4. A histogram of the above data is created as a form of visual representation.

  
  ```r
  with(newDailyStepSum, {
      par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
      barplot(
        height=Steps,
        main="Graph of Total Steps taken per Day",
        xlab="Dates",
        ylab="Steps per Day",
        names.arg=Date,
        space=c(0)
      )
  })
  ```
  
  ![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 


5. Calculate the mean and median values of this new dataset (NA values replaced with mean).

  1. Mean
      
      ```r
      newDailyStepMean <- mean(newDailyStepSum$Steps)
      ```

      
      ```
      ## [1] 10785
      ```

  2. Median
      
      ```r
      newDailyStepMedian <- median(newDailyStepSum$Steps)
      ```

      
      ```
      ## [1] 10909
      ```

      
6. It seems that adding the missing values to the original data has caused both the mean and median values to increase.

  1. Mean:
  
      10766 to 10784
  2. Median:
  
      10765 to 10909


## Are there differences in activity patterns between weekdays and weekends?
>For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

1.  A new column indicating whether the date is a weekday or a weekend is added to the new dataset created in the previous section.

  
  ```r
  dateDayType <- data.frame(sapply(X = newData$Date, FUN = function(day) {
      if (weekdays(as.Date(day)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
          "Friday")) {
          day <- "weekday"
      } else {
          day <- "weekend"
      }
  }))
  
  newDataWithDayType <- cbind(newData, dateDayType)
  
  colnames(newDataWithDayType) <- c("Steps", "Date", "Interval", "DayType")
  ```

  
   A portion of this dataset is as follows:
  
  ```
  ##    Steps       Date Interval DayType
  ## 1      2 2012-10-01        0 weekday
  ## 2      1 2012-10-01        5 weekday
  ## 3      1 2012-10-01       10 weekday
  ## 4      1 2012-10-01       15 weekday
  ## 5      1 2012-10-01       20 weekday
  ## 6      3 2012-10-01       25 weekday
  ## 7      1 2012-10-01       30 weekday
  ## 8      1 2012-10-01       35 weekday
  ## 9      0 2012-10-01       40 weekday
  ## 10     2 2012-10-01       45 weekday
  ## 11     1 2012-10-01       50 weekday
  ## 12     1 2012-10-01       55 weekday
  ## 13     1 2012-10-01      100 weekday
  ## 14     1 2012-10-01      105 weekday
  ## 15     1 2012-10-01      110 weekday
  ## 16     1 2012-10-01      115 weekday
  ## 17     0 2012-10-01      120 weekday
  ## 18     2 2012-10-01      125 weekday
  ## 19     2 2012-10-01      130 weekday
  ## 20     1 2012-10-01      135 weekday
  ```

2. The data is then separated into weekday or weekend and the mean (average) number of steps taken for each 5-minute interval, itself averaged across all weekday days or weekend days is calculated.

  
  ```r
  dayTypeIntervalSteps <- aggregate(
      data=newDataWithDayType,
      Steps ~ DayType + Interval,
      FUN=mean
  )
  ```

   A portion of the dataset is as follows:
  
  ```
  ##    DayType Interval  Steps
  ## 1  weekday        0 2.2889
  ## 2  weekend        0 0.2500
  ## 3  weekday        5 0.5333
  ## 4  weekend        5 0.1250
  ## 5  weekday       10 0.2889
  ## 6  weekend       10 0.1250
  ## 7  weekday       15 0.3111
  ## 8  weekend       15 0.1250
  ## 9  weekday       20 0.2222
  ## 10 weekend       20 0.1250
  ## 11 weekday       25 1.7111
  ## 12 weekend       25 3.6250
  ## 13 weekday       30 0.7556
  ## 14 weekend       30 0.1250
  ## 15 weekday       35 1.1556
  ## 16 weekend       35 0.1250
  ## 17 weekday       40 0.0000
  ## 18 weekend       40 0.0000
  ## 19 weekday       45 1.8667
  ## 20 weekend       45 0.6250
  ```


3. Finally, a panel plot of both weekend and weekday graphs is generated.

  
  ```r
  library("lattice")
  
  xyplot(
      type="l",
      data=dayTypeIntervalSteps,
      Steps ~ Interval | DayType,
      xlab="Interval",
      ylab="Number of steps",
      layout=c(1,2)
  )
  ```
  
  ![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 

