---
output: 
  html_document: 
    keep_md: yes
---
Reproducible Research Course Project 1
-------------------------------------------------------------------------
title: "Reproducible Research Course Project 1"
output: html_document
-------------------------------------------------------------------------



## Step 1: Code for reading dataset and processing data

```r
        if (!file.exists("activity.csv")) {
                unzip("activity.zip")
        }
        cls = c("integer", "character", "integer")        
        activityData <- read.csv("activity.csv", header=TRUE, colClasses=cls,na.strings = "NA")
        activityData$date <- as.Date(activityData$date)
```

## Step 2: Histogram of number of steps taken each day

```r
        steps_by_day <- aggregate(steps ~ date, na.rm=TRUE, activityData, sum)
        plot(steps_by_day, type="h", lwd=10, lend="square", main="Histogram - Total Number of Steps taken Each Day",xlab="Day", ylab="Number of Steps", col="blue")
```

![](PA1_template_files/figure-html/hist_num_steps-1.png)<!-- -->

## Step 3a: Mean number of steps taken each day

```r
        paste("Mean number of steps per Day = ", mean(steps_by_day$steps, na.rm=TRUE))   
```

```
## [1] "Mean number of steps per Day =  10766.1886792453"
```

## Step 3b: Median number of steps taken each day

```r
        paste("Median number of steps per Day = ", median(steps_by_day$steps, na.rm=TRUE))        
```

```
## [1] "Median number of steps per Day =  10765"
```

## Step 4: Time series plot for average number of steps taken (across all days)

```r
        steps_by_interval <- aggregate(steps ~ interval, na.rm=TRUE, data=activityData, FUN=mean)
        plot(steps_by_interval$interval, steps_by_interval$steps, type="l", main = "Time series plot for average number of steps taken", xlab= " 5-minute interval", ylab=" avg number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Step 5: Which 5-minute interval, on an average, contains maximum number of steps (across all days)

```r
        steps_by_interval[which.max(steps_by_interval$steps),1]
```

```
## [1] 835
```

## Step 6: Code to describe and show a strategy for imputing missing data
### Impute Strategy : fill the missing data for steps, each by populating the average number of steps for that corresponding interval for all days
### Then, validate that the Impute worked by counting the number of NA's

```r
        paste("Total number of Rows with missing Steps data (\"NA\")\":", sum(is.na(activityData$steps)))   
```

```
## [1] "Total number of Rows with missing Steps data (\"NA\")\": 2304"
```

```r
        incomplete <- sum(!complete.cases(activityData))
    
        imputed_data <- transform(activityData, steps=ifelse(is.na(activityData$steps), steps_by_interval$steps[match(activityData$interval, steps_by_interval$interval)], activityData$steps))
        
        paste("Total number of Rows with missing Steps data (\"NA\")\" after the Impute process:", sum(is.na(imputed_data$steps)))  
```

```
## [1] "Total number of Rows with missing Steps data (\"NA\")\" after the Impute process: 0"
```


## Step 7: Histogram of the total number of steps taken each day after missing values are imputed

```r
        steps_by_day_after_impute <- aggregate(steps ~ date, imputed_data, sum)
        plot(steps_by_day_after_impute, type="h" ,lwd=10, lend="square", main="Histogram - Total Number of Steps taken Each Day after Impute",xlab="Day", ylab="Number of Steps", col="green")
```

![](PA1_template_files/figure-html/hist_num_steps_after_impute-1.png)<!-- -->

## Step 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
        weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
        imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)), weekdays), "Weekday", "Weekend"))
        
        steps_by_interval_i <- aggregate(steps ~ interval, data=imputed_data, FUN=mean)

        ggplot(imputed_data, aes(x =interval , y=steps, color=dow)) + geom_line() + labs(title = "Average Steps per Day by Interval", x = "5-minute interval", y = "avg number of steps") +  facet_wrap(~ dow, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
