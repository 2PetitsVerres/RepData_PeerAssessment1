# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv(file = unzip("activity.zip"))
```


## What is mean total number of steps taken per day?

We start this analyse by plotting an histogram of the number of steps per days, and calculating the mean and median of steps per days.


```r
steps_per_day <- tapply(data$steps, data$date, sum, na.rm = T)
hist(steps_per_day, main = "Steps per day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean_steps_per_days <- mean(steps_per_day)
median_steps_per_days <- median(steps_per_day)
```


The mean number of steps per days, ignoring missing data, is **9354.2295** and the median number is **10395**.



## What is the average daily activity pattern?

Here is the average daily pattern plot.


```r
interval <- unique(data$interval)
steps_per_interval <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(interval, steps_per_interval, type = "l", main = "Steps per interval", 
    xlab = "interval", ylab = "Mean number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
max_interval <- names(which.max(steps_per_interval))
```


The interval **835** is the one with a maximum averaged number of steps.


## Imputing missing values


```r
na_index <- is.na(data$steps)
nb_miss <- sum(na_index)
```

The number of missing values is **2304**. To replace these missing values, I will remplace them by the mean value on the same time interval, across all days.



```r
data.no_na <- data
data.no_na$steps[which(na_index)] <- steps_per_interval[match(data$interval[which(na_index)], 
    interval)]

steps_per_day.no_na <- tapply(data.no_na$steps, data.no_na$date, sum, na.rm = T)
hist(steps_per_day.no_na, main = "Steps per day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
mean_steps_per_days.no_na <- mean(steps_per_day.no_na)
median_steps_per_days.no_na <- median(steps_per_day.no_na)
```



The mean number of steps per days, after remplacing missing data, is **1.0766 &times; 10<sup>4</sup>** and the median number is **1.0766 &times; 10<sup>4</sup>**. We see with this value, and in the histogram, that this did change slightly the data, but not drastically.

## Are there differences in activity patterns between weekdays and weekends?

For this plot, I first create a "weekday"/"weekend" factor on the data, and then plot it.


```r
data.no_na$day <- as.POSIXlt(data.no_na$date, format = "%Y-%m-%d")$wday
data.no_na$day[data.no_na$day == 0] <- "weekend"
data.no_na$day[data.no_na$day == 6] <- "weekend"
data.no_na$day[data.no_na$day != "weekend"] <- "weekday"
data.no_na$day <- factor(data.no_na$day)

steps_per_interval.no_na <- with(data.no_na, tapply(steps, list(interval, day), 
    mean))

par(mfrow = c(2, 1))
with(data.no_na, {
    par(mai = c(0, 1, 1, 0))
    plot(steps_per_interval.no_na[, "weekday"], type = "l", main = ("Steps vs. Interval"), 
        xaxt = "n", ylab = "Weekdays")
    par(mai = c(1, 1, 0, 0))
    plot(steps_per_interval.no_na[, "weekend"], type = "l", xlab = "Interval", 
        ylab = "Weekend")
})
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


We can see on the plot that the steps looks more distributed during weekend than during weekdays.
