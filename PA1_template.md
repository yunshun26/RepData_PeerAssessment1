Reproducible Research - Course Project 1
========================================
        
        
This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals throughout the day during the months of October and November 2012. 


## Loading and preprocessing the data 



Here we are going to load some data:

```r
unzip("activity.zip", list=FALSE, overwrite=TRUE, exdir=".")
myfile <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
completecases <- myfile[complete.cases(myfile),]
```


## What is mean total number of steps taken per day?
Total number of steps taken per day

```r
totalsteps <- with(completecases, aggregate(steps, FUN=sum, by=list(date=as.Date(date, "%Y-%m-%d")), na.rm=TRUE))
```


Histogram

```r
hist(totalsteps$x, xlab="Total Steps per day", main="Histogram of Total Steps Per Day", ylim=c(0,30))
```

![](c:/Users/catherine/projects/RepData_PeerAssessment1/figures/Histogram1-1.png)<!-- -->


Mean of the total number of steps taken per day

```r
print(meansteps <- round(mean(totalsteps$x)))
```

```
## [1] 10766
```


Median of the total number of steps taken per day

```r
print(mediansteps <- round(median(totalsteps$x)))
```

```
## [1] 10765
```


## What is the average daily activity pattern
Plot Time series plot of the 5-min interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
tsteps_interval <- with(completecases, aggregate(steps, FUN=mean, by=list(interval=interval)))
with(tsteps_interval, plot(interval, x, type="l",xlab="5-min Interval", ylab="Average Number of Steps", 
main="Average Number of Steps Taken by Intervals"))
```

![](c:/Users/catherine/projects/RepData_PeerAssessment1/figures/Plot1-1.png)<!-- -->


Determine which 5-min interval, on average across all the days in the dataset, contains the maximum number of steps

```r
print(tsteps_interval[which.max(tsteps_interval$x),1])
```

```
## [1] 835
```

## Impute missing values
The strategy used here for imputing missing values is to replace variable steps with 'NA' values by the mean for that 5-min interval.

Firstly, we calculate the Total number of rows with NAs

```r
na_rows <- nrow(myfile) - nrow(completecases)
```

Secondly, merge and order dataset 

```r
## Merge two dataframes (myfile1 and tsteps_interval) by variable Interval
merged <- merge(myfile, tsteps_interval, by="interval")
## Order the data frame by date
myfile3 <- merged[order(merged$date),]
for (i in 1:nrow(myfile3)) {
        if (is.na(myfile3[i,"steps"])) {
                myfile3[i,"steps"] <- round(myfile3[i,"x"])
        }
}
```


Plot Histogram

```r
## Total steps by date
totalsteps2 <- with(myfile3, aggregate(steps, FUN=sum, by=list(date=as.Date(date, "%Y-%m-%d"))))
## Plot Histogram 
hist(totalsteps2$x, xlab="Total Steps per day", main="Histogram of Total Steps Per Day (After Imputing Missing Values)", ylim=c(0,35))
```

![](c:/Users/catherine/projects/RepData_PeerAssessment1/figures/Histogram2-1.png)<!-- -->


Mean of Total number of steps taken per day

```r
print(meansteps2 <- round(mean(totalsteps2$x)))
```

```
## [1] 10766
```


Median of Total number of steps taken per day

```r
print(mediansteps2 <- round(median(totalsteps2$x)))
```

```
## [1] 10762
```

Comparing the Mean and Median before and after imputing the values, we noticed that the mean remains the same at 10766 steps. This means that imputing the missing values using the mean steps for that 5-min interval did not change the average number of steps taken.  As for the median, we see a slight decrease from 10765 to 10762 as there is a slight increase in the count in the region of 1000-15000 total steps after missing values are imputed (refer to two histograms before and after imputation). Hence, we infer that there is **minimal impact** in imputing missing data on the estimates of the total daily number of steps in this case.

## Are there differences in activity patterns between weekdays and weekends?

Firstly, let's create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend.

```r
we <- c("Saturday","Sunday")
for (i in 1:nrow(myfile3))
        {
        if (weekdays(as.Date(myfile3[i,"date"])) %in% we)
        {
        myfile3[i,5] <- "weekend"
        } else {
        myfile3[i,5] <- "weekday"
        }
}
colnames(myfile3)[5] <- "week"
myfile3[,5] <- as.factor(myfile3[,5])
```


Secondly, make a panel plot of the 5-miniute interval (x-axis) and the average number of steps taken, averaged across all weekday days (y-axis).

```r
library(ggplot2)
## Calculate average no of steps taken per 5-min interval on weekdays and weekend 
totalsteps3 <- with(myfile3, aggregate(steps, FUN=mean, 
               by=list(interval=interval, week=week)))
## Plot time series
print(ggplot(totalsteps3,aes(x=interval, y=round(x),color=week)) + geom_line() + 
      facet_grid(week~.) + facet_wrap(~week,nrow=2,strip.position="top") +
      theme(strip.background = element_rect(fill="cyan")) +
      ylab("Average Number of Steps")) 
```

![](c:/Users/catherine/projects/RepData_PeerAssessment1/figures/plot2-1.png)<!-- -->

