##  Reproducible Research : Course Project 1

##  Set working directory
library(ggplot2)
setwd("c:/Users/catherine/projects/RepData_PeerAssessment1")


## 1. Unzip and read data file. Process the data
unzip("activity.zip", list=FALSE, overwrite=TRUE, exdir=".")
myfile <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
## Remove incomplete cases with missing values.
completecases <- myfile[complete.cases(myfile),]
## Compute total steps by day
totalsteps <- with(completecases, aggregate(steps, FUN=sum, 
              by=list(date=as.Date(date, "%Y-%m-%d")), na.rm=TRUE))


## 2. Plot Histogram of Total Steps per day
hist(totalsteps$x, xlab="Total Steps per day", main="Histogram of Total Steps Per Day", ylim=c(0,30))


## 3. Calculate mean and median of total number of steps taken per day
print(meansteps <- round(mean(totalsteps$x)))
print(mediansteps <- round(median(totalsteps$x)))


## 4. Plot Time series plot of the 5-min interval (x-axis) and the average 
##    number of steps taken, averaged across all days (y-axis)
##   Compute average number of steps by intervals
tsteps_interval <- with(completecases, aggregate(steps, FUN=mean, by=list(interval=interval)))
##   Plot time series
with(tsteps_interval, plot(interval, x, type="l",xlab="5-min Interval", ylab="Average Number of Steps", main="Average Number of Steps Taken by Intervals"))


## 5. Determine which 5-min interval, on average across all the days in the 
##    dataset, contains the maximum number of steps
print(tsteps_interval[which.max(tsteps_interval$x),1])


## 6. Impute the missing data by using the mean for that 5-min interval
## Report the Total number of rows with NAs
na_rows <- nrow(myfile) - nrow(completecases)

##This part is commented out#####
## Make a copy of myfile2 including column names
##myfile2 <- data.frame()
##i <-0
##for (i in 1:nrow(myfile)) {
##        myfile2[i,1] <- myfile[i,1]
##        myfile2[i,2] <- myfile[i,2]
##        myfile2[i,3] <- myfile[i,3]
##}
#names(myfile2) <- names(myfile)


## Merge two dataframes (myfile2 and tsteps_interval) by variable Interval
merged <- merge(myfile, tsteps_interval, by="interval")
## Order the data frame by date
myfile3 <- merged[order(merged$date),]
## Impute the missing data with mean for that 5-min interval
for (i in 1:nrow(myfile3)) {
        if (is.na(myfile3[i,"steps"])) {
                myfile3[i,"steps"] <- round(myfile3[i,"x"])
        }
}


## 7. Histogram of the total number of steps taken each day after missing values
##    are imputed
## Total steps by date
totalsteps2 <- with(myfile3, aggregate(steps, FUN=sum, by=list(date=as.Date(date, "%Y-%m-%d"))))
## Plot Histogram 
hist(totalsteps2$x, xlab="Total Steps per day", main="Histogram of Total Steps Per Day", ylim=c(0,35))
## Calculate new mean and median after imputing missing values
print(meansteps2 <- round(mean(totalsteps2$x)))
print(mediansteps2 <- round(median(totalsteps2$x)))


## 8. Panel plot comparing the average number of steps taken per 5-min interval 
##    across weekdays and weekends
##  Create new factor variable: weekday, weekend
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
## Calculate average no of steps taken per 5-min interval on weekdays and weekend 
totalsteps3 <- with(myfile3, aggregate(steps, FUN=mean, 
               by=list(interval=interval, week=week)))
## Plot time series
print(ggplot(totalsteps3,aes(x=interval, y=round(x),color=week)) + geom_line() + 
      facet_grid(week~.) + facet_wrap(~week,nrow=2,strip.position="top") +
      theme(strip.background = element_rect(fill="cyan")) +
      ylab("Average Number of Steps")) 
