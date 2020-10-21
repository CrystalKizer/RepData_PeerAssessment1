---
title: "Reproducible Research: Peer Assessment 1"
author: "Crystal Kizer"
date: "October 21, 2020" 
output: 
  html_document:
    keep_md: true
---
Loading and preprocessing the data:


```r
#Loading Data to read file
setwd("C:/Users/AF03941/Documents/Coursera")
activity<-read.csv("activity.csv")
```
Process/transform the data into a format suitable for your analysis

```r
#process data. Review activity file to understand the dataset
dim(activity) 
```

```
## [1] 17568     3
```

```r
names(activity) 
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity) 
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity) 
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#formats date 
activity$date <- as.Date(activity$date, "%Y-%m-%d")
#remove NA from data file
head(activity[which(!is.na(activity$steps)), ]) 
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
##What is mean total number of steps taken per day?



```r
# Calculate the total number of steps taken per day
total_steps <- aggregate(steps ~ date, activity, sum )
names(total_steps)[names(total_steps)=="steps"] <- "total_steps"
 total_steps 
```

```
##          date total_steps
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## 11 2012-10-13       12426
## 12 2012-10-14       15098
## 13 2012-10-15       10139
## 14 2012-10-16       15084
## 15 2012-10-17       13452
## 16 2012-10-18       10056
## 17 2012-10-19       11829
## 18 2012-10-20       10395
## 19 2012-10-21        8821
## 20 2012-10-22       13460
## 21 2012-10-23        8918
## 22 2012-10-24        8355
## 23 2012-10-25        2492
## 24 2012-10-26        6778
## 25 2012-10-27       10119
## 26 2012-10-28       11458
## 27 2012-10-29        5018
## 28 2012-10-30        9819
## 29 2012-10-31       15414
## 30 2012-11-02       10600
## 31 2012-11-03       10571
## 32 2012-11-05       10439
## 33 2012-11-06        8334
## 34 2012-11-07       12883
## 35 2012-11-08        3219
## 36 2012-11-11       12608
## 37 2012-11-12       10765
## 38 2012-11-13        7336
## 39 2012-11-15          41
## 40 2012-11-16        5441
## 41 2012-11-17       14339
## 42 2012-11-18       15110
## 43 2012-11-19        8841
## 44 2012-11-20        4472
## 45 2012-11-21       12787
## 46 2012-11-22       20427
## 47 2012-11-23       21194
## 48 2012-11-24       14478
## 49 2012-11-25       11834
## 50 2012-11-26       11162
## 51 2012-11-27       13646
## 52 2012-11-28       10183
## 53 2012-11-29        7047
```


```r
# Make a histogram of the total number of steps taken each day
sumSteps <- aggregate(activity$steps ~ activity$date, FUN=sum) 
colnames(sumSteps)<- c("Date", "Steps") 
 hist(sumSteps$Steps, breaks=10, xlab="Steps", main = "Total Steps per day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day

```r
#MEAN Steps per day:
as.integer(mean(sumSteps$Steps)) 
```

```
## [1] 10766
```

```r
#MEDIAN Steps per day:
as.integer(median(sumSteps$Steps)) 
```

```
## [1] 10765
```
What is the average daily activity pattern?


```r
#Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

 #calculate mean steps by 5 min interval
x <- aggregate(steps ~ interval, activity, mean, na.rm=TRUE) 

 # plot time series chart and label accordingly
 plot(x, type="l", xlab="5 Min Interval", ylab="AVG Steps Taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# get the interval which has the most steps
x$interval[x$steps==max(x$steps)]
```

```
## [1] 835
```
Imputing missing value:
Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
# create the new dataset to fill in values for NA 
 Fillin <- activity 
# substitute NA with zeros in dataset
 Fillin$steps[is.na(Fillin$steps)] <- 0 
# make sure there are no NAs in dataset
 sum(is.na(Fillin$steps)) 
```

```
## [1] 0
```

```r
# validate the dataset is equal to the original dataset but with the missing data filled in as 0s
sum(!is.na(Fillin$steps)) 
```

```
## [1] 17568
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
 


```r
TotalSteps2 <- aggregate(Fillin$steps ~ Fillin$date, FUN=sum) 
colnames(TotalSteps2)<- c("Date", "Steps") 

  #Create histogram of total number of steps
 hist(TotalSteps2$Steps, breaks=10, xlab="Steps", main = "Total Steps per day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#report the mean and median total number of steps taken per day
#Mean steps per day
as.integer(mean(TotalSteps2$Steps)) 
```

```
## [1] 9354
```

```r
#Median steps per day
as.integer(median(TotalSteps2$Steps)) 
```

```
## [1] 10395
```

```r
#Do these values differ from the estimates from the first part of the assignment? 
#Yes, filling in the missing values to 0 skewed the data closer to 0. We can see there are more 0 values in our historgram.  
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Decreased both mean and median
```
Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
#create weekday and weekend variable 
activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
```
 Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
 #Creating the data set that will be plotted using new weekday and weekend variables
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)


library(ggplot2) 

#Plotting panel plot using ggplot2
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type") 

print(dayPlot) 
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
