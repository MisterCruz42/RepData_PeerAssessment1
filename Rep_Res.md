---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This markdown document contains the answers and r code that are part of Project 1 of the Coursera course Reproducible Research 

###Loading and preprocessing the data

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
setwd("C:/Users/arthur.croes/Documents/Coursera Data Science/5. Reproducible Research")
data<-read.csv("activity.csv")
library(ggplot2)
library(knitr)
```

###Mean and Median steps per day

```r
Summy <- aggregate(steps~date,data, sum)
g<- ggplot(Summy, aes(steps))
g+geom_histogram()+labs(y= "Frequency")+labs(x = "Number of steps per day")+
        ggtitle("Frequency of # of steps per day") 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Rep_Res_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
Med <- median(Summy$steps)
Mea <- mean(Summy$steps)
Med
```

```
## [1] 10765
```

```r
Mea
```

```
## [1] 10766.19
```
Mean number of steps = 10766.19  
Median number of steps = 10765

###Average daily activity pattern

```r
Aver <- aggregate(steps~interval,data, mean)
h <- ggplot(Aver, aes(interval,steps))
h + geom_line(col="lightskyblue4", size=1)+theme_bw()
```

![](Rep_Res_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Aver$interval[Aver$steps==max(Aver$steps)]
```

```
## [1] 835
```
The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset is 835-840

###Imputing Missing Values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304

Create a new dataset with interval mean replacing the missing values

```r
data2 <- data
InMean<- tapply(data2$steps, data2$interval, mean, na.rm=T)
data2$steps[is.na(data2$steps)]<-InMean
```

Histogram of steps per day

```r
Sum2 <- aggregate(steps~date,data2, sum)
i<- ggplot(Sum2, aes(steps))
i+geom_histogram()+labs(y= "Frequency")+labs(x = "Number of steps per day")+
        ggtitle("Frequency of # of steps per day") 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Rep_Res_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Median and mean

```r
Med2 <- median(Sum2$steps)
Mea2 <- mean(Sum2$steps)
Med2
```

```
## [1] 10766.19
```

```r
Mea2
```

```
## [1] 10766.19
```
Mean = 10766.19  
Median = 10766.19

The values don't differ significantly from the first part of the assignment. The impact of imputing missing data is that the frequency of each of the different number of steps per day increases, which does not increase the mean or median.

###Differences activity patterns between weekdays and weekends 
Create factor variable

```r
data2$date <- as.POSIXct.Date(data2$date)
data2$WD <- weekdays(data2$date)
data2$Wday <- "Weekday"
data2$Wday<- replace(data2$Wday, data2$WD=="Sunday"|data2$WD=="Saturday", "Weekend")
```
Panel plot

```r
Avg<-aggregate(steps~interval+Wday,data2, mean)
j<- ggplot(Avg, aes(interval, steps))
j+geom_line(col= "lightskyblue4")+labs(y= "Number of steps")+labs(x = "Interval")+facet_grid(vars(Avg$Wday)) 
```

![](Rep_Res_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


