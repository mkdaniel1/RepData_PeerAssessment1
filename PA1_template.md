---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
unzip("activity.zip")
active_data <- read.csv("activity.csv", header = TRUE, stringsAsFactors=FALSE)
active_data$date <-  as.POSIXct(active_data$date, format="%Y-%m-%d")
dayofweek <- weekdays(active_data$date)
active_data <- cbind(active_data, dayofweek)
summary(active_data)
```

```
##      steps             date                        interval     
##  Min.   :  0.00   Min.   :2012-10-01 00:00:00   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16 00:00:00   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31 00:00:00   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31 00:25:34   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15 00:00:00   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30 00:00:00   Max.   :2355.0  
##  NA's   :2304                                                   
##      dayofweek   
##  Friday   :2592  
##  Monday   :2592  
##  Saturday :2304  
##  Sunday   :2304  
##  Thursday :2592  
##  Tuesday  :2592  
##  Wednesday:2592
```

```r
head(active_data)
```

```
##   steps       date interval dayofweek
## 1    NA 2012-10-01        0    Monday
## 2    NA 2012-10-01        5    Monday
## 3    NA 2012-10-01       10    Monday
## 4    NA 2012-10-01       15    Monday
## 5    NA 2012-10-01       20    Monday
## 6    NA 2012-10-01       25    Monday
```


## What is mean total number of steps taken per day?

### Calculate the number of steps per day

```r
all_steps <- active_data %>% group_by(date) %>% summarise (daily_steps = sum(steps,na.rm=TRUE))
head(all_steps)
```

```
## # A tibble: 6 x 2
##   date                daily_steps
##   <dttm>                    <int>
## 1 2012-10-01 00:00:00           0
## 2 2012-10-02 00:00:00         126
## 3 2012-10-03 00:00:00       11352
## 4 2012-10-04 00:00:00       12116
## 5 2012-10-05 00:00:00       13294
## 6 2012-10-06 00:00:00       15420
```

## Plot number of steps per day

```r
barplot(height = all_steps$daily_steps, names.arg=all_steps$date, cex.names=0.60, las=3, col = "green")
abline(h=median(all_steps$daily_steps),lwd=3, col="blue")
abline(h=mean(all_steps$daily_steps),lwd=3, col="red")
legend("top",  c("blue","red"), c("median", "mean"), fill = c("blue", "red"))
```

![](PA1_template_files/figure-html/plot_steps-1.png)<!-- -->


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
