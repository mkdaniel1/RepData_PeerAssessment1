---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
zipF<- "activity.zip"
unzip(zipF)
active_data <- read.csv("activity.csv", header = TRUE, na.strings = TRUE)
summary(active_data)
```

```
##      steps               date          interval     
##  0      :11014   2012-10-01:  288   Min.   :   0.0  
##  NA     : 2304   2012-10-02:  288   1st Qu.: 588.8  
##  7      :   87   2012-10-03:  288   Median :1177.5  
##  8      :   83   2012-10-04:  288   Mean   :1177.5  
##  15     :   68   2012-10-05:  288   3rd Qu.:1766.2  
##  16     :   65   2012-10-06:  288   Max.   :2355.0  
##  (Other): 3947   (Other)   :15840
```


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
