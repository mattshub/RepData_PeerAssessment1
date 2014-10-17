# Reproducible Research: Peer Assessment 1

This report analyzes the data collected from a personal activity monitoring device. The data was collected at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


```r
setwd("/Users/matt/05datascience/DS/coursera/repdata-007/assign1/RepData_PeerAssessment1")
```

## Loading and preprocessing the data

```r
  actData <- read.csv("activity.csv")
  summary(actData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
  str(actData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
  actCleanData <- actData[!is.na(actData$steps),]
  str(actCleanData)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## What is mean total number of steps taken per day?
#### Histogram of Steps taken Daily

```r
splitByDate<-split(actCleanData,actCleanData$date, drop=TRUE)  
dailySteps<-sapply(splitByDate, function(x) sum(x$steps))  
hist(dailySteps, main="Total Steps per Day", xlab="# of Steps", col="blue")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
##summary(dailySteps)
##str(dailySteps)
```
#### Mean of Steps taken Daily

```r
mean(as.numeric(dailySteps))                  
```

```
## [1] 10766.19
```
#### Median of Steps taken Daily

```r
median(dailySteps)                    
```

```
## [1] 10765
```
## What is the average daily activity pattern?
#### Time Series

```r
splitByInterval <- split(actCleanData,actCleanData$interval, drop=TRUE)     
intervalMean <- sapply(splitByInterval, function(x) mean(x$steps))
str(intervalMean)
```

```
##  Named num [1:288] 1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "names")= chr [1:288] "0" "5" "10" "15" ...
```

```r
plot(intervalMean, type="l",  
     main="5 Minute Interval Time Series", 
     ylab="Mean # of Steps", 
     xlab="Intervals", col="red")                          
abline(v=which.max(intervalMean), lty=2, col="blue")                   
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### 5 Minutes Interval with Max # of Steps 

```r
mx <- names(which.max(intervalMean))
print(mx)
```

```
## [1] "835"
```
## Imputing missing values

#### Number of rows with missing value

```r
nrow(actData[is.na(actData$steps),])
```

```
## [1] 2304
```


```r
newData <- actData
for (rownum in 1:nrow(newData)){
  row <- newData[rownum,]
  if (is.na(row$steps))
    {
      meanVal <- intervalMean[as.character(row$interval)]
      if (length(meanVal)==0)
        {
          meanVal <- 0
        }
      newData[rownum,]$steps <- meanVal
    } 
  ##print("hello")
  }
summary(dailySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
splitNewByDay <- split(newData,newData$date, drop=TRUE)                  
dailyStepsNew <- sapply(splitNewByDay, function(x) sum(x$steps))   
summary(dailyStepsNew)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?
