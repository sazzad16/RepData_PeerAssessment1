---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the data

```r
unzip("./activity.zip")
activity <- read.csv("./activity.csv", na.strings = "NA", 
                     colClasses = c("numeric", "Date", "integer"))
activity$minute <- sapply(activity$interval, function(x) {x%/%100*60+x%%100})
head(activity)
```

```
##   steps       date interval minute
## 1    NA 2012-10-01        0      0
## 2    NA 2012-10-01        5      5
## 3    NA 2012-10-01       10     10
## 4    NA 2012-10-01       15     15
## 5    NA 2012-10-01       20     20
## 6    NA 2012-10-01       25     25
```

```r
tail(activity)
```

```
##       steps       date interval minute
## 17563    NA 2012-11-30     2330   1410
## 17564    NA 2012-11-30     2335   1415
## 17565    NA 2012-11-30     2340   1420
## 17566    NA 2012-11-30     2345   1425
## 17567    NA 2012-11-30     2350   1430
## 17568    NA 2012-11-30     2355   1435
```



## What is mean total number of steps taken per day?

```r
eachday <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(eachday$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(eachday$steps)
```

```
## [1] 10766
```

```r
median(eachday$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
eachtime <- aggregate(steps ~ minute + interval, data = activity, FUN = mean)
plot(eachtime$minute, eachtime$steps, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
eachtime[eachtime$steps >= max(eachtime$steps), ]
```

```
##     minute interval steps
## 104    515      835 206.2
```



## Imputing missing values

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

```r
# for filling in the missing values in `steps`, 
# using the median for that 5-minute interval
newdata <- activity
newdata$steps <- with(newdata, do.call(c, tapply(steps, minute, function(y) {
    ym <- median(y, na.rm=TRUE)
    y[is.na(y)] <- ym
    y
})))
head(newdata)
```

```
##   steps       date interval minute
## 1     0 2012-10-01        0      0
## 2     0 2012-10-01        5      5
## 3     0 2012-10-01       10     10
## 4    47 2012-10-01       15     15
## 5     0 2012-10-01       20     20
## 6     0 2012-10-01       25     25
```

```r
tail(newdata)
```

```
##       steps       date interval minute
## 17563     0 2012-11-30     2330   1410
## 17564     0 2012-11-30     2335   1415
## 17565     0 2012-11-30     2340   1420
## 17566     0 2012-11-30     2345   1425
## 17567     0 2012-11-30     2350   1430
## 17568     0 2012-11-30     2355   1435
```

```r
eachdaynew <- aggregate(steps ~ date, data = newdata, FUN = sum)
hist(eachdaynew$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(eachdaynew$steps)
```

```
## [1] 9504
```

```r
median(eachdaynew$steps)
```

```
## [1] 9069
```



## Are there differences in activity patterns between weekdays and weekends?

```r
newdata$daytype <- as.factor(sapply(weekdays(newdata$date), function(x) {
    if(grepl("^S", x)) {"weekend"}
    else {"weekday"}
}))
bydaytype <- aggregate(steps ~ minute + daytype, data = newdata, FUN = mean)

library(lattice)
xyplot(steps ~ minute | daytype, data = bydaytype, type = "l", layout = c(1, 2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
