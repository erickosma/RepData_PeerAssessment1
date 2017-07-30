# Reproducible Research: Peer Assessment 1
Erick GIorgio  
2017-07-30  




## Loading and preprocessing the data


load the libraty needed in this project


```r
library(knitr)
library(markdown)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library("tidyverse")
```

```
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library("lubridate")
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following objects are masked from 'package:readr':
## 
##     col_factor, col_numeric
```

```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
packages <- c("dplyr", "plyr","data.table")
```




```r
###########function unzip file if exist 
##if dont exist downlod file 
extractFile <- function(nameFile = "activity.zip"){
    print("check if file exist")
    if (!file.exists(nameFile)) {
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        tmp_file <- nameFile
        download.file(url,tmp_file, method="curl")
    }
    print(paste("unzip file ",nameFile,"   ...."))
    unzip(nameFile)
}
extractFile()
```

```
## [1] "check if file exist"
## [1] "unzip file  activity.zip    ...."
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset


```r
#function to extract  data 
processData <- function(csv ="activity.csv"){
    dataRow <- read.csv('activity.csv')
    data <- dataRow[ with (dataRow, { !(is.na(steps)) } ), ]
}

data <- processData("activity.csv")
activity <- mutate(data, ?(date))
names(activity)[4] <- "date2" 
```


```r
# Calculate the total number of steps taken per day
totalStepsDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

Make a histogram of the total number of steps taken each day



```r
# Histogram of the total number of steps taken each day
qplot(totalStepsDay, xlab='Total Steps Per Day', ylab='Frequency', binwidth=600)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## What is the average daily activity pattern?

1- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps, averaged across all days (y-axis). 


```r
#making a time series plot
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
library(ggplot2)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
# So maximum number of steps is
timeMostSteps
```

```
## [1] "835"
```

3- Calculate and report the mean and median of the total number of steps taken per day




```r
# Mean
totalSNan <- totalStepsDay[!is.na(totalStepsDay)]
stepsByDayMean <- mean(totalSNan)
stepsByDayMean
```

```
## [1] 10766.19
```



```r
# Median
stepsByDayMedian <- median(totalSNan)
stepsByDayMedian
```

```
## [1] 10765
```


## Imputing missing values

Calculate and report the total number of missing values


```r
#Calulate and report the total number of missing values in the dataset
missing <- is.na(activity$steps)
#total number of missing values
table(missing)
```

```
## missing
## FALSE 
## 15264
```



```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean) # computing the (single) imputed value from the non-NAs
```

Make a histogram of the total number of steps taken each day 



```r
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
stepsByDayImputed <- tapply(activityImputed$steps, activityImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total Steps Per Day from Imputed Dataset', ylab='Frequency', binwidth=600)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



```r
# Calculate and report the mean and median total number of steps taken per day.
#Mean
stepsByDayImputedNa <- stepsByDayImputed[!is.na(stepsByDayImputed)]
stepsByDayMeanImputed <- mean(stepsByDayImputedNa)
stepsByDayMeanImputed
```

```
## [1] 10766.19
```


```r
#Median
stepsByDayMedianImputed <- median(stepsByDayImputedNa)
stepsByDayMedianImputed
```

```
## [1] 10765
```
## Are there differences in activity patterns between weekdays and weekends?



