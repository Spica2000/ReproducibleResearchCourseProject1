# Reproducible Research. Course Project 1
Vadim  
September 26, 2017  
# Assignment
1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

##1. Loading and processing the dataset

```r
setwd("C:/Users/Vadim/Desktop/Курс DataScience/Reproducible research/Course Project 1")
data <- read.csv("activity.csv")
```

Some basic commands to explore the data

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We need to transform the data because Date represented as factors. Reading again with right format

```r
data <- read.csv("activity.csv",colClasses=c("numeric","Date","numeric"))
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

##2. Histogram of the total number of steps taken each day

```r
#Sum steps by data
daytotal <- tapply(data$steps,data$date,sum,na.rm=TRUE)
#Total Steps by date bar chart. We need to repeat to output to the file.
hist(daytotal,col=heat.colors(8),xlab="Total Steps by Date",main="Total Steps by Date")
```

![](PA1_template_2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
png(filename="plot1.png")
hist(daytotal,col=heat.colors(8),xlab="Total Steps by Date",main="Total Steps by Date")
dev.off()
```

```
## png 
##   2
```

##3. Mean and median number of steps taken each day


```r
mean(daytotal)
```

```
## [1] 9354.23
```

```r
median(daytotal)
```

```
## [1] 10395
```

##4. Time series plot of the average number of steps taken.
Let's calculate mean of the steps in the interval:

```r
meanint <- tapply(data$steps,data$interval,mean,na.rm=TRUE)
#Output to the screen
plot(row.names(meanint),meanint,type="l",xlab="Time intervals (in minutes)",ylab="Average of Total Steps",main="Average of Total Steps in a Day")
```

![](PA1_template_2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#Output to the file
png(filename="plot2.png")
plot(row.names(meanint),meanint,type="l",xlab="Time intervals (in minutes)",ylab="Average of Total Steps",main="Average of Total Steps in a Day")
dev.off()
```

```
## png 
##   2
```

##5. The 5-minute interval that, on average, contains the maximum number of steps
Finding maximum in *meanint* and when finding the position of it.

```r
maxint <- max(meanint)
maxint_pos <- match(maxint,meanint)
meanint[maxint_pos]
```

```
##      835 
## 206.1698
```
So the answer is 

```
## [1] "835"
```

##6. Describe and show a strategy for imputing missing data
There are several common strategies to deal with missing values:

1. Constant value imputations
2. Regression model value imputations
3. Mean/mode value substitutions

We will use #3: mean value substitution of NA values in original dataset
We will use previously calculated vector *meanint* and push it to datatable.  


```r
#total number of missing data
sum(is.na(data))
```

```
## [1] 2304
```

```r
data_na <- data[is.na(data),]
data_nona <- data[complete.cases(data),]
data_na$steps <- as.numeric(meanint)
newdata <- rbind(data_na,data_nona)
newdata <- newdata[order(newdata[,2],newdata[,3]),]
```

##7. Histogram of the total number of steps taken each day after missing values are imputed
Again, we create new dataset via tapply from *newdata* to build a histogram.

```r
newdaytotal <- tapply(newdata$steps,newdata$date,sum)
hist(newdaytotal,col=terrain.colors(8),xlab="Total Steps by Date",main="Total Steps by Date (no missing values)")
```

![](PA1_template_2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#Output to the file
png(filename="plot3.png")
hist(newdaytotal,col=terrain.colors(8),xlab="Total Steps by Date",main="Total Steps by Date (no missing values)")
dev.off()
```

```
## png 
##   2
```
Note a change on the left side of the histogram. Now the data looks like a normal distribution curve.

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
Adding a new column to newdata with days and after reclassing it with *weekdays* ot *weekends* 

```r
days <- weekdays(newdata[,2])
newdata <- cbind(newdata,days)
library(plyr)
newdata$days <- revalue(newdata$days,c("понедельник"="weekday","вторник"="weekday","среда"="weekday",
                                       "четверг"="weekday","пятница"="weekday"))
newdata$days <- revalue(newdata$days,c("суббота"="weekend","воскресенье"="weekend"))
```

Now plot using lattice library after constructing a new dataset with sums by weekdays/weekends

```r
new_meanint <- tapply(newdata$steps,list(newdata$interval,newdata$days),mean)
library(reshape2)
new_meanint <- melt(new_meanint)
colnames(new_meanint) <- c("interval","day","steps")
library(lattice)
xyplot(new_meanint$steps ~ new_meanint$interval | new_meanint$day, layout=c(1,2),type="l",main="Average of Total Steps (weekday vs. weekend)",xlab="Time intervals (in minutes)",ylab="Average of Total Steps")
```

![](PA1_template_2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#Output to the file
png("plot4.png")
xyplot(new_meanint$steps ~ new_meanint$interval | new_meanint$day, layout=c(1,2),type="l",main="Average of Total Steps (weekday vs. weekend)",xlab="Time intervals (in minutes)",ylab="Average of Total Steps")
dev.off()
```

```
## png 
##   2
```
Notice difference in activity pattern on weekdays and weekends. There are less peaks activity on weekdays.  
