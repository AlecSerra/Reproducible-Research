---
title: "PA1_template.Rmd"
author: "Alec Serra"
date: "9 mar?? de 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Obtaining the data

First of all, the data is obtained and processed.
Obtaining the data:

```{r cars}
activity <- read.csv(file = "activity.csv", sep = ",", header = TRUE)
summary(activity)
```

## Part 1 - What is mean total number of steps taken per day?

- Aggregate the steps by day
- plot an Histogram
- calculate mean and median

```{r pressure, echo=TRUE}
steps_by_interval <- aggregate(steps~interval,activity,mean)
activity2 <- transform(activity, steps = ifelse(is.na(activity$steps), steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], activity$steps))
weekdays <- c("dilluns", "dimarts", "dimecres", "dijous", "divendres")
activity2$dow = as.factor(ifelse(is.element(weekdays(as.Date(activity2$date)),weekdays), "Weekday", "Weekend"))
step_by_day <- aggregate(steps~date,activity,sum)
steps_by_interval_3 <- aggregate(steps ~ interval + dow, activity2, mean)
hist(step_by_day$steps, main = "Steps for each day", xlab = "Number of Steps", col = "red")
```

```{r}
mean <- mean(step_by_day$steps)
median <- median(step_by_day$steps)
```

The mean is `r mean` and the median is `r median`

##Part 2 - What is the average daily activity pattern?

- Calculate the mean of steps by interval of all days
- Plotting the results
- Identify the interval with more steps
```{r}
plot(steps_by_interval$interval,steps_by_interval$steps, type ="l", main = "Average of Steps by Interval", xlab = "Interval", ylab = "Number of Steps")
```
```{r}
max_interval <- max(steps_by_interval$steps)
step_interval <- subset(steps_by_interval,steps==max_interval)[,1]
```


The Interval with more steps is the interval `r step_interval`

##Part 3 - 

In this part we will obtain the following points:
- Obtain the number of rows with NAs
- Obtain a strategy of how to obtain values for those NAs and create a new data set with tha strategy.
- Plot and Histogram of the new data set and the change of mean and median.

```{r}
NAs <- nrow(activity[!complete.cases(activity),])
```

The number of rows with NAs are `r NAs`


The NAs become only for the Column "Steps", so for those missing values the action proposed is to assign the mean of the value for that interval
```{r}
activity2 <- transform(activity, steps = ifelse(is.na(activity$steps), steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], activity$steps))
```

Except for the first day, which will be assigned to 0, since is always at NA
```{r}
activity2[as.character(activity2$date,'%Y/%m/%d'), 1] <- 0
```

Plotting and Histogram of the new dataset:
```{r}
steps_by_day_2 <- aggregate(steps~date,activity2,sum)
hist(steps_by_day_2$steps, col = "blue", main = "Steps by day without NA vs with NA", xlab = "Number of Steps")
hist(step_by_day$steps, col = "red", add = T)
legend("topright", c("without NA", "with NA"), col=c("blue", "red"), lwd=10)
```

Calculate new median and mean:
```{r}
mean_2 <- mean(steps_by_day_2$steps)
median_2 <- median(steps_by_day_2$steps)
```

- The new mean is `r mean_2`
- the previous mean is `r mean`
- The new median is `r median_2`
- The previous median is `r median`

#Part 4

Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.

```{r}
#weekdays <- c("dilluns", "dimarts", "dimecres", "dijous", "divendres")
#activity2$dow = as.factor(ifelse(is.element(weekdays(as.Date(activity2$date)),weekdays), "Weekday", "Weekend"))
library(lattice)
xyplot(steps_by_interval_3$steps ~ steps_by_interval_3$interval|steps_by_interval_3$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")