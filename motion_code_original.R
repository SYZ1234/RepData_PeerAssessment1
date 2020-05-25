Author:SYZ

library(data.table)

###1. Loading and preprocessing the data
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(path, "motion_dataFiles.zip"))
unzip(zipfile = "motion_dataFiles.zip")
data <- read.csv('./activity.csv')
#just check
str(data)
#date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

###2. What is mean total number of steps taken per day?
#Histogram of total number of steps per day
steps_sum <- tapply(data$steps,data$date,sum,na.rm=T)
barplot(steps_sum,xlab='date',ylab='total steps',main='total steps per day')

###3. Mean and median number of steps taken each day
steps_mean <- mean(steps_sum,na.rm=T)
print(steps_mean) #9354.23
#Calculate median
steps_median <- median(steps_sum,na.rm=T)
print(steps_median) #10395

###4. Time series plot of the average number of steps taken
steps_average <- tapply(data$steps,data$interval,mean,na.rm=T)
plot(steps_average,xlab='5-minute interval',ylab='average steps',main='time series',type='l')

###5. The 5-minute interval that, on average, contains the maximum number of steps
which.max(steps_average) #835, 104

###6. Code to describe and show a strategy for imputing missing data
#describe dataset (data$steps) with NA
str(data)
sum(is.na(data$steps)) #2304 missing
#impute with mice package
library(mice)
data_impute <- mice(data) #default m=5 number of imputed datasets, method: pmm
data_complete <-complete(data_impute,5) #May select any 1 out of the 5 imputed datasets


###7. Histogram of the total number of steps taken each day after missing values are imputed
steps_sum_complete <- tapply(data_complete$steps, data_complete$date, sum, na.rm=T)
barplot(steps_sum_complete, xlab='date', ylab='total steps',main='total steps (impute)')

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
library(ggplot2)
data_complete$date <- as.Date(data_complete$date, format = "%Y-%m-%d")
data_complete$Week <- ifelse(weekdays(data_complete$date) %in% c('Saturday','Sunday'), 'Weekend','Weekday')

g<-ggplot(data_complete,aes(interval,steps,color=Week))
#g+geom_line(stat='summary', fun.y='mean')+labs(y='average steps',x='5-minute interval')+ggtitle('comparison')
g+facet_grid(rows=vars(Week))+geom_line(stat='summary', fun.y='mean')

###9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
