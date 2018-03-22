library(dplyr)
library(ggplot2)
library(data.table)
# Download and save the file acitivity.csv setwd
setwd("C:/SM_Projects/Personal/Training/Data Science Orientation/Data_Science_Course_5")
activity <- read.csv("activity.csv", sep = ",")
#With the str function we can check if the date is in the correct format
str(activity)
#Since date shows as a factor, we will need to convert into a Date format
activity$date <- as.Date(activity$date)
#We can use the Str function to check now if the date is converted
Str(activity)
# Question 1: What is mean total number of steps taken per day?
# First we need to remove the NA values from the steps
activity<-activity %>% filter(complete.cases(activity))
stepsummary <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
#check stepsummary
stepsummary
#calculate mean and median
mean(stepsummary)
median(stepsummary)
#plot histogram for total number of steps
hist(activity$steps, xlab = "Number of Steps", main = "Total Number of Steps Each Day", col = 'red')
# Question 1: What is the average daily activity pattern?
averagepattern <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averagepattern, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken")
#Calculating maximum number of steps
averagepattern[which.max(averagepattern$steps), ]
#Question 4: Imputing missing values
#Since I already filtered NA previously. I will create another dataset with original na values
activitywithna <- read.csv("activity.csv", sep = ",")
sum(is.na(activitywithna$steps))
#Replacing missing values
imputedactivity <- activitywithna %>%
  +     group_by(interval) %>%
  +     mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm = TRUE)))
imputedsteps <- tapply(imputedactivity$steps, imputedactivity$date, FUN = sum)
qplot(imputedsteps, binwidth = 500, xlab = "total number of steps taken each day")
mean(imputedsteps)
median(imputedsteps)
#Question 5: Are there differences in activity patterns between weekdays and weekends?
 weekday.or.weekend <- function(date) {
    day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
                  return("weekend") else stop("invalid date")
  + }
ggplot(data = imputedactivity, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken")




