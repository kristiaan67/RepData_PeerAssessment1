library(dplyr)
library(lubridate)

stopifnot(file.exists("activity.zip"))
if (!file.exists("activity.csv")) {
    unzip("activity.zip") 
}

# Load and transform the data
activityData <- read.csv("activity.csv")
activityData <- mutate(activityData,
                       date = ymd(date))

## What is mean total number of steps taken per day?

# Calculate the total number, mean and median of steps for each day
activityDataPerDay <- group_by(activityData, date) %>%
    summarise(totsteps = sum(steps, na.rm = TRUE))
print(summary(activityDataPerDay))

# Plot a histogram of the number of steps per day
meanTotSteps <- mean(activityDataPerDay$totsteps)
medianTotSteps <- median(activityDataPerDay$totsteps)
hist(activityDataPerDay$totsteps,
     main = "Total Number of Steps per Day", xlab = "Steps per Day")
abline(h = meanTotSteps, col = "blue", lty = 2)
abline(h = medianTotSteps, col = "red", lty = 2)
legend("topright", col = c("blue", "red"), lty = 2,
       legend = c(paste("Mean:", format(meanTotSteps, digits = 0, scientific = FALSE)), 
                  paste("Median:", format(medianTotSteps, digits = 0, scientific = FALSE))))


## What is the average daily activity pattern?
activityDataPerInterval <- group_by(activityData, interval) %>%
    summarise(meanSteps = mean(steps, na.rm = TRUE))
print(summary(activityDataPerInterval))

meanSteps <- mean(activityDataPerInterval$meanSteps)
maxSteps <- activityDataPerInterval[which.max(activityDataPerInterval$meanSteps),]
plot(activityDataPerInterval$interval, activityDataPerInterval$meanSteps, type = "l",
     main = "Avg. Number of Steps per Interval", xlab = "Interval", ylab = "Steps")
abline(h = meanSteps, col = "blue", lty = 2)
abline(v = maxSteps$interval, col = "red", lty = 3)
legend("topright", col = c("blue", "red"), lty = c(2, 3),
       legend = c(paste("Mean:", format(meanSteps, digits = 0, scientific = FALSE)), 
                  paste("Max:", format(maxSteps$meanSteps, digits = 0, scientific = FALSE))))

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
numNAs <- length(which(is.na(activityData$steps)))
print(paste("Number of missing values:", numNAs))

# Impute missing values by taking the mean of the corresponding interval
imputedActivityData <- mutate(activityData,
                              steps = ifelse(is.na(steps), 
                                             activityDataPerInterval$meanSteps, 
                                             steps))

# Plot a histogram of the number of steps per day
imputedActivityDataPerDay <- group_by(imputedActivityData, date) %>%
    summarise(totsteps = sum(steps, na.rm = TRUE))
print(summary(imputedActivityDataPerDay))

meanImputedTotSteps <- mean(imputedActivityDataPerDay$totsteps)
medianImputedTotSteps <- median(imputedActivityDataPerDay$totsteps)
plot(imputedActivityDataPerDay$date, imputedActivityDataPerDay$totsteps, type = "h",
     main = "Total Number of Steps per Day", xlab = "Day", ylab = "Steps")
abline(h = meanImputedTotSteps, col = "blue", lty = 2)
abline(h = medianImputedTotSteps, col = "red", lty = 2)
legend("topright", col = c("blue", "red"), lty = 2,
       legend = c(paste("Mean:", format(meanImputedTotSteps, digits = 0, scientific = FALSE)), 
                  paste("Median:", format(medianImputedTotSteps, digits = 0, scientific = FALSE))))

## Are there differences in activity patterns between weekdays and weekends?

# add factor variable 
imputedActivityData <- mutate(imputedActivityData,
                              typeOfDay = as.factor(ifelse(weekdays(imputedActivityData$date) %in% c("Saturday", "Sunday"), 
                                                    "weekend", "weekday")))
imputedActivityDataPerInterval <- group_by(imputedActivityData, interval, typeOfDay) %>%
    summarise(meanSteps = mean(steps, na.rm = TRUE))
imputedActivityDataPerIntervalWeekday <- subset(imputedActivityDataPerInterval, typeOfDay == "weekday")
imputedActivityDataPerIntervalWeekend <- subset(imputedActivityDataPerInterval, typeOfDay == "weekend")

par(mfrow = c(2, 1), mar = c(4, 4, 4, 2))
yLim <- c(0, max(imputedActivityDataPerIntervalWeekday$meanSteps, imputedActivityDataPerIntervalWeekend$meanSteps)) + 5
with(imputedActivityDataPerIntervalWeekday, plot(interval, meanSteps, 
                                                 type = "l", ylim = yLim, 
                                                 main = "Avg. Number of Steps per Interval on Weekdays", 
                                                 xlab = "", ylab = "Steps"))

with(imputedActivityDataPerIntervalWeekend, plot(interval, meanSteps, 
                                                 type = "l", ylim = yLim, 
                                                 main = "Avg. Number of Steps per Interval on Weekend", 
                                                 xlab = "Interval", ylab = "Steps"))


