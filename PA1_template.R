
#=====================================#
## Reproducible Research
#=====================================#


## List of libraries needed  :

library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(imputeMissings)
library(lattice)
library(mice)
library(Hmisc)
library(knitr)

# Source URL :

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#=====================================#
# Extract data from the URL :
#=====================================#

download.file(url, destfile = "zipped_file.zip")

unzip(zipfile = "zipped_file.zip")

#=====================================#
## Read Input file
#=====================================#


input <- fread("activity.csv")
str(input)
input$date <- as.Date(input$date, format = "%Y-%m-%d")

# 1. Aggregate after removing NA from the input file


input_agg <- input[complete.cases(input),]

input_agg <- input_agg %>%
  group_by(date) %>%
  summarise(steps = sum(steps, na.rm = TRUE))


# 2. Plot Histogram of the total number of steps taken each day


ggplot(input_agg, aes(x = steps)) + geom_histogram(binwidth = 500, fill =
                                                     "brown")  +
  ylab("Frequency") + xlab("Total Number of Steps")  +
  ggtitle("Total Steps takes each day")
dev.copy(png, 'Total_Steps_each_day.png')
dev.off()
# 3. Mean and median number of steps taken each day


mean_steps <- mean(input_agg$steps, na.rm = TRUE)
median_steps <- median(input_agg$steps, na.rm = TRUE)
mean_steps


# 4.Time series plot of the average number of steps taken


input_avg <- input[complete.cases(input),]
input_avg <- input_avg %>%
  group_by(date) %>%
  summarise(steps = mean(steps, na.rm = TRUE))


ggplot(input_avg, aes(x = date, y = steps)) + geom_line(color = "green") +
  geom_path(na.rm = TRUE) + geom_point() + ylab("Average Steps # ") + xlab("Date")  +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "1 days") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Time series plot of average number of steps taken")
dev.copy(png, 'Time_Series_of_Avg_Steps.png')
dev.off()
# 5. The 5-minute interval that, on average, contains the maximum number of steps

# Time series plot of the 5-minute interval(x-axis) and the average number of steps taken,averaged across all days(y-axis)

input_int <- input[complete.cases(input),]

input_int <- input_int %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))



m <- input_int %>%
  mutate(color = (max(steps) == steps))
max_int <- m[m$color == "TRUE", "interval"]


ggplot(m, aes(x = interval, y = steps)) + geom_line() + geom_path(na.rm =
                                                                    TRUE) + geom_point(aes(color = color)) + ylab("Average Steps # ") + xlab("Time interval")  +  ggtitle("Time series plot of 5 min interval and average steps") +
  scale_color_manual(values = c(NA, "red"),
                     label = c("", "Maximum Steps"))
dev.copy(png, 'Time_Series of_Interveal_vs_Steps.png')
dev.off()

## Average in each interval


input_avg_interval <- input[complete.cases(input),]

input_avg_interval <- input_avg_interval %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))


ggplot(input_avg_interval, aes(x = interval, y = steps, group = 1)) + geom_line(color =
                                                                                  "green") +  ylab("Average Steps # ") + xlab("5 minute Interval") + ggtitle("Average steps in each time interval  ")
dev.copy(png, 'Avg_Steps_in_5min_Interval.png')
dev.off()
# 6.Code to describe and show a strategy for imputing missing data

str(input)

# Convert interval to factor

input$interval <- as.factor(input$interval)

# Understand the missing value pattern

input_non_missing <- length(input$steps)
input_missing <- length(which(is.na(input$steps)))

input_non_missing
input_fixed <- input

input_fixed$steps <-
  with(input_fixed, impute(input_fixed$steps, mean))

head(input_fixed)
# 7.Histogram of the total number of steps taken each day after missing values are imputed

input_fixed_agg <- input_fixed %>%
  group_by(date) %>%
  summarise(steps = sum(steps))


ggplot(input_fixed_agg, aes(x = steps)) + geom_histogram(binwidth = 500, fill =
                                                           "brown")  + ylab("Frequency") + xlab("Total Number of Steps (after imputation)")  + ggtitle("Total Steps takes each day after imputation ")
dev.copy(png, 'Total_Steps_after_Imputation.png')
dev.off()
mean_steps_imputed <- mean(input_fixed_agg$steps, na.rm = TRUE)
median_steps_imputed <- median(input_fixed_agg$steps, na.rm = TRUE)

# 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


input_weekend <-
  input_fixed[weekdays(input_fixed$date) %in% c("Saturday", "Sunday"), ]

input_weekend$Day_Type <- "Weekend"

input_weekdays <-
  input_fixed[!weekdays(input_fixed$date) %in% c("Saturday", "Sunday"), ]
input_weekdays$Day_Type <- "Weekday"

input_fixed_1 <- rbind(input_weekend, input_weekdays)


input_fixed_1 <- input_fixed_1 %>%
  group_by(Day_Type, interval) %>%
  summarise(steps = mean(steps))


ggplot(input_fixed_1, aes(x = interval, y = steps, group = 1)) + geom_line() +  ylab("Average Steps # ") + xlab("5 minute Interval") +   facet_grid(Day_Type ~ .)
dev.copy(png, 'Avg_Steps_WeekdaysvsWeekend.png')
dev.off()
