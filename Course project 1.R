getwd()
setwd("C:/Users/hldew/Documents/Coursera/R Stats John Hopkins/Reproducible Research/Week 2")
install.packages("Hmisc")
install.packages("plyr")
install.packages('lubridate')

library(plyr)
library(Hmisc)
library(tidyverse)
library(scales)
library(lubridate)

##################
#################
activity_data<-read.csv("./activity.csv")
head(activity_data)
dim(activity_data)

#What is mean total number of steps taken per day?
#1: Total count of steps per day (sum of total steps per day)
data <-aggregate(x = activity_data$steps,                # Specify data column
                        by = list(activity_data$date),              # Specify group indicator
                        FUN = sum, na.rm = TRUE)                            # Specify function (i.e. sum)
head(data)

#2: histogram of step frequency 
data_hist<- data$x
hist(data_hist,
     main="Frequency of range of steps per day",
     xlab="Total steps",
     col="blue",
     ylim = c(0,30),
)

#3: #mean and median number of all steps per day
mean_steps <- round(mean(data$x, na.rm = TRUE))
print(paste("The mean is: ", mean_steps)) 

median_steps <- round(median(data$x, na.rm = TRUE))
print(paste("The median is: ", median_steps)) 


#What is the average daily activity pattern?
#4: average number of steps for each interval
interval_mean <-aggregate(x = activity_data$steps,                # Specify data column
                          by = list(activity_data$interval),              # Specify group indicator
                          FUN = mean, na.rm=TRUE)                            # Specify function (i.e. sum)
head(interval_mean)

#4: time series plot  (can still convert time/date to make graph more accurate - does not change visual outcome)
interval_meanplot <- ggplot(interval_mean, aes(x=Group.1, y=x)) +
  geom_line(color="#69b3a2") + 
  xlab("Five minute intervals over 24 hour period") +
  ylab("Average Steps taken")
interval_meanplot

#5 min interval with the highest step average 
interval_avg <-interval_mean[which.max(interval_mean$x),]
print(paste("The highest average number of steps for any 5 minute interval: ", interval_avg$Group.1)) 

#6 Count of NA's in original activity_data
sapply(activity_data, function(x) sum(is.na(x)))

#6 impute with mean value
df_imputed <- ddply(activity_data, "interval", mutate, imputed.value = impute(steps, mean))

# merge imputed.value column data with steps to replace NA values with imputed mean values in new column steps1
dfIm <- df_imputed %>%
          mutate(steps1 = coalesce(steps, imputed.value))
head(dfIm)

drop_col <- c("steps","imputed.value")
new_df<-dfIm[ , !(names(dfIm) %in% drop_col)]
head(new_df)


#7 histogram of step frequency  (imputed data)
new_dfIm <-aggregate(x = new_df$steps1,                # Specify data column
                 by = list(new_df$date),              # Specify group indicator
                 FUN = sum)                            # Specify function (i.e. sum)
head(new_dfIm)

dfIMhist<- new_dfIm$x
head(dfIMhist)
hist(dfIMhist,
     main="Frequency of range of steps per day (Imputed data)",
     xlab="Total steps",
     col="blue",
)

#mean number of all steps per day
#mean and median number of all steps per day
mean_steps_new_df <- round(mean(new_dfIm$x))
print(paste("The mean is: ", mean_steps_new_df)) 

median_steps_new_df <- round(median(new_dfIm$x))
print(paste("The median is: ", median_steps_new_df)) 

#8 Are there differences in activity patterns between weekdays and weekends?
new_df1 <-new_df

new_df1$date <- as.Date(new_df1$date)
new_df1$day <- weekdays(new_df1$date)

new_df1$weekday_end <- ifelse(new_df1$day == c("Saturday","Sunday"), "Weekend","Weekday")

week_df <- filter(new_df1, new_df1$weekday_end == "Weekday")
head(week_df)

Weekend_df <- filter(new_df1, new_df1$weekday_end == "Weekend")
head(Weekend_df)

week_df_mean <-aggregate(x = week_df$steps1,                
                         by = list(week_df$interval),              
                         FUN = mean)                            

week_df_mean$weekday_end <- "weekday"
colnames(week_df_mean) <- c("interval", "mean_steps","Day")
head(week_df_mean)

Weekend_df_mean <-aggregate(x = Weekend_df$steps1,                
                            by = list(Weekend_df$interval),              
                            FUN = mean)                            

Weekend_df_mean$weekday_end <- "Weekend"
colnames(Weekend_df_mean) <- c("interval", "mean_steps","Day")
head(Weekend_df_mean)

joined_df <- rbind(week_df_mean, Weekend_df_mean) 
head(joined_df)


#time series plots  weekdays and weekends
g <- ggplot (joined_df, aes (x=interval, y=mean_steps))
g + geom_line(color="steelblue") + facet_grid (Day~.) + 
  labs(y = "Mean Number of Steps") + labs(x = " 5 Minute Intervals") + 
  ggtitle("Average Number of Steps - Weekday vs. Weekend") +
  theme(plot.title = element_text(hjust = 0.5))
