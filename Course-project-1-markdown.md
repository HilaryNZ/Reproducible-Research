Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The dataset (Activity monitoring data \[52K\]) used in this assignment
was downloaded from the course web site:

The variables included in this dataset are:

    *steps: Number of steps taking in a 5-minute interval (missing values are coded as NA\color{red}{\verb|NA|}NA)
    *date: The date on which the measurement was taken in YYYY-MM-DD format
    *interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations.

Install libraries
-----------------

    library(plyr)
    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     is.discrete, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

    library(tidyverse)

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v tibble  3.0.5     v dplyr   1.0.3
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::arrange()   masks plyr::arrange()
    ## x purrr::compact()   masks plyr::compact()
    ## x dplyr::count()     masks plyr::count()
    ## x dplyr::failwith()  masks plyr::failwith()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::id()        masks plyr::id()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::mutate()    masks plyr::mutate()
    ## x dplyr::rename()    masks plyr::rename()
    ## x dplyr::src()       masks Hmisc::src()
    ## x dplyr::summarise() masks plyr::summarise()
    ## x dplyr::summarize() masks Hmisc::summarize(), plyr::summarize()

    library(scales)

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

Loading and preprocessing the data
----------------------------------

    activity_data<-read.csv("./activity.csv")
    head(activity_data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

### Calculate the total number of steps taken per day

    data <-aggregate(x = activity_data$steps,                # Specify data column
                            by = list(activity_data$date),              # Specify group indicator
                            FUN = sum, na.rm = TRUE)                            # Specify function (i.e. sum)
    head(data)

    ##      Group.1     x
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

### Histogram of the total number of steps taken each day

    data_hist<- data$x
    hist(data_hist,
         main="Frequency of range of steps per day",
         xlab="Total steps",
         col="blue",
         ylim = c(0,30),
    )

![](Course-project-1-markdown_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

    mean_steps <- round(mean(data$x, na.rm = TRUE))
    print(paste("The mean is: ", mean_steps)) 

    ## [1] "The mean is:  9354"

    median_steps <- round(median(data$x, na.rm = TRUE))
    print(paste("The median is: ", median_steps)) 

    ## [1] "The median is:  10395"

What is the average daily activity pattern?
-------------------------------------------

    interval_mean <-aggregate(x = activity_data$steps,                
                              by = list(activity_data$interval),              
                              FUN = mean, na.rm=TRUE)                            
    head(interval_mean)

    ##   Group.1         x
    ## 1       0 1.7169811
    ## 2       5 0.3396226
    ## 3      10 0.1320755
    ## 4      15 0.1509434
    ## 5      20 0.0754717
    ## 6      25 2.0943396

Time series plot
----------------

    interval_meanplot <- ggplot(interval_mean, aes(x=Group.1, y=x)) +
      geom_line(color="#69b3a2") + 
      xlab("Five minute intervals over 24 hour period") +
      ylab("Average Steps taken")
    interval_meanplot

![](Course-project-1-markdown_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### 5 min interval with the highest step average

    interval_avg <-interval_mean[which.max(interval_mean$x),]
    print(paste("The highest average number of steps for any 5 minute interval: ", interval_avg$Group.1)) 

    ## [1] "The highest average number of steps for any 5 minute interval:  835"

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values. The presence of missing days may introduce bias into some
calculations or summaries of the data.

### Calculate and report the total number of missing values in the dataset

    sapply(activity_data, function(x) sum(is.na(x)))

    ##    steps     date interval 
    ##     2304        0        0

### Impute with mean value

    df_imputed <- ddply(activity_data, "interval", mutate, imputed.value = impute(steps, mean))
    dfIm <- df_imputed %>%
              mutate(steps1 = coalesce(steps, imputed.value))

    drop_col <- c("steps","imputed.value")
    new_df<-dfIm[ , !(names(dfIm) %in% drop_col)]
    head(new_df)

    ##         date interval    steps1
    ## 1 2012-10-01        0  1.716981
    ## 2 2012-10-02        0  0.000000
    ## 3 2012-10-03        0  0.000000
    ## 4 2012-10-04        0 47.000000
    ## 5 2012-10-05        0  0.000000
    ## 6 2012-10-06        0  0.000000

### Histogram of step frequency

    new_dfIm <-aggregate(x = new_df$steps1,               
                     by = list(new_df$date),               
                     FUN = sum)                            

    dfIMhist<- new_dfIm$x
    head(dfIMhist)

    ## [1] 10766.19   126.00 11352.00 12116.00 13294.00 15420.00

    hist(dfIMhist,
         main="Frequency of range of steps per day (Imputed data)",
         xlab="Total steps",
         col="blue",
    )

![](Course-project-1-markdown_files/figure-markdown_strict/unnamed-chunk-10-1.png)

### Mean and median number of all steps per day

    mean_steps_new_df <- round(mean(new_dfIm$x))
    print(paste("The mean is: ", mean_steps_new_df)) 

    ## [1] "The mean is:  10766"

    median_steps_new_df <- round(median(new_dfIm$x))
    print(paste("The median is: ", median_steps_new_df)) 

    ## [1] "The median is:  10766"

What is the impact of imputing missing data on the estimates of the total daily number of steps?
------------------------------------------------------------------------------------------------

Imputing the NA’s values increases the mean of steps taken per day from
9354 to 10766 and the median from 10395 to 10766. Using imputed data
allows for the data containing NA’s to be used in the analysis, however
caution should be taken as the imputed values may bias the analysis.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    new_df1 <-new_df

    new_df1$date <- as.Date(new_df1$date)
    new_df1$day <- weekdays(new_df1$date)

    new_df1$weekday_end <- ifelse(new_df1$day == c("Saturday","Sunday"), "Weekend","Weekday")

    week_df <- filter(new_df1, new_df1$weekday_end == "Weekday")
    head(week_df)

    ##         date interval    steps1       day weekday_end
    ## 1 2012-10-01        0  1.716981    Monday     Weekday
    ## 2 2012-10-02        0  0.000000   Tuesday     Weekday
    ## 3 2012-10-03        0  0.000000 Wednesday     Weekday
    ## 4 2012-10-04        0 47.000000  Thursday     Weekday
    ## 5 2012-10-05        0  0.000000    Friday     Weekday
    ## 6 2012-10-06        0  0.000000  Saturday     Weekday

    Weekend_df <- filter(new_df1, new_df1$weekday_end == "Weekend")
    head(Weekend_df)

    ##         date interval   steps1      day weekday_end
    ## 1 2012-10-13        0 0.000000 Saturday     Weekend
    ## 2 2012-10-14        0 0.000000   Sunday     Weekend
    ## 3 2012-10-27        0 0.000000 Saturday     Weekend
    ## 4 2012-10-28        0 0.000000   Sunday     Weekend
    ## 5 2012-11-10        0 1.716981 Saturday     Weekend
    ## 6 2012-11-11        0 0.000000   Sunday     Weekend

### Create df for weekdays

    week_df_mean <-aggregate(x = week_df$steps1,                
                              by = list(week_df$interval),              
                              FUN = mean)                            

    week_df_mean$weekday_end <- "weekday"
    colnames(week_df_mean) <- c("interval", "mean_steps","Day")
    head(week_df_mean)

    ##   interval mean_steps     Day
    ## 1        0 1.94375222 weekday
    ## 2        5 0.38447846 weekday
    ## 3       10 0.14951940 weekday
    ## 4       15 0.17087932 weekday
    ## 5       20 0.08543966 weekday
    ## 6       25 2.37095052 weekday

### Create df for weekends

    Weekend_df_mean <-aggregate(x = Weekend_df$steps1,                
                             by = list(Weekend_df$interval),              
                             FUN = mean)                            

    Weekend_df_mean$weekday_end <- "Weekend"
    colnames(Weekend_df_mean) <- c("interval", "mean_steps","Day")
    head(Weekend_df_mean)

    ##   interval  mean_steps     Day
    ## 1        0 0.214622642 Weekend
    ## 2        5 0.042452830 Weekend
    ## 3       10 0.016509434 Weekend
    ## 4       15 0.018867925 Weekend
    ## 5       20 0.009433962 Weekend
    ## 6       25 0.261792453 Weekend

### Join df’s for time series plots

    joined_df <- rbind(week_df_mean, Weekend_df_mean)  #UP TO HERE
    head(joined_df)

    ##   interval mean_steps     Day
    ## 1        0 1.94375222 weekday
    ## 2        5 0.38447846 weekday
    ## 3       10 0.14951940 weekday
    ## 4       15 0.17087932 weekday
    ## 5       20 0.08543966 weekday
    ## 6       25 2.37095052 weekday

### Time series plots for weekdays and weekends

    g <- ggplot (joined_df, aes (x=interval, y=mean_steps))
    g + geom_line(color="steelblue") + facet_grid (Day~.) + 
      labs(y = "Mean Number of Steps") + labs(x = " 5 Minute Intervals") + 
      ggtitle("Average Number of Steps - Weekday vs. Weekend") +
      theme(plot.title = element_text(hjust = 0.5))

![](Course-project-1-markdown_files/figure-markdown_strict/unnamed-chunk-17-1.png)
