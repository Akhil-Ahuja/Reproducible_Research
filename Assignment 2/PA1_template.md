Loading Necessary Packages
--------------------------

    library(dplyr)
    library(lattice)

Loading and Preprocessing the data
----------------------------------

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "repdata_data_activity.zip")
    activity <- read.csv(unz("repdata_data_activity.zip", "activity.csv"), header = TRUE, na.strings = "NA")

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    summary(activity)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

What is mean total number of steps taken per day?
-------------------------------------------------

Mean is in red color and Median is in Blue color

    activity_per_day<- activity %>% 
        group_by(date) %>% 
        summarise(total_steps = sum(steps), .groups = "keep")

    median <- median(activity_per_day$total_steps, na.rm = TRUE)
    median

    ## [1] 10765

    mean <- round(mean(activity_per_day$total_steps, na.rm = TRUE), 2)
    mean

    ## [1] 10766.19

    hist(activity_per_day$total_steps, main = "Histogram of Number of Steps per day", xlab = "Number of Steps")

    abline(v = median, col = "blue")
    text (10000, 20, median, col = "blue", pos = 1, srt = 90)

    abline(v = mean, col = "red", lty = 2)
    text (11000, 20, mean, col = "red", pos = 1, srt = 90)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

What is the average daily activity pattern?
-------------------------------------------

    activity_per_interval <- activity %>% 
        group_by(interval) %>% 
        summarise(total_steps = sum(steps, na.rm = TRUE), .groups = "keep")

    max <- max(activity_per_interval$total_steps, na.rm = TRUE)
    max

    ## [1] 10927

    plot(activity_per_interval$interval, activity_per_interval$total_steps, type = "l", main = "Time Series of Number of Steps per interval", xlab = "Interval", ylab = "Number of Steps")

    text(1000, max, as.character(max), col = "red")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Imputing missing values
-----------------------

    ## cleaning
    activity_cleaned <- activity[complete.cases(activity),]
    head(activity_cleaned)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

    summary(activity_cleaned)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:15264       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0

    ## missing values
    nrow(activity)-nrow(activity_cleaned)

    ## [1] 2304

    ## histogram
    activity_per_day<- activity_cleaned %>% 
        group_by(date) %>% 
        summarise(total_steps = sum(steps), .groups = "keep")

    median <- median(activity_per_day$total_steps, na.rm = TRUE)
    median

    ## [1] 10765

    mean <- round(mean(activity_per_day$total_steps, na.rm = TRUE), 2)
    mean

    ## [1] 10766.19

    hist(activity_per_day$total_steps, main = "Histogram of Number of Steps per day", xlab = "Number of Steps")

    abline(v = median, col = "blue")
    text (10000, 20, median, col = "blue", pos = 1, srt = 90)

    abline(v = mean, col = "red", lty = 2)
    text (11000, 20, mean, col = "red", pos = 1, srt = 90)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ## time series
    activity_per_interval <- activity_cleaned %>% 
        group_by(interval) %>% 
        summarise(total_steps = sum(steps), .groups = "keep")

    max <- max(activity_per_interval$total_steps)
    max

    ## [1] 10927

    plot(activity_per_interval$interval, activity_per_interval$total_steps, type = "l", main = "Time Series of Number of Steps per interval", xlab = "Interval", ylab = "Number of Steps")

    text(1000, max, as.character(max), col = "red")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-2.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    activity_cleaned$date <- as.Date(activity_cleaned$date, "%Y-%m-%d")
    activity_cleaned$day <- weekdays(activity_cleaned$date)
    activity_cleaned$day_type <- "Weekday"
    activity_cleaned$day_type[activity_cleaned$day %in% c("Saturday", "Sunday")] <- "Weekend"
    activity_cleaned$day_type <- as.factor(activity_cleaned$day_type)

    activity_per_interval <- activity_cleaned %>% 
        group_by(interval, day_type) %>% 
        summarise(total_steps = sum(steps), .groups = "keep")

    xyplot(total_steps~ interval| day_type, data = activity_per_interval, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
