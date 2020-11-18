---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





```r
rm(list = ls())
graphics.off()

# Load R packages
packages <- c("dplyr", "ggplot2", "lubridate", "stringr") # list of packages to load

package.check <- lapply( # load or install & load list of packages
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
) 
rm(packages, package.check)
```


## Loading and preprocessing the data


```r
# if csv is still in .zip file first unzip iz
if(!file.exists("activity.csv")){
  unzip(zipfile = "activity.zip")
}

# import data
df <- read.csv(file = "activity.csv", header = T)

# data pre-processing
df <- df %>% 
  mutate(hour_minute = as.character(interval), # convert interval to character to get hour minute
         hour_minute = str_pad(string = interval, # pad with leading zeros
                               width = 4, 
                               side = "left", 
                               pad = "0"), 
         hour = as.numeric(str_sub(string = hour_minute, start = 1, end = 2)), # extract only hours
         minute = as.numeric(str_sub(string = hour_minute, start = 3, end = 4)), # extract only minutes
         timestamp = paste(date, " ", hour_minute, sep = ""), # paste date and time together
         date = ymd(date), # convert date column to date class
         timestamp = ymd_hm(timestamp), # convert to datetime object
         minutes_in_day = hour * 60 + minute) %>% # calculate minutes in the day
  select(-hour_minute) # drop column
```




## What is mean total number of steps taken per day?

Total number of steps taken per day:

```r
df.steps.tot <- df %>% 
  group_by(date) %>% # calculate per day (total)
  summarise(`steps total` = sum(steps, na.rm = T)) %>% # total steps
  ungroup() 
```


Histogram of the total number of steps taken each day:


```r
df.steps.tot %>% 
  ggplot(aes(x = `steps total`)) +
  geom_histogram(bins = 30, color = "black", fill = "gray67") +
  xlab("Number of total steps per day") +
  ylab("Count") +
  ggtitle("Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/stepshist-1.png)<!-- -->



Mean and median of total number of steps taken per day:

```r
df.steps.tot %>% 
  summarise(`mean of total steps per day` = mean(`steps total`), # mean of total steps per day
            `median of total steps per day` = median(`steps total`)) # median of total steps per day
```

```
## # A tibble: 1 x 2
##   `mean of total steps per day` `median of total steps per day`
##                           <dbl>                           <int>
## 1                         9354.                           10395
```




## What is the average daily activity pattern?

Average number of steps taken per 5-min interval (all days):

```r
df.steps.interval.avg <- df %>% 
  group_by(minutes_in_day) %>% # calculate per day (total)
  summarise(`steps avg` = mean(steps, na.rm = T)) %>% # total steps
  ungroup() 
```

Time series plot of 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
df.steps.interval.avg %>% 
  ggplot(aes(x = minutes_in_day, y = `steps avg`)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(0,60*24, 60)) +
  xlab("5 minute interval (in the day)") +
  ylab("Average number of steps (across all days)") +
  ggtitle("Line plot - 5-minute interval VS average number of steps taken, averaged across all days")
```

![](PA1_template_files/figure-html/stepsavgline-1.png)<!-- -->




## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
