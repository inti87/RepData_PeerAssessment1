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
packages <- c("dplyr", "ggplot2", "lubridate") # list of packages to load

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
  mutate(date = ymd(date)) # convert date column to date class
```




## What is mean total number of steps taken per day?

Mean of total number of steps taken per day:

```r
df %>% 
  group_by(date) %>% # calculate per day (total)
  summarise(`total steps per day` = sum(steps, na.rm = T)) %>% # total steps
  ungroup() %>% 
  summarise(`mean of total steps per day` = mean(`total steps per day`)) # mean of total steps per day
```

```
## # A tibble: 1 x 1
##   `mean of total steps per day`
##                           <dbl>
## 1                         9354.
```




## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
