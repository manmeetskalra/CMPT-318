library(lubridate)
library(chron)
library(dplyr)
library(magrittr)
library(ggplot2)

detect <- function(d) 
{
  data <- d

  
  newdata <- data[c("Date","Global_active_power")]
  newdata$Date <- as.Date(newdata$Date, format='%d/%m/%Y')
  
  averaged_days <- data[c("hours","Global_active_power")]
  #aggregate(Global_active_power ~ Date, newdata, mean)
  GAP_array = array(data = NA, dim = nrow(averaged_days)-6)
  
  # This is for calculating the global active power average using 
  # a window of 20 observations.
  # WARNING: This process will take a while.
  i <- 1
  j <- 7
  while (j < nrow(averaged_days)) {
    test_window <- averaged_days %>% slice(i:j)
    GAP_array[i] <- mean(test_window$Global_active_power, na.rm = TRUE)
    i = i+1
    j = j+1
  }
  
  df <- data.frame(GAP_array)
  df$row_num <- seq.int(nrow(df))
  
  plot(GAP_array, type="l",
       main = "Global Active Power Average per Window",
       xlab = "Days", ylab = "Global Active Power Average")
}


###########################################
# Retrieve function
retrieve <- function(d)
{
  # For Dataset 1
  # Retrieve a specific week in every dataset
  data <- d
  data$newDate = as.Date(data$Date,format='%d/%m/%Y')
  data$week = week(data$newDate)
  temp <- data[data1$week == "5",]
  # apply a specific time window on a weekday and weekend on each dataset
  temp1$day_of_week = wday(temp1$newDate,week_start = getOption("lubridate.week.start", 1))
  n <- factor(temp1$Time)
  a_time <- hms(as.character(n))
  temp1$hours <- as.integer(hour(a_time))
  
  #weekdays
  tempp1 <- temp1[temp1$day_of_week==3,]
  weekday1 = tempp1[(tempp1$hours>=9) & (tempp1$hours<=17),]
  
  #weekend
  tempe1 <- temp1[temp1$day_of_week==7,]
  weekend1 = tempe1[(tempe1$hours>=10) | (tempe1$hours<=15),]
  
  
  # apply moving average on that dataset with a sliding window of 7
  detect(weekday1)
  detect(weekend1)
}




data1 <- read.table("test1.txt", header = TRUE, sep = ",")
data2 <- read.table("test2.txt", header = TRUE, sep = ",")
data3 <- read.table("test3.txt", header = TRUE, sep = ",")
data4 <- read.table("test4.txt", header = TRUE, sep = ",")
data5 <- read.table("test5.txt", header = TRUE, sep = ",")



retrieve(data1)
retrieve(data2)
retrieve(data3)
retrieve(data4)
retrieve(data5)









