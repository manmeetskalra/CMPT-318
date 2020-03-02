library(lubridate)
library(chron)
library(dplyr)
library(magrittr)
library(ggplot2)



data <- read.table("Data_Assignmet2.txt", header = TRUE, sep = ",")
Datadf <- read.table("Data_Assignmet2.txt", header = TRUE, sep = ",")
data <- Datadf
n <- factor(data$Time)
a_time <- hms(as.character(n))
data$hour <- as.integer(hour(a_time))

newdata <- data[c("Date","Global_active_power")]
newdata$Date <- as.Date(newdata$Date, format='%d/%m/%Y')

averaged_days <- data[c("hour","Global_active_power")]
#aggregate(Global_active_power ~ Date, newdata, mean)
GAP_array = array(data = NA, dim = nrow(averaged_days)-20)

i <- 1
j <- 20
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



averaged_days <- aggregate(Global_active_power ~ Date, newdata, mean)
window_frame = array(data = NA, dim = nrow(averaged_days)-6)
i <- 1
j <- 6

while(j < nrow(averaged_days)) {
  test_window <- averaged_days %>% slice(i:j)
  window_frame[i] <- mean(test_window$Global_active_power, na.rm = TRUE)
  i = i+1
  j = j+1
}

dd <- data.frame(window_frame)
dd$row_num <- seq.int(nrow(dd))
plot(window_frame, type="l",
     main = "Global Active Power Average per Window",
     xlab = "Days", ylab = "Global Active Power Average")
