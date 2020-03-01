library(lubridate)
library(chron)
library(dplyr)
library(magrittr)


#Extract the data for group 05
Datadf <- read.table("Data_Assignmet2.txt", header = TRUE, sep = ",")
#newdate=parse_date_time(Datadf$Date, orders = c("ymd", "dmy", "mdy"))
#Datadf$week=strftime(newdate, format = "%V", tz = "", usetz = FALSE)
#data <- Datadf[Datadf$week == '05',]
data <- Datadf
n <- factor(data$Time)
a_time <- hms(as.character(n))
data$hour <- as.integer(hour(a_time))
newdata <- data[c("hour","Global_active_power")]
time_array = array(data = NA, dim = nrow(newdata)-20)
GAP_array = array(data = NA, dim = nrow(newdata)-20)

i <- 1
j <- 20
while (j < nrow(newdata)) {
  test_window <- newdata %>% slice(i:j)
  #time_array[i] <- mean(test_window$hour, na.rm = TRUE)
  GAP_array[i] <- mean(test_window$Global_active_power, na.rm = TRUE)
  i = i+1
  j = j+1
}

plot(GAP_array, type="p",
     main = "Global Active Power Average per Window",
     xlab = "Window", ylab = "Global Active Power Average")

