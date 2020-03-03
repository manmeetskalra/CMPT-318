#CMPT318 Group Assignment 1
#ctrl-shift-enter to run entire script at once, ctrl-L to clear console
library(ggcorrplot)
library(ggplot2)
library(gridExtra)

#Part A

Datadf <- read.table("Data_Assignment_1.txt", header = TRUE, sep = ",")
Datadf$Date <- as.POSIXlt(Datadf$Date, format = "%d/%m/%Y")
Datadf$Time <- as.POSIXlt(Datadf$Time, format = "%H:%M:%S")
ourData <- subset(Datadf, Datadf$Date >= as.POSIXlt("29/1/2007", format = "%d/%m/%Y") & Datadf$Date <= as.POSIXlt("4/2/2007", format = "%d/%m/%Y"))##week 5 data (starts on Jan 29, 2007 - Feb 4 2007 inclusive)

getMode <- function(x) {#http://www.programmingr.com/how-to-calculate-mode-in-r/
  keys <- na.omit(unique(x))
  keys[which.max(tabulate(match(x, keys)))]
}

gm_mean = function(x, na.rm=TRUE){#used from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

arithMeanA <- mean(ourData$Global_active_power, na.rm = TRUE)#removes values containing 'NA' and calculates mean
geoMeanA <- gm_mean(ourData$Global_active_power, na.rm = TRUE)
arithMedianA <- median(ourData$Global_active_power, na.rm = TRUE)
modeA <- getMode(ourData$Global_active_power)
stdDevA <- sd(ourData$Global_active_power, na.rm = TRUE)

arithMeanB <- mean(ourData$Global_reactive_power, na.rm = TRUE)#removes values containing 'NA' and calculates mean
geoMeanB <- gm_mean(ourData$Global_reactive_power, na.rm = TRUE)
arithMedianB <- median(ourData$Global_reactive_power, na.rm = TRUE)
modeB <- getMode(ourData$Global_reactive_power)
stdDevB <- sd(ourData$Global_reactive_power, na.rm = TRUE)

arithMeanC <- mean(ourData$Voltage, na.rm = TRUE)#removes values containing 'NA' and calculates mean
geoMeanC <- gm_mean(ourData$Voltage, na.rm = TRUE)
arithMedianC <- median(ourData$Voltage, na.rm = TRUE)
modeC <- getMode(ourData$Voltage)
stdDevC <- sd(ourData$Voltage, na.rm = TRUE)

cat(sprintf("Arithmetic Mean of Global_active_power: %.10f
Geometric Mean of Global_active_power: %.10f
Arithmetic Median of Global_active_power: %.10f
Mode of Global_active_power: %.10f
Standard Deviation of Global_active_power: %.10f
Arithmetic Mean of Global_reactive_power: %.10f
Geometric Mean of Global_reactive_power: %.10f
Arithmetic Median of Global_reactive_power: %.10f
Mode of Global_reactive_power: %.10f
Standard Deviation of Global_reactive_power: %.10f
Arithmetic Mean of Voltage: %.10f
Geometric Mean of Voltage: %.10f
Arithmetic Median of Voltage: %.10f
Mode of Voltage: %.10f
Standard Deviation of Voltage: %.10f", arithMeanA, geoMeanA, arithMedianA, modeA, stdDevA, arithMeanB,geoMeanB, arithMedianB, modeB, stdDevB, arithMeanC,geoMeanC, arithMedianC, modeC, stdDevC))


daysOfTheWeek <- as.POSIXlt(ourData$Date, format = "%d/%m/%Y")$wday
weekdaysDay <- subset(ourData, daysOfTheWeek != 0 & daysOfTheWeek != 6 & ourData$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & ourData$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekdaysNight <- subset(ourData, daysOfTheWeek != 0 & daysOfTheWeek != 6 & (ourData$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S") | ourData$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S")))
##might have to change weekend day to 5 and 6 according to actual calendar dates

weekendsDay <- subset(ourData, (daysOfTheWeek == 0 | daysOfTheWeek == 6) & ourData$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & ourData$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekendsNight <- subset(ourData, (daysOfTheWeek == 0 | daysOfTheWeek == 6) & (ourData$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S") | ourData$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S")))

weekdaysDayMaxA <- max(weekdaysDay$Global_active_power, na.rm = TRUE)
weekdaysDayMinA <- min(weekdaysDay$Global_active_power, na.rm = TRUE)
weekendsDayMaxA <- max(weekendsDay$Global_active_power, na.rm = TRUE)
weekendsDayMinA <- min(weekendsDay$Global_active_power, na.rm = TRUE)

weekdaysNightMaxA <- max(weekdaysNight$Global_active_power, na.rm = TRUE)
weekdaysNightMinA <- min(weekdaysNight$Global_active_power, na.rm = TRUE)
weekendsNightMaxA <- max(weekendsNight$Global_active_power, na.rm = TRUE)
weekendsNightMinA <- min(weekendsNight$Global_active_power, na.rm = TRUE)

weekdaysDayMaxB <- max(weekdaysDay$Global_reactive_power, na.rm = TRUE)
weekdaysDayMinB <- min(weekdaysDay$Global_reactive_power, na.rm = TRUE)
weekendsDayMaxB <- max(weekendsDay$Global_reactive_power, na.rm = TRUE)
weekendsDayMinB <- min(weekendsDay$Global_reactive_power, na.rm = TRUE)

weekdaysNightMaxB <- max(weekdaysNight$Global_reactive_power, na.rm = TRUE)
weekdaysNightMinB <- min(weekdaysNight$Global_reactive_power, na.rm = TRUE)
weekendsNightMaxB <- max(weekendsNight$Global_reactive_power, na.rm = TRUE)
weekendsNightMinB <- min(weekendsNight$Global_reactive_power, na.rm = TRUE)

cat(sprintf("Maximum global active power on weekends during day hours: %0.10f
            Minimum global active power on weekends during day hours: %0.10f
            Maximum global active power on weekends during night hours: %0.10f
            Minimum global active power on weekends during night hours: %0.10f
            Maximum global active power on weekdays during day hours: %0.10f
            Minimum global active power on weekdays during day hours: %0.10f
            Maximum global active power on weekdays during night hours: %0.10f
            Minimum global active power on weekdays during night hours: %0.10f
            Maximum global reactive power on weekends during day hours: %0.10f
            Minimum global reactive power on weekends during day hours: %0.10f
            Maximum global reactive power on weekends during night hours: %0.10f
            Minimum global reactive power on weekends during night hours: %0.10f
            Maximum global reactive power on weekdays during day hours: %0.10f
            Minimum global reactive power on weekdays during day hours: %0.10f
            Maximum global reactive power on weekdays during night hours: %0.10f
            Minimum global reactive power on weekdays during night hours: %0.10f", weekendsDayMaxA, weekendsDayMinA, weekendsNightMaxA, weekendsNightMinA, weekdaysDayMaxA, weekdaysDayMinA, weekdaysNightMaxA, weekdaysNightMinA, weekendsDayMaxB, weekendsDayMinB, weekendsNightMaxB, weekendsNightMinB, weekdaysDayMaxB, weekdaysDayMinB, weekdaysNightMaxB, weekdaysNightMinB))





#CMPT318 Group Assignment 1
#Part B

Datadf <- read.table("Data_Assignment_1.txt", header = TRUE, sep = ",")
Datadf$Date <- as.POSIXlt(Datadf$Date, format = "%d/%m/%Y")
Datadf$Time <- as.POSIXlt(Datadf$Time, format = "%H:%M:%S")
ourData <- subset(Datadf, Datadf$Date >= as.POSIXlt("29/1/2007", format = "%d/%m/%Y") & Datadf$Date <= as.POSIXlt("4/2/2007", format = "%d/%m/%Y"))##week 5 data (starts on Jan 29, 2007 - Feb 4 2007 inclusive)

#AB (Global_active_power, Global_reactive_power)
AB <- cor(ourData$Global_active_power, ourData$Global_reactive_power, method = "pearson")

#AC (Global_active_power, Voltage)
AC <- cor(ourData$Global_active_power, ourData$Voltage, method = "pearson")

#AD (Global_active_power, Global_intensity)
AD <- cor(ourData$Global_active_power, ourData$Global_intensity, method = "pearson")

#AE (Global_active_power, Submetering 1)
AE <- cor(ourData$Global_active_power, ourData$Sub_metering_1, method = "pearson")

#AF (Global_active_power, Submetering 2)
AF <- cor(ourData$Global_active_power, ourData$Sub_metering_2, method = "pearson")

#AG (Global_active_power, Submetering 3)
AG <- cor(ourData$Global_active_power, ourData$Sub_metering_3, method = "pearson")

#BC (Global_reactive_power, Voltage)
BC <- cor(ourData$Global_reactive_power, ourData$Voltage, method = "pearson")

#BD (Global_reactive_power, Global_intensity)
BD <- cor(ourData$Global_reactive_power, ourData$Global_intensity, method = "pearson")

#BE (Global_reactive_power, Submetering 1)
BE <- cor(ourData$Global_reactive_power, ourData$Sub_metering_1, method = "pearson")

#BF (Global_reactive_power, Submetering 2)
BF <- cor(ourData$Global_reactive_power, ourData$Sub_metering_2, method = "pearson")

#BG (Global_reactive_power, Submetering 3)
BG <- cor(ourData$Global_reactive_power, ourData$Sub_metering_3, method = "pearson")

#CD (Voltage, Global_intensity)
CD <- cor(ourData$Voltage, ourData$Global_intensity, method = "pearson")

#CE (Voltage, Submetering 1)
CE <- cor(ourData$Voltage, ourData$Sub_metering_1, method = "pearson")

#CF (Voltage, Submetering 2)
CF <- cor(ourData$Voltage, ourData$Sub_metering_2, method = "pearson")

#CG (Voltage, SUbmetering 3)
CG <- cor(ourData$Voltage, ourData$Sub_metering_3, method = "pearson")

#DE (Global_intensity, Submetering 1)
DE <- cor(ourData$Global_intensity, ourData$Sub_metering_1, method = "pearson")

#DF (Global_intensity, Submetering 2)
DF <- cor(ourData$Global_intensity, ourData$Sub_metering_2, method = "pearson")

#DG (Global_intensity, Submetering 3)
DG <- cor(ourData$Global_intensity, ourData$Sub_metering_3, method = "pearson")

#EF (Submetering 1, Submetering 2)
EF <- cor(ourData$Sub_metering_1, ourData$Sub_metering_2, method = "pearson")

#EG (Submetering 1, Submetering 3)
EG <- cor(ourData$Sub_metering_1, ourData$Sub_metering_3, method = "pearson")

#FG (Submetering 2, Submetering 3)
FG <- cor(ourData$Sub_metering_2, ourData$Sub_metering_3, method = "pearson")

cat(sprintf("Correlation between AB: %0.2f
            Correlation between AC: %0.2f
            Correlation between AD: %0.2f
            Correlation between AE: %0.2f
            Correlation between AF: %0.2f
            Correlation between AG: %0.2f
            Correlation between BC: %0.2f
            Correlation between BD: %0.2f
            Correlation between BE: %0.2f
            Correlation between BF: %0.2f
            Correlation between BG: %0.2f
            Correlation between CD: %0.2f
            Correlation between CE: %0.2f
            Correlation between CF: %0.2f
            Correlation between CG: %0.2f
            Correlation between DE: %0.2f
            Correlation between DF: %0.2f
            Correlation between DG: %0.2f
            Correlation between EF: %0.2f
            Correlation between EG: %0.2f
            Correlation between FG: %0.2f", AB,AC,AD,AE,AF,AG,BC,BD,BE,BF,BG,CD,CE,CF,CG,DE,DF,DG,EF,EG,FG))

#Plot correlation matrix 
mc_data <- ourData[,3:length(ourData)]
matr <- round(cor(mc_data),3)
ggcorrplot(matr, lab = TRUE)





#CMPT318 Group Assignment 1 
#Part C

####################################################
#             Loading Data                        #
###################################################
Datadf <- read.table("Data_Assignment_1.txt", header = TRUE, sep = ",")
Datadf$Date <- as.POSIXlt(Datadf$Date, format = "%d/%m/%Y")
Datadf$Time <- as.POSIXlt(Datadf$Time, format = "%H:%M:%S")
ourData <- subset(Datadf, Datadf$Date >= as.POSIXlt("29/1/2007", format = "%d/%m/%Y") & Datadf$Date <= as.POSIXlt("4/2/2007", format = "%d/%m/%Y"))##week 5 data (starts on Jan 29, 2007 - Feb 4 2007 inclusive)

daysOfTheWeek <- as.POSIXlt(ourData$Date, format = "%d/%m/%Y")$wday
weekdaysDay <- subset(ourData, daysOfTheWeek != 0 & daysOfTheWeek != 6 & ourData$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & ourData$Time < as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekdaysNight <- subset(ourData, daysOfTheWeek != 0 & daysOfTheWeek != 6 & (ourData$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S") | ourData$Time >= as.POSIXlt("18:00:00", format = "%H:%M:%S")))

weekendsDay <- subset(ourData, (daysOfTheWeek == 0 | daysOfTheWeek == 6) & ourData$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & ourData$Time < as.POSIXlt("18:00:00", format = "%H:%M:%S"))
weekendsNight <- subset(ourData, (daysOfTheWeek == 0 | daysOfTheWeek == 6) & (ourData$Time >= as.POSIXlt("18:00:00", format = "%H:%M:%S") | ourData$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S")))

#Daytime starts at 06:00:00
hr <- 06
min <- 00
sec <- 00
j <- -1

weekdaysDaytimeAverages <- 0:719
weekdaysNighttimeAverages <- 0:719
weekendsDaytimeAverages <- 0:719
weekendsNighttimeAverages <- 0:719



####################################################
#             Calculating Averages                 #
###################################################
#Weekday Daytime Calculations
for(i in 0:719){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekdaysDaytimeMinutes <- subset(weekdaysDay, weekdaysDay$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekdaysDaytimeMinutes$Global_intensity, na.rm = TRUE)
  weekdaysDaytimeAverages[i + 1] <- avg
}

#Weekday Nighttime calculation from 00:00:00 - 05:59:00
hr <- 0
j <- -1
for(i in 0:359){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekdaysNighttimeMinutes <- subset(weekdaysNight, weekdaysNight$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekdaysNighttimeMinutes$Global_intensity, na.rm = TRUE)
  weekdaysNighttimeAverages[i + 1] <- avg
}

#Weekday Nighttime Calculation from 18:00:00 - 23:59:00
hr <- 18
j <- -1
k <- 361
for(i in 360:719){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekdaysNighttimeMinutes <- subset(weekdaysNight, weekdaysNight$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekdaysNighttimeMinutes$Global_intensity, na.rm = TRUE)
  weekdaysNighttimeAverages[k] <- avg
  k <- k + 1
}

#Weekend Daytime Calculations
hr <- 06
j <- -1
for(i in 0:719){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekendsDaytimeMinutes <- subset(weekendsDay, weekendsDay$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekendsDaytimeMinutes$Global_intensity, na.rm = TRUE)
  weekendsDaytimeAverages[i + 1] <- avg
}

#Weekend Nighttime Calculation from 00:00:00 - 05:59:00
hr <- 18
j <- -1
for(i in 0:719){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekendsNighttimeMinutes <- subset(weekendsNight, weekendsNight$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekendsNighttimeMinutes$Global_intensity, na.rm = TRUE)
  weekendsNighttimeAverages[i + 1] <- avg
}

#Weekend Nighttime Calculations from 18:00:00 - 23:59:00
hr <- 18
j <- -1
k <- 361
for(i in 360:719){
  if((i %% 60) == 0){
    j <- j + 1
  }
  weekendsNighttimeMinutes <- subset(weekendsNight, weekendsNight$Time == as.POSIXlt(paste((hr + j) %% 24, (min + i) %% 60, sec, sep = ":"), format = "%H:%M:%S"))
  avg <- mean(weekendsNighttimeMinutes$Global_intensity, na.rm = TRUE)
  weekdaysNighttimeAverages[k] <- avg
  k <- k + 1
}


####################################################
#             Converting to Dataframes             #
###################################################
x <- subset(weekdaysDay$Time, weekdaysDay$Date == as.POSIXlt("29/1/2007",format = "%d/%m/%Y"))
avg <- c(weekdaysDaytimeAverages)
Time <- c(x)
weekdaysDaytimeDF <- data.frame(avg, Time)

x2 <- subset(weekdaysNight$Time, weekdaysNight$Date == as.POSIXlt("29/1/2007",format = "%d/%m/%Y"))
avg2 <- c(weekdaysNighttimeAverages)
Time2 <- c(x2)
weekdaysNighttimeDF <- data.frame(avg2, Time2)

x3 <- subset(weekendsDay$Time, weekendsDay$Date == as.POSIXlt("3/2/2007", format = "%d/%m/%Y"))
avg3 <- c(weekendsDaytimeAverages)
Time3 <- c(x3)
weekendsDaytimeDF <- data.frame(avg3, Time3)

x4 <- subset(weekendsNight$Time, weekendsNight$Date == as.POSIXlt("3/2/2007",format = "%d/%m/%Y"))
avg4 <- c(weekendsNighttimeAverages)
Time4 <- c(x4)
weekendsNighttimeDF <- data.frame(avg4, Time4)





####################################################
#             Calculating Linear Fits              #
###################################################
fit_linear_wdd <- lm(formula=avg ~ Time, data=weekdaysDaytimeDF)
summary(fit_linear_wdd)
RSS <- (fit_linear_wdd$residuals^2)
p <- predict(fit_linear_wdd, weekdaysDaytimeDF)
weekdaysDaytimeDF$prediction <- p

fit_linear_wdn <- lm(formula=avg2 ~ Time2, data=weekdaysNighttimeDF)
summary(fit_linear_wdn)
RSS2 <- (fit_linear_wdn$residuals^2)
p2 <- predict(fit_linear_wdn, weekdaysNighttimeDF)
weekdaysNighttimeDF$prediction2 <- p2

fit_linear_wed <- lm(formula=avg3 ~ Time3, data=weekendsDaytimeDF)
summary(fit_linear_wed)
RSS3 <- (fit_linear_wed$residuals^2)
p3 <- predict(fit_linear_wed, weekendsDaytimeDF)
weekendsDaytimeDF$prediction3 <- p3

fit_linear_wen <- lm(formula=avg4 ~ Time4, data=weekendsNighttimeDF)
summary(fit_linear_wen)
RSS4 <- (fit_linear_wen$residuals^2)
p4 <- predict(fit_linear_wen, weekendsNighttimeDF)
weekendsNighttimeDF$prediction4 <- p4



####################################################
#             Plotting Linear Regressions          #
###################################################

weekdaysPlot <- ggplot() + layer(data=weekdaysDaytimeDF, mapping=aes(x=Time, y=avg), stat="identity", geom = "point", position = "identity") +
  layer(data=weekdaysNighttimeDF, mapping=aes(x=Time2, y=avg2), stat="identity", geom = "point", position = "identity") + xlab("Time") + ylab("Average Global Intensity") + labs(colour = "Time Frame") + ggtitle("Weekday Averages")
weekendsPlot <- ggplot() + layer(data=weekendsDaytimeDF, mapping=aes(x=Time3, y=avg3), stat="identity", geom = "point", position = "identity") +
  layer(data=weekendsNighttimeDF, mapping=aes(x=Time4, y=avg4), stat="identity", geom = "point", position = "identity") + xlab("Time") + ylab("Average Global Intensity") + labs(colour = "Time Frame") + ggtitle("Weekend Averages")



weekdaysDaytimeLin <- ggplot() + layer(data=weekdaysDaytimeDF, mapping=aes(x=Time, y=avg), stat="identity", geom = "point", position = "identity") + geom_line(data=weekdaysDaytimeDF, mapping=aes(x=Time, y=prediction), size=1, color="red") + ylab("Average Global Intensity") + labs(colour = "Time Frame") + ggtitle("Weekday Daytime Linear Fit")
weekdaysNighttimeLin <- ggplot() + layer(data=weekdaysNighttimeDF, mapping=aes(x=Time2, y=avg2), stat="identity", geom = "point", position = "identity") + geom_line(data=weekdaysNighttimeDF, mapping=aes(x=Time2, y=prediction2), size=1, color="purple") + ylab("Average Global Intensity") + labs(colour = "Time Frame")  + ggtitle("Weekday Nighttime Linear Fit")
weekendsDaytimeLin <- ggplot() + layer(data=weekendsDaytimeDF, mapping=aes(x=Time3, y=avg3), stat="identity", geom = "point", position = "identity") + geom_line(data=weekendsDaytimeDF, mapping=aes(x=Time3, y=prediction3), size=1, color="pink") + ylab("Average Global Intensity") + labs(colour = "Time Frame")  + ggtitle("Weekend Daytime Linear Fit")
weekendsNighttimeLin <- ggplot() + layer(data=weekendsNighttimeDF, mapping=aes(x=Time4, y=avg4), stat="identity", geom = "point", position = "identity") + geom_line(data=weekendsNighttimeDF, mapping=aes(x=Time4, y=prediction4), size=1, color="yellow") + ylab("Average Global Intensity") + labs(colour = "Time Frame")  + ggtitle("Weekend Nighttime Linear Fit")


grid.arrange(weekdaysPlot, weekendsPlot, nrow=1)
grid.arrange(weekdaysDaytimeLin, weekdaysNighttimeLin,weekendsDaytimeLin, weekendsNighttimeLin, nrow = 2)


####################################################
#        Polynomial Regression of Degree 2         #
###################################################

dfTime <- data.frame(Time)
dfTime$Timex <- format(as.POSIXct(Time) , format = "%H%M")
dfTime2 <- data.frame(Time2)
dfTime2$Timex <- format(as.POSIXct(Time2) , format = "%H%M")
whatevs <- as.numeric(dfTime$Timex) - 600

##############Weekday Daytime######################
fit_polynomial <- lm(avg~poly(as.numeric(dfTime$Timex) - 600,2,raw=TRUE), data=weekdaysDaytimeDF)

# Residual Sum of Squares
RSS_polynomial <- sum(fit_polynomial$residuals^2)

prediction_polynomial <- predict(fit_polynomial, data=weekdaysDaytimeDF$Time)
weekdaysDaytimeDF$prediction_polynomial <- prediction_polynomial

weekdaysDaytimePoly <- ggplot(data=weekdaysDaytimeDF, mapping=aes(x=Time, y=avg)) +
  geom_point(color="blue") +
  geom_line(data=weekdaysDaytimeDF, mapping=aes(x=Time, y=prediction_polynomial), size=1, color="red") + xlab("Time") + ylab("Average Global Intensity")  + ggtitle("Weekday Daytime Polynomial Fit")

##############Weekday Nighttime######################
fit_polynomial <- lm(avg2~poly(as.numeric(dfTime2$Timex),2,raw=TRUE), data=weekdaysNighttimeDF)

# Residual Sum of Squares
RSS_polynomial <- sum(fit_polynomial$residuals^2)

prediction_polynomial <- predict(fit_polynomial, data=weekdaysNighttimeDF$Time)
weekdaysNighttimeDF$prediction_polynomial <- prediction_polynomial

weekdaysNighttimePoly <- ggplot(data=weekdaysDaytimeDF, mapping=aes(x=dfTime2$Time2, y=avg2)) +
  geom_point(color="blue") +
  geom_line(data=weekdaysNighttimeDF, mapping=aes(x=dfTime2$Time2, y=prediction_polynomial), size=1, color="red")+ xlab("Time") + ylab("Average Global Intensity")  + ggtitle("Weekday Nighttime Polynomial Fit")

##############Weekend Daytime######################
fit_polynomial <- lm(avg3~poly(as.numeric(dfTime$Timex) - 600,2,raw=TRUE), data=weekendsDaytimeDF)

RSS_polynomial <- sum(fit_polynomial$residuals^2)

prediction_polynomial <- predict(fit_polynomial, data=weekendsDaytimeDF$Time)
weekendsDaytimeDF$prediction_polynomial <- prediction_polynomial

weekendsDaytimePoly <- ggplot(data=weekdaysDaytimeDF, mapping=aes(x=dfTime$Time, y=avg3)) +
  geom_point(color="blue") +
  geom_line(data=weekendsDaytimeDF, mapping=aes(x=dfTime$Time, y=prediction_polynomial), size=1, color="red")+ xlab("Time") + ylab("Average Global Intensity")  + ggtitle("Weekend Daytime Polynomial Fit")


##############Weekend Nighttime######################
fit_polynomial <- lm(avg4~poly(as.numeric(dfTime2$Timex),2,raw=TRUE), data=weekendsNighttimeDF)

# Residual Sum of Squares
RSS_polynomial <- sum(fit_polynomial$residuals^2)

prediction_polynomial <- predict(fit_polynomial, data=weekendsNighttimeDF$Time)
weekendsNighttimeDF$prediction_polynomial <- prediction_polynomial

weekendsNighttimePoly <- ggplot(data=weekendsNighttimeDF, mapping=aes(x=Time4, y=avg4)) +
  geom_point(color="blue") +
  geom_line(data=weekendsNighttimeDF, mapping=aes(x=Time4, y=prediction_polynomial), size=1, color="red")+ xlab("Time") + ylab("Average Global Intensity")  + ggtitle("Weekend Nighttime Polynomial Fit")


grid.arrange(weekdaysDaytimePoly, weekdaysNighttimePoly, weekendsDaytimePoly, weekendsNighttimePoly, nrow=2)
