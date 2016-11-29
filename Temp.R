## Author: Peter Byrd
## Data: November 25, 2016
## Case Study 2 - Question 4

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/CaseStudy2_v2")
library(tseries)
library(ggplot2)
library(plyr)

#   Part I
##  Read CSV input file
Temp <- read.csv("Data/Temp.csv", header=TRUE)

##  Create single date format
a <- as.Date(Temp$Date,format="%Y-%m-%d") 
b <- as.Date(Temp$Date,format="%m/%d/%Y") 
a[is.na(a)] <- b[!is.na(b)]   # Combine both while keeping their ranks
Temp$Date <- a                # Put it back in your dataframe

##  Remove NA and change Country to a factor
Temp <- Temp[!(is.na(Temp$Monthly.AverageTemp)),]
Temp$Country <- as.factor(Temp$Country)

##  Create a subset of the dates for 1900 and newer
Date1 <- as.Date("1900-01-01")
Temp1900 <- Temp[Temp$Date >= Date1,]

##  Find the difference between max and min monthly avg temp for each country
maxtemp <- tapply(Temp1900$Monthly.AverageTemp, Temp1900$Country, max)
mintemp <- tapply(Temp1900$Monthly.AverageTemp, Temp1900$Country, min)
difftemp <- maxtemp - mintemp

##  Sort the data from largest to smallest difference in monthly temperatures and show top 20
difftemp.sorted <- sort(difftemp, decreasing = TRUE)
top20countries <- difftemp.sorted[1:20]
top20countries

### Plot the data
x1names <- names(top20countries)
plot(top20countries,xaxt="n",xlab="Country",ylab="Celsius",main="Top 20 Country Temp Differences")
axis(1, at=1:length(x1names), labels=x1names)

#   Part II
### Subset the data for US temperatures for dates later than 1900
UStemp <- subset(Temp1900, Country == "United States")

### Convert from C to F and add to new column
UStemp["Monthly.AverageTemp_F"] = (UStemp$Monthly.AverageTemp*1.8)+32

### Remove variables we don't want and rename variables
UStemp_new <- UStemp
UStemp_new <- UStemp_new[,-c(1,3,4)]
UStemp_new <- plyr::rename(x=UStemp_new,
                        replace = c("Monthly.AverageTemp"="AvgTemp_C","Monthly.AverageTemp_F"="AvgTemp_F"))

### Create a time series and aggregate the data by year
UStemp_monthly <- ts(UStemp_new, start=c(1900,1), end=c(2013,9), frequency = 12)
UStemp_yearly <- aggregate(UStemp_monthly, nfrequency=1, FUN=mean)

### Plot the average land temperature by year
plot(UStemp_yearly,type="l")

### Calculate the one year difference of average land temperature
UStemp_diff <- diff(UStemp_yearly)
maxdiff <- max(abs(UStemp_diff))
maxdiffyr <- 1900 + which.max(UStemp_diff[,2])
print(c('The max monthly temperature difference in Fahrenheit was ',maxdiff))
print(c('It occured between the years ',maxdiffyr-1,' and ',maxdiffyr,'.'))
### Since we measured the difference from the previous year, 1920 and 1921 have the 
### largest average temperature difference of 2.54 degrees F

#   Part III
### Read CityTemp data
CityTemp <- read.csv("Data/CityTemp.csv", header=TRUE)

### Create single date format
a <- as.Date(CityTemp$Date,format="%Y-%m-%d") 
b <- as.Date(CityTemp$Date,format="%m/%d/%Y") 
a[is.na(a)] <- b[!is.na(b)]   # Combine both while keeping their ranks
CityTemp$Date <- a                # Put it back in your dataframe

### Remove NA
CityTemp <- CityTemp[!(is.na(CityTemp$Monthly.AverageTemp)),]

### Create a subset of the dates for 1900 and newer
Date1 <- as.Date("1900-01-01")
CityTemp1900 <- CityTemp[CityTemp$Date >= Date1,]

### Find the difference between max and min monthly avg temp for each city
maxcitytemp <- tapply(CityTemp1900$Monthly.AverageTemp, CityTemp1900$City, max)
mincitytemp <- tapply(CityTemp1900$Monthly.AverageTemp, CityTemp1900$City, min)
diffcitytemp <- maxcitytemp - mincitytemp

### Sort the data from largest to smallest difference in monthly temperatures and show top 20
diffcitytemp.sorted <- sort(diffcitytemp, decreasing = TRUE)
top20cities <- diffcitytemp.sorted[1:20]
top20cities

### Plot the data
x2names <- names(top20cities)
plot(top20cities,xaxt="n",xlab="City",ylab="Celsius",main="Top 20 City Temp Differences")
axis(1, at=1:length(x2names), labels=x2names)
