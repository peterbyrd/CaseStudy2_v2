## Author: Peter Byrd
## Data: November 25, 2016
## Case Study 2 - Question 2

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/CaseStudy2_v2")

## Install and load the following packages: 
# install.packages('tseries')
library(tseries)

## Read the file
TCPdata <- get.hist.quote('tcp',quote="Close")
length(TCPdata)

## Calculate Log Returns and Volatility
## Time series starts 5-25-1999 to 11-23-2016
TCPret  <- log(lag(TCPdata)) - log(TCPdata)
TCPvol  <- sd(TCPret)*sqrt(250)*100
length(TCPret)
TCPvol

## Create volatility function
Vol <- function (d,logrets){
  var=0
  lam=0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist,var)
  }
  sqrt(varlist)}

## Run three scenarios of d=10,30,100
volest <- Vol(10,TCPret)
volest2 <- Vol(30,TCPret)
volest3 <- Vol(100,TCPret)

## Plot the volatility results
plot(volest,type="l")
title(main="TCP Volatility")
lines(volest2,type="l",col="red")
lines(volest3,type="l",col="blue")
