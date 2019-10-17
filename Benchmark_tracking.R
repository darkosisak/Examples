# required packages will be installed if they don't exist
required_packages <- c('quantmod', 'tidyverse', 'readxl', 'PerformanceAnalytics')
new_packages <- required_packages[!(required_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(readxl)                # part of tidyverse, for excel data reading
library(quantmod)              # for returns, stdev and correlation
library(PerformanceAnalytics)  # for annualised stdev

raw_data <- read_excel("/Users/darkosisak/Documents/R Folder/PTAP/GWP_PTAP_Data.xlsx", sheet = "10 SPDRs and S&P 500")

#####################################################################
# (a) Computing price returns                                       #
#####################################################################
ret_XLE<-ROC(raw_data$XLE, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLB<-ROC(raw_data$XLB, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLI<-ROC(raw_data$XLI, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLY<-ROC(raw_data$XLY, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLP<-ROC(raw_data$XLP, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLV<-ROC(raw_data$XLV, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLF<-ROC(raw_data$XLF, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLK<-ROC(raw_data$XLK, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLU<-ROC(raw_data$XLU, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_XLRE<-ROC(raw_data$XLRE, n=1, type=c("continuous","discrete"), na.pad = FALSE)
ret_SP500<-ROC(raw_data$SP500, n=1, type=c("continuous","discrete"), na.pad = FALSE)


# Creating the monthly returns table
spdr2_ret <- cbind(ret_XLB, ret_XLE, ret_XLF, ret_XLI, ret_XLK, ret_XLP, ret_XLRE, ret_XLU, ret_XLV, ret_XLY, ret_SP500)
returns_table <- matrix(c(spdr2_ret), ncol=11, byrow = FALSE)
colnames(returns_table) <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLRE", "XLU", "XLV", "XLY", "S&P 500")
rownames(returns_table) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
round(returns_table*10000, digits = 2)

###########################################################################
# (b) Monthly active returns for each SPDR in 2017                        #
###########################################################################
bench = ret_SP500
# Function for computing the active returns
ar_compute <- function(return)
{
  ar = return-bench
  return(ar)
}

# Implementing the function
ar_spdr <- cbind(ar_compute(ret_XLB), ar_compute(ret_XLE), ar_compute(ret_XLF), ar_compute(ret_XLI), ar_compute(ret_XLK), ar_compute(ret_XLP), ar_compute(ret_XLRE), ar_compute(ret_XLU), ar_compute(ret_XLV), ar_compute(ret_XLY))                                                                                             

# Creating the active returns table
table_spdr <- matrix(c(ar_spdr), ncol = 10, byrow = FALSE)
rownames(table_spdr) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(table_spdr) <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLRE", "XLU", "XLV", "XLY")
round(table_spdr*10000, digits = 2)

###########################################################################
#  (c) Monthly tracking errors for each SPDR in 2017                      #
###########################################################################
# Function for computing the tracking errors and MATEs
ters_compute <- function(return)
{
  t = length(return)
  ar = return-bench
  avgar = mean(ar)
  te = sd(ar)
  mate = sqrt((t-1)/t*te^2+avgar^2)
  results <- list("TE" = te, "MATE" = mate) # makes list to enable multiple outputs
  return(results)
}

# Implementing the function
ters_spdr <- cbind(ters_compute(ret_XLB), ters_compute(ret_XLE), ters_compute(ret_XLF), ters_compute(ret_XLI), ters_compute(ret_XLK), ters_compute(ret_XLP), ters_compute(ret_XLRE), ters_compute(ret_XLU), ters_compute(ret_XLV), ters_compute(ret_XLY))                                                                                             
ters_spdr = as.numeric(ters_spdr) # convert values to numeric

# Creating the tracking errors and MATEs table
table2_spdr <- matrix(c(ters_spdr), ncol = 10, byrow = FALSE)
colnames(table2_spdr) <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLRE", "XLU", "XLV", "XLY")
rownames(table2_spdr) <- c("TE","MATE")
round(table2_spdr, digits = 7)

###########################################################################
#  Determining which SPDR tracks the S&P500 best                          #
###########################################################################
best_te <- names(which.min(table2_spdr[1,]))
best_mate <- names(which.min(table2_spdr[2,]))

if (best_te == best_mate) {
  cat(best_te, 'tracks the S&P 500 best, based upon having lowest tracking error and MATE values')
}else{
cat('Based on tracking error, the', best_te, 'SPDR tracks the S&P 500 best, however, based on MATE, the', best_mate, 'SPDR tracks the S&P 500 best')
}

# Ranking SPDRs from "best" to "worst"
rank(table2_spdr[1,])
rank(table2_spdr[2,])
