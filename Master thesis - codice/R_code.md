Master thesis code - FX rates. A nonlinear evaluation
================

Libraries used

``` r
## String manipulation library using regualr expressions

library(stringr) ## to make use of regular expressions to parse strings
```

    ## Warning: package 'stringr' was built under R version 3.5.2

``` r
## Data collection library

library(quantmod) ##same principle as Quandl, not limited by the amount of API calls though and connect to multiple daset providers. In this study we will mainly query the FRED database and extract data by referring to their API.
```

    ## Warning: package 'quantmod' was built under R version 3.5.2

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 3.5.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.5.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 3.5.2

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
## Data cleaning libraries

library(forecast) ## for seasonality adjusting the times series
```

    ## Warning: package 'forecast' was built under R version 3.5.2

``` r
library(tseries) ## -> check when used
```

    ## Warning: package 'tseries' was built under R version 3.5.2

``` r
library(TSA) ## for applying FFT to get seasonality frequencies
```

    ## Warning: package 'TSA' was built under R version 3.5.2

    ## 
    ## Attaching package: 'TSA'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     acf, arima

    ## The following object is masked from 'package:utils':
    ## 
    ##     tar

``` r
library(strucchange) ## to perform chow test
```

    ## Warning: package 'strucchange' was built under R version 3.5.2

    ## Loading required package: sandwich

    ## Warning: package 'sandwich' was built under R version 3.5.2

    ## 
    ## Attaching package: 'strucchange'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     boundary

``` r
## VAR packages

library(vars)  ## to estimate VAR lag with exogenous variables
```

    ## Warning: package 'vars' was built under R version 3.5.2

    ## Loading required package: MASS

    ## Loading required package: urca

    ## Warning: package 'urca' was built under R version 3.5.2

    ## Loading required package: lmtest

    ## Warning: package 'lmtest' was built under R version 3.5.2

``` r
library(tsDyn) ## to estimate the VAR model and perform the out of sample fit based on rolling window.
```

    ## Warning: package 'tsDyn' was built under R version 3.5.2

###### 

Write data to csv and import the data in order for me to start code from here, when rebooting the next day \#\#\#\#\#\#

Write csv and import csv

``` r
write <- function(dat)
  {
  write.csv(as.data.frame(dat), file = "exchange rates.csv") 
  ## as.data.frame is necessary to get the date index out of the xts
  }

import <- function(file)
  {
  data <- read.csv(file, header = T) ## loose xts notation.
  data <- as.xts(data[,-1], order.by = as.Date(data[,1]))  
  }
```

=============== Data Collection ===============

Meese and Rogoff also used the Forward rates for their estimation.

Specify function to get symbols

``` r
get_Symbols <- function(x)
  {
   lapply(x, function(sym)
     {getSymbols(sym, src = "FRED",
                 return.class = "xts",
                  auto.assign = F)  ## auto.assign specifies that not the ticker should be passed
     }) 
  }
```

########### 

FX - rates \#\#\#\#\#\#\#\#\#\#\#

Download foreign exchange rates

``` r
Symbols <- c("EXUSUK", "EXJPUS", "EXSZUS")

FX <-  get_Symbols(Symbols)
```

    ## 'getSymbols' currently uses auto.assign=TRUE by default, but will
    ## use auto.assign=FALSE in 0.5-0. You will still be able to use
    ## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
    ## and getOption("getSymbols.auto.assign") will still be checked for
    ## alternate defaults.
    ## 
    ## This message is shown once per session and may be disabled by setting 
    ## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

``` r
## Invert the USUK rate to UKUS to keep it consistent with the other series
FX[[1]] <- 1/FX[[1]]

FX <- do.call(merge, FX)

colnames(FX)[] <- c("EX_UKUS", "EX_JPUS", "EX_CHUS")

## notice like that all of the evaluation will be performed compared to the usa. if you want to make smaller bilateral comparison you need to download the other forex.
```

### 

GDP \#\#\#

GDP is an aggregated statistic that is published on a quarterly basis. You can use monthly proxies. Electricity consumption? Industrial production? &lt;-&gt; this is normalized to 100. Is that good?

Unemployment not good. Does not capture the size of an economy. Here it will be important.

``` r
## use industrial production? 
## use unemployment rate. This approach preferred
Symbols <- c("LRUN64TTUSM156N", "LRUN64TTJPM156N", "LMUNRRTTCHM156N", "LMUNRRTTGBM156N") #F not seasonally adjusted.

UNply <- get_Symbols(Symbols)

UNply <- do.call(merge, UNply) ## notice the unemployment series for UK starts just in the 90s.

colnames(UNply) <- c("UN_US", "UN_JP", "UN_CH", "UN_UK")
```

################ 

Short term bills \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

Treasury bills for capturing short term attractiveness of investments

``` r
Symbols <- c("TB3MS","INTGSTJPM193N","TBDRUKM")
 ## 3 months bill rate usa; secondary market.
 ## bill for japan; explaination not clear. check at it again to clearly understand what it is precisely.
 ## treasury bill for uk

bills <- get_Symbols(Symbols)

bills<- do.call(merge, bills) 

colnames(bills) <- c("TB_US", "TB_JP", "TB_CH") ## Treasury bills

## above not available for switzerland. use interbank rate for it.
```

Use interbank for the moment

``` r
#Symbols <- c("IR3TIB01CHM156N","IR3TIB01USM156N","IR3TIB01GBM156N", "IR3TIB01JPM156N")
 ## 3 months bill rate usa; secondary market.
 ## bill for japan; explaination not clear. check at it again to clearly understand what it is precisely.
 ## treasury bill for uk

#bills <- get_Symbols(Symbols)

#bills <- do.call(merge, bills) 

#colnames(bills) <- c("TB_US", "TB_JP", "TB_CH") ## Treasury bills
```

############# 

Monetary base \#\#\#\#\#\#\#\#\#\#\#\#\#

Problem you are taking differentials for all of the above measures. The values should consequently be consistent and comparable. Use M3 or M1 for all; these should be similarly defined.

``` r
Symbols <- c("MYAGM3USM052N", "MYAGM3JPM189N", "MABMM301GBM189N", "MABMM301CHM189N") #us, jp, uk, not seasonally adjusted. 

M3 <- get_Symbols(Symbols)

M3 <- do.call(merge, M3)

colnames(M3) <- c("M3_US", "M3_JP", "M3_UK", "M3_CH")

## all the values in national currency. Will need to transform them at a later period using FX-rate and then take the logarithm.
```

Extract M1

``` r
Symbols <- c("M1NS", "MANMM101JPM189N", "MANMM101GBM189N", "MANMM101CHM189N") 

M1 <- get_Symbols(Symbols)

M1 <- do.call(merge, M1)

colnames(M1) <- c("M1_US", "M1_JP", "M1_UK", "M1_CH")


## all the values in national currency. Will need to transform them at a later period using FX-rate and then take the logarithm.
```

### 

CPI \#\#\#

``` r
## all not seasonally adjusted, all with basis 2015
Symbols <- c('CHECPIALLMINMEI', 'JPNCPIALLMINMEI', 'GBRCPIALLMINMEI', 'USACPIALLMINMEI')

CPI <- get_Symbols(Symbols)

CPI <- do.call(merge, CPI)

colnames(CPI) <- c("CPI_CH", "CPI_JP", "CPI_UK", "CPI_US")
  
## Important difference, lapply applies a function over a list, do.call calls a function with a list of arguments.

## CHECPIALLMINMEI ## switzerland from FRED; harmonised index; basis 2015
## JPNCPIALLMINMEI ## japan; basis 2015
## GBRCPIALLMINMEI ## uk; basis 2015
## USACPIALLMINMEI ## usa, basis 2015

## good all with the same basis
```

############## 

Trade balances \#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
## Current accounts are available just on quartal level. Two options use current accounts and hold it fix for the entire quartal.

## Option two; chosen option, find a proxy for the current account balance.

Symbols <- c("XTNTVA01CHM664N", "XTNTVA01JPM664N", "XTNTVA01USM664N", "XTNTVA01GBM664N")
 ## net trade. monthly not seasonally adjusted. broad proxy for current account balance.

Trade <- get_Symbols(Symbols)

Trade <- do.call(merge, Trade)

colnames(Trade) <- c("T_CH", "T_JP", "T_US", "T_UK") 

## check at the currency. All in national currency! Convert them. 
```

########################### 

Merge all of the statistics \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

``` r
data <- merge.xts(FX, bills, CPI, M1, M3, Trade, UNply) ##dagli anni '90 al 2005 hai tutte le statistiche.

rm(list=setdiff(ls(), c("data", "import", "write"))) ## clean the workspace just keeping the data

data <- data["1990-01-01/2006-02-01"] ## extract a complete dataset without NA; notice if you run the analysis with M1 you have complete data until 2013.

length(data[,1]) ## 194 observations per ts
```

    ## [1] 194

``` r
## Transform all the variables in USD currency
```

UK statistics are poor. For the other countries you can run the analysis back to the '70s. For UK because of M3 and especially unemployment rate the comparison is much more limited.

################### 

Data transformation \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

Transform M1, M3 and Trade balance sheets in USD. Compute cumulative Trade balance sheet. Transform also the interest rate in USD interest rate. Assume FX-rate martingale. Otherwise you would have to download the forward interest rate.

``` r
helper1 <- c("M1", "M3", "TB", "T")
for(j in helper1){
                  helper2 <- c("JP", "CH", "UK")
                  help <- function(x){eval(parse(text = x))}
                  for (i in helper2){
                       if(paste0(j, "_", i) != "TB_UK")
                       data[,paste0(j, "_", i)] <- help(paste0("data$", j, "_", i))/help(
                                                              paste0("data$EX_", i, "US"))
                  }
}
rm(helper1)
rm(list = setdiff(ls(), c("data", "import", "write")))
  
##Explaination of the code above; a loop in a loop to paste the code the right way and convert all of the variables into USD values. This will facilitate the comparison and the times series analysis at a later step.
```

Transform Trade to cumulative trade variable

``` r
helper2 <- c("JP", "CH", "UK", "US")
for(j in helper2){
    total = 0
    for(i in 1:length(data$T_CH)){
      total = total + as.numeric(data[i, paste0("T_", j)])
      data[i, paste0("T_", j)] <- total
    }
    
}
rm(list = setdiff(ls(), c("data", "import", "write")))
```

Logarithm transformation

``` r
transformation <- c("M1", "M3", "EX")
helper2 <- c("JP", "CH", "UK", "US")
for(j in transformation){
    for(i in helper2){
      if(j != "EX"){
         data[, paste0(j, "_", i)] <- log(data[, paste0(j, "_", i)])
      }
      else{if(i != "US") data[, paste0(j, "_", i, "US")] <- log(data[, paste0(j, "_", i,
                                                                              "US")])}
    }
    
}
rm(list = setdiff(ls(), c("data", "import", "write")))
```

################################### 

Create independent variables series \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

To do that we have to create new series. Convert to data.frame to make this possible vectorizing.

``` r
data2 <- as.data.frame(data)
```

The series to be created

1.  log(M1\_US/M1\_Foreign)

2.  log(M3\_US/M3\_Foreign)

3.  UN\_US - UN\_Foreign

4.  TB\_US - TB\_Foreign

5.  CPI\_US - CPI\_Foreign

``` r
## as the series are already in log. just take the difference
helper1 <- c("TB", "M3", "UN", "M1", "CPI")
for(i in helper1){
  helper2 <-  c("CH", "JP", "UK") 
  for(j in helper2){
    if((i == "TB") & (j == "UK")) next
    data2[, paste0(i, "_US",  j)] <- 
      data2[,paste0(i,"_US")] - data2[,paste0(i, "_", j)]
  }
} 
```

Convert back to xts

``` r
data <- xts(data2, order.by=index(data))

## clean workspace
rm(list = setdiff(ls(), c("data", "import", "write")))
```

====================== Descriptive Statistics ======================

This section continues with general descriptive statistics trying to understand the behaviour of the exogenous series and trying to test whether the latter are indeed exogenous.

Keep just the series of interes

``` r
data <- cbind(data[,27:ncol(data)], data[,1:3], data[,19:22])
```

Before diving into the descriptive analysis of the series, I will try to transform the series into stationary series so that it will possible to analyze the various series throught linear and non-linear times series models.

In order to transform the series into stationary series I will:

    1. Detrend the series.

    2. Remove the seasonal component.

    3. Test the series for unit roots.

    4. Perform chow-tests to check for structural breaks in the resulting series.

################## 

Part 1: Detrending \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

In a first step I will test the necessity for detrending the series.

Plot the series as a help:

``` r
help <- function(i){
  plot(i, main = colnames(i))
}
lis <- list()
lis <- lapply(data, help)
for(i in 1:ncol(data)){
  if(i == 1) 
    par(mfrow = c(2,1))
  else if((i%%3 == 0) & !(i %in% c(18, 21)))
    par(mfrow = c(3,1))
  else if (i == ncol(data) -3)
    par(mfrow = c(2,2))
  plot(lis[[i]])
}
```

![](R_code_files/figure-markdown_github/unnamed-chunk-20-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-20-7.png)

Plot proxy for variance as a help:

``` r
help <- function(i)plot((i- mean(i))**2, main = colnames(i)) ## notice assumes time invariant mean, which is clearly violated in the series.
lis <- list()
lis <- lapply(data, help)
for(i in 1:ncol(data)){
  if(i == 1)
    par(mfrow = c(3,1))
  else if((i%%3 == 0) & !(i %in% c(18, 21)))
    par(mfrow = c(3,1))
  else if (i == ncol(data) -3)
    par(mfrow = c(2,2))
  plot(lis[[i]])                  
}
```

![](R_code_files/figure-markdown_github/unnamed-chunk-21-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-21-7.png)

The majority of the series are clearly not mean stationary.

For the exchange rate and the money supply it is difficult to judge based on simple plots, these might could appear to be mean stationary but not volatility stationary.

``` r
acf_plots <- function(){
  for(i in 1:ncol(data)){
    if(i == 1)
      par(mfrow = c(2,1), mar = c(4,3,4,2))
    else if((i%%3 == 0) & !(i %in% c(18, 21)))
      par(mfrow = c(3,1), mar = c(4,3,4,2))
    else if (i == ncol(data) -3)
      par(mfrow = c(2,2), mar = c(4,3,4,2))
    acf(na.trim(data[,i]), main = colnames(data[,i]))                 
  }
}

## Plot acf
acf_plots() ## slow decaying autocorrelation points to non-stationarity mean series.
```

![](R_code_files/figure-markdown_github/unnamed-chunk-22-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-22-7.png)

``` r
## The slow decay suggests that the series would be mean varyiant based on time shifts.
```

Step 2: Choose detrending solution

1.  Detrend via differenciation

2.  Detrend via linear time trend

3.  Use a moving average filter, this is a 'low pass' filter since it takes data and removes from it the rapidly fluctuating component to leave the slowly varying estimated trend.

Linear trend will provide to be ineffective for all of the times series.

Perform detrending via differentiation

``` r
data2 <- data ## to play then with original dataset
data3 <- lapply(data, diff)
for(i in 1:ncol(data)){data[,i] <- data3[[i]]}
```

Plot new Variables

``` r
help <- function(i){
  plot(i, main = colnames(i))
}
lis <- list()
lis <- lapply(data, help)
for(i in 1:ncol(data)){
    if(i == 1)
      par(mfrow = c(2,1), mar = c(4,3,4,2))
    else if((i%%3 == 0) & !(i %in% c(18, 21)))
      par(mfrow = c(3,1), mar = c(4,3,4,2))
    else if (i == ncol(data) -3)
      par(mfrow = c(2,2), mar = c(4,3,4,2))
  plot(lis[[i]])
}
```

![](R_code_files/figure-markdown_github/unnamed-chunk-24-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-24-7.png)

Plot proxy for variance for the new variables as a help:

``` r
help <- function(i)plot((i- mean(i, na.rm = T))**2, main = colnames(i)) ## notice assumes time invariant mean, which is clearly violated in the series.
lis <- list()
lis <- lapply(data, help)
for(i in 1:ncol(data)){
    if(i == 1)
      par(mfrow = c(2,1), mar = c(4,3,4,2))
    else if((i%%3 == 0) & !(i %in% c(18, 21)))
      par(mfrow = c(3,1), mar = c(4,3,4,2))
    else if (i == ncol(data) -3)
      par(mfrow = c(2,2), mar = c(4,3,4,2))
  plot(lis[[i]])                  
}
```

![](R_code_files/figure-markdown_github/unnamed-chunk-25-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-25-7.png)

Plot autocorrelation functions

``` r
## Plot acf
acf_plots()
```

![](R_code_files/figure-markdown_github/unnamed-chunk-26-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-26-7.png)

Result: Trade was a disaster. For the rest good improvement. Some adjustment still needed - see TB\_US; possibly double differentiation.

Compute moving average smoother for Trade.

``` r
helper1 <- c("JP", "CH", "UK", "US")

par(mfrow = c(2,2), mar = c(4,4,4,2))
for(i in helper1){
          trend <- ma(data2[, paste0("T_", i)], 
                                       order = 4, centre = T) ##moving average of quartal
          data[, paste0("T_", i)] <- as.ts(data2[, paste0("T_", i)]) - trend
          acf(na.trim(data[,paste0("T_", i)]), main = paste0("T_", i)) 
         
}
```

![](R_code_files/figure-markdown_github/unnamed-chunk-27-1.png)

Check for unit roots in the series performing augmented Dickey-Fuller tests

``` r
help <- function(x){
        dd <- adf.test(na.trim(x))
        if(dd$p.value >= 0.05) dd
}

suppressWarnings(lapply(data, help)) ## supress warning to ignore message that true p-value smaller than the one printed
```

    ## $TB_USCH
    ## NULL
    ## 
    ## $TB_USJP
    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  na.trim(x)
    ## Dickey-Fuller = -3.1375, Lag order = 5, p-value = 0.1001
    ## alternative hypothesis: stationary
    ## 
    ## 
    ## $M3_USCH
    ## NULL
    ## 
    ## $M3_USJP
    ## NULL
    ## 
    ## $M3_USUK
    ## NULL
    ## 
    ## $UN_USCH
    ## NULL
    ## 
    ## $UN_USJP
    ## NULL
    ## 
    ## $UN_USUK
    ## NULL
    ## 
    ## $M1_USCH
    ## NULL
    ## 
    ## $M1_USJP
    ## NULL
    ## 
    ## $M1_USUK
    ## NULL
    ## 
    ## $CPI_USCH
    ## NULL
    ## 
    ## $CPI_USJP
    ## NULL
    ## 
    ## $CPI_USUK
    ## NULL
    ## 
    ## $EX_UKUS
    ## NULL
    ## 
    ## $EX_JPUS
    ## NULL
    ## 
    ## $EX_CHUS
    ## NULL
    ## 
    ## $T_CH
    ## NULL
    ## 
    ## $T_JP
    ## NULL
    ## 
    ## $T_US
    ## NULL
    ## 
    ## $T_UK
    ## NULL

From the above two problematic series:

1.  Treasury bills of US - JP

Solution: double differentiate the both

``` r
data$TB_USJP <- diff(data$TB_USJP)
acf(na.trim(data$TB_USJP))
```

![](R_code_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
adf.test(na.trim(data$TB_USJP)) ## good
```

    ## Warning in adf.test(na.trim(data$TB_USJP)): p-value smaller than printed p-
    ## value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  na.trim(data$TB_USJP)
    ## Dickey-Fuller = -8.7431, Lag order = 5, p-value = 0.01
    ## alternative hypothesis: stationary

Will all of the above corrections the reslut is good and there is strong evidence for stationarity in the series.

Clean workspace and just keep the full dataset

``` r
data <- na.trim(data)
rm(list=setdiff(ls(), c("data", "acf_plots", "import", "write")))
```

Step 3: analyze seasonality and correct for it.

Two possible solutions for adjusting for seasonality after identification of the cyclical frequency:

1.  Seasonal differentiation. Similar idea to the general differentiation of a series.

2.  Subtract the block average of the series in the cycle. Similar idea to moving average smoothers.

Let's test now for the presence of seasonal patterns and let's try to identify the cyclical frequency.

Checking again at autocorrelation plots

``` r
acf_plots()
```

![](R_code_files/figure-markdown_github/unnamed-chunk-31-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-31-7.png)

Well behaved series; adjust for annual seasonality by block differenciating.

``` r
seasonality_12 <- function(x){
                m_series = t(matrix(data = x, nrow = 12))
                season = colMeans(m_series, na.rm = T)
                data[,colnames(x)] <<- x - season
               }

## do an r test of the function tomorrow to check if the syntax is holds for the 13:24 and 25:36 observations.

invisible(suppressWarnings(lapply(data, seasonality_12))) ## invisible does not return the lapply data assignments. The suppressWarnings tells us that for the last period there are not 12 observations and the demeaning is done with the remaining. This is because 190 is not a multiple of 12
```

Check at new adjusted

``` r
acf_plots()
```

![](R_code_files/figure-markdown_github/unnamed-chunk-33-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-33-7.png)

Perform test checking for seasonal unit root at monthly, quartarly frequency.

``` r
seasonality <- function(x){
               ## test for unit root for given seasonality
               dat <- ts(x, frequency = 4) 
               
               # Test for the presence of seasonal patterns at the determined frequency
               res <- nsdiffs(dat)
}

lapply(data, seasonality) ## good both quartely and monthly.
```

    ## $TB_USCH
    ## [1] 0
    ## 
    ## $TB_USJP
    ## [1] 0
    ## 
    ## $M3_USCH
    ## [1] 0
    ## 
    ## $M3_USJP
    ## [1] 0
    ## 
    ## $M3_USUK
    ## [1] 0
    ## 
    ## $UN_USCH
    ## [1] 0
    ## 
    ## $UN_USJP
    ## [1] 0
    ## 
    ## $UN_USUK
    ## [1] 0
    ## 
    ## $M1_USCH
    ## [1] 0
    ## 
    ## $M1_USJP
    ## [1] 0
    ## 
    ## $M1_USUK
    ## [1] 0
    ## 
    ## $CPI_USCH
    ## [1] 0
    ## 
    ## $CPI_USJP
    ## [1] 0
    ## 
    ## $CPI_USUK
    ## [1] 0
    ## 
    ## $EX_UKUS
    ## [1] 0
    ## 
    ## $EX_JPUS
    ## [1] 0
    ## 
    ## $EX_CHUS
    ## [1] 0
    ## 
    ## $T_CH
    ## [1] 0
    ## 
    ## $T_JP
    ## [1] 0
    ## 
    ## $T_US
    ## [1] 0
    ## 
    ## $T_UK
    ## [1] 0

Perform FFT to check for possible other seasonal patterns

``` r
seasonality <- function(x){
               p = periodogram(x, plot = F) ## short lag determinant
               dd = data.frame(freq=p$freq, spec=p$spec)
               order = dd[order(-dd$spec),]
               top = head(order, 1)
               
               # convert frequency to time periods
               time = round(1/top$f)
               
               time[1]
}

lapply(data, seasonality)
```

    ## $TB_USCH
    ## [1] 8
    ## 
    ## $TB_USJP
    ## [1] 3
    ## 
    ## $M3_USCH
    ## [1] 11
    ## 
    ## $M3_USJP
    ## [1] 11
    ## 
    ## $M3_USUK
    ## [1] 4
    ## 
    ## $UN_USCH
    ## [1] 192
    ## 
    ## $UN_USJP
    ## [1] 3
    ## 
    ## $UN_USUK
    ## [1] 3
    ## 
    ## $M1_USCH
    ## [1] 3
    ## 
    ## $M1_USJP
    ## [1] 11
    ## 
    ## $M1_USUK
    ## [1] 96
    ## 
    ## $CPI_USCH
    ## [1] 6
    ## 
    ## $CPI_USJP
    ## [1] 6
    ## 
    ## $CPI_USUK
    ## [1] 5
    ## 
    ## $EX_UKUS
    ## [1] 17
    ## 
    ## $EX_JPUS
    ## [1] 11
    ## 
    ## $EX_CHUS
    ## [1] 11
    ## 
    ## $T_CH
    ## [1] 3
    ## 
    ## $T_JP
    ## [1] 3
    ## 
    ## $T_US
    ## [1] 3
    ## 
    ## $T_UK
    ## [1] 4

``` r
## Trimestral frequency is worth to test.
```

Good. No other seasonal unit roots are displayed.

Clean workspace

``` r
rm(list=setdiff(ls(), c("data", "acf_plots", "import", "write")))
```

Before starting with the implementation of the linear modelling of the series I perform a chow test to understand the possibility of structural breaks in the sample that would suggest prudence when working with the series as a whole.

This is especially important as the period analyzed involves the outbreak of the dot-com bubble, which called for aggressive monetary policies and might significantly affect the results.

``` r
prova <- function(x){
  
   ## I test against the alternative that each series can be fitted with a single constant     against the alternative of structural breaks that would call for multiple constants in      the sample.
  
  ## Using CUSUM
  ocus.x <- efp(as.ts(x) ~ 1, type = "OLS-CUSUM")
  
  #if(i == 1)
    #par(mfrow = c(2,1), mar = c(4,3,4,2))
  #else if((i%%3 == 0) & !(i %in% c(18, 21)))
    #par(mfrow = c(3,1), mar = c(4,3,4,2))
  #else if (i == ncol(data) -3)                ### search how to count over iterations of lapply
    #par(mfrow = c(2,2), mar = c(4,3,4,2))
  plot(ocus.x, main = colnames(x))
  
  ## Using dynamic programming of <cite> 
  ## https://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Kleiber+Kraemer-2003.pdf

  # store the breakdates
  #bp_ts <- breakpoints(x~ 1)

  # this will give you the break dates and their confidence intervals
  #summary(bp_ts) 
  
  #bp_ts$breakpoints

  # store the confidence intervals
  #ci_ts <- tryCatch(confint(bp_ts), error=function(e) NULL)
}

lapply(data, prova)
```

![](R_code_files/figure-markdown_github/unnamed-chunk-37-1.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-2.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-3.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-4.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-5.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-6.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-7.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-8.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-9.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-10.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-11.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-12.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-13.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-14.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-15.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-16.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-17.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-18.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-19.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-20.png)![](R_code_files/figure-markdown_github/unnamed-chunk-37-21.png)

    ## $TB_USCH
    ## NULL
    ## 
    ## $TB_USJP
    ## NULL
    ## 
    ## $M3_USCH
    ## NULL
    ## 
    ## $M3_USJP
    ## NULL
    ## 
    ## $M3_USUK
    ## NULL
    ## 
    ## $UN_USCH
    ## NULL
    ## 
    ## $UN_USJP
    ## NULL
    ## 
    ## $UN_USUK
    ## NULL
    ## 
    ## $M1_USCH
    ## NULL
    ## 
    ## $M1_USJP
    ## NULL
    ## 
    ## $M1_USUK
    ## NULL
    ## 
    ## $CPI_USCH
    ## NULL
    ## 
    ## $CPI_USJP
    ## NULL
    ## 
    ## $CPI_USUK
    ## NULL
    ## 
    ## $EX_UKUS
    ## NULL
    ## 
    ## $EX_JPUS
    ## NULL
    ## 
    ## $EX_CHUS
    ## NULL
    ## 
    ## $T_CH
    ## NULL
    ## 
    ## $T_JP
    ## NULL
    ## 
    ## $T_US
    ## NULL
    ## 
    ## $T_UK
    ## NULL

================================ Univariate times series analysis ================================

This section starts with the analysis of the cleaned dataset.

In a first step I will try to check whether the Meese-Rogoff paradox holds in the analyzed time period. I will work with the USA as the benchmark series and apply the different macroeconomics models to linearly estimate the FX\_rates.

General model:

EX = const. + b1 M\_USFR + b2 UN\_USFR + b3 TB\_USFR + b4 CPI\_USFR + b5(TB\_US) + b6 TB\_FR

I will test the different models:

1.  Frenkel Bilson -&gt; b4, b5, b6 = 0

2.  Dornbusch-Frankel -&gt; b5, b6 = 0

3.  Hooper Morton -&gt; no constraint

As in the case of Meese and Rogoff forecasting will be performed at one, three, six and twelwe months.

Cite Meese and Rogoff in the thesis: The purpose of considering multiple forecast horizons in this type of experiment is to see whether the structural models do better than time series models in the long run, when adjustment due to lags and/or a serially correlated error term has taken place. Of course, when lags and serial correlation are fully incorporated into the structural models, a consistently estimated true structural model will outpredict a time series model at all horizons in a large sample.

=============== Random walk fit ===============

Random walk fit code below is horribly complex. Define helper functions and simplify if you have time.

Random walk without drift prediction using rolling series

``` r
## In the other models I will use 3/4 of the sample to train the data and the rest to predict. Be consistent to facilitate comaprison at a later stage.
three_fourth <- round(nrow(data)*(3/4))

## to create new series transform in data frame as before

data2 <- as.data.frame(data)

for(iPred in c(1,3,6,12)) 
  { 
    helper <- c("JP", "UK", "CH")
    for(jCountry in helper)
    {
    data2[, paste0("rwPred" , iPred, "_", jCountry, "US")] <- NA
    }
  } 

data2 <- as.xts(data2, order.by = index(data))## transform back to xts
data <- data2
rm(data2)
```

Fit the Random Walk prediction

``` r
for(iPred in c(1,3,6,12)) 
  { 
    helpCountry <- c("JP", "UK", "CH")
    for(jCountry in helpCountry)
    {
        for(kRow in three_fourth:nrow(data)) 
        {
         if ((iPred != 1) & (kRow+(iPred-2) == nrow(data))) break
         data[kRow+(iPred-1), paste0("rwPred" , iPred, "_", jCountry, "US")] <- 
           data[kRow-1, paste0("EX_", jCountry, "US")] 
        }
    }
  } 
```

============================== Macroeconomic structural model ==============================

In contrast to Meese and Rogoff I decided to make use of the ARIMAX model here. This will allow the introduction of lagged exogenous variables and its implementation is favoured compared to the long lagged AR model used by Meese and Rogoff to my mind.

=============================== Vector Autoregressive Modelling ===============================

``` r
data <- as.data.frame(data) ## package tsDyn cannot deal with xts objects

endogenous <- c("EX_UKUS", "EX_JPUS", "EX_CHUS")
notexogenous <- str_extract(colnames(data), "^rw|^EX")
notexogenous <- notexogenous %in% c("rw", "EX")
exogenous <- !notexogenous

VARselect(data[1:three_fourth, endogenous], lag.max = 7,
          type = "none", exogen = data[1:three_fourth, exogenous]) 
```

    ## $selection
    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##      1      1      1      1 
    ## 
    ## $criteria
    ##                    1             2             3             4
    ## AIC(n) -2.896094e+01 -2.891765e+01 -2.888420e+01 -2.881903e+01
    ## HQ(n)  -2.840998e+01 -2.828798e+01 -2.817583e+01 -2.803194e+01
    ## SC(n)  -2.760514e+01 -2.736817e+01 -2.714104e+01 -2.688218e+01
    ## FPE(n)  2.665279e-13  2.793835e-13  2.903164e-13  3.118033e-13
    ##                    5             6             7
    ## AIC(n) -2.889591e+01 -2.885643e+01 -2.882365e+01
    ## HQ(n)  -2.803012e+01 -2.791193e+01 -2.780045e+01
    ## SC(n)  -2.676538e+01 -2.653221e+01 -2.630575e+01
    ## FPE(n)  2.909560e-13  3.055083e-13  3.192294e-13

``` r
## one lag the best

model <- lineVar(data[1:three_fourth, endogenous], 1, include = "none", 
                 model = "VAR", I = "level",
                 estim = "ML", LRinclude = "none", 
                 exogen = data[1:three_fourth, exogenous])
```

Create columns for inserting prediction results

``` r
for(iPred in c(1,3,6,12)) 
  { 
    helper <- c("JP", "UK", "CH")
    for(jCountry in helper)
    {
    data[, paste0("varPred" , iPred, "_", jCountry, "US")] <- NA
    }
  } 
```

Rolling prediction using VAR

``` r
# number rolling prediction per forecasting time frame
for(i in c(1,3,6,12))
  {
  assign(paste0("numprediction", i), sum(!is.na(data[, paste0("rwPred", i, "_JPUS")])))
  }

## Estimate the rolling window forecast
for(j in c("UK", "JP", "CH")) 
  {
    for(i in c(1,3,6,12))
    {
    parsed_text1 <- eval(parse(text=paste0("numprediction", i)))
    assign(paste0("endRow",i), nrow(data) +1 - parsed_text1)
    assign(paste0("pred_model", i), 
           predict_rolling(model, nroll = parsed_text1, n.ahead = i))
    parsed_text2 <- eval(parse(text=paste0("pred_model", i)))
    parsed_text3 <- eval(parse(text=paste0("endRow", i)))
    if(j == "UK") list_member = 1
    if(j == "JP") list_member = 2
    if(j == "CH") list_member = 3
    data[parsed_text3:nrow(data), 
         paste0("varPred", i, "_", j, "US")] <- parsed_text2$pred[list_member]
    }
  }
```

TO DO: Would be nice to calculate the degrees of freedom in order to understand how reliable the model is.

Check at variance decomposition of the VAR to see whether the series present hgih endogeneity in the sense that the vector moving average model implied by the VAR modle suggests a high degree of spillovers from one variable to the next.

``` r
plot(vars::fevd(model, ortho = T))
```

![](R_code_files/figure-markdown_github/unnamed-chunk-44-1.png)

Endogeneity can be well ruled out in the series for the UKUS dollar and JPUS foreign exchange rates. This does not hold strictly for the CHUS foreign exchange rate that seems to be well infulenced by the UKUS forex evolution.

Clean working space

``` r
rm(list=setdiff(ls(), c("data", "acf_plots", "import", "write")))
```

Write data to the computer

``` r
write(data)
```

Make cusum test on random walk to check for structural breaks in the series

Make use of Dibold Mariano test to test the possibility of equal predictability of the methods.
