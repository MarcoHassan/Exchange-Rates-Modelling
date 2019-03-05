####################
# Useful functions #
####################

## Function 1 ##

# helps to evalue paste0 expressions and to evalue them directly without 
# interpreting them as text.
evalue <- function(text){eval(parse(text = text))}

#################################################################################

## Function 2 ##

#write the data to a csv in the working directory

write <- function(dat, name)
{
  write.csv(as.data.frame(dat), file = paste0("../Written\ Data/", name)) 
  ## as.data.frame is necessary to get the date index out of the xts
}

#################################################################################

## Function 3 ##

# Specify helper function to extract data leveraging quantmod API interaction.

get_Symbols <- function(x)
{
  lapply(x, function(sym)
  {getSymbols(sym, src = "FRED",
              return.class = "xts",
              auto.assign = F)  ## auto.assign specifies that not the ticker should be passed
  }) 
}

#################################################################################

## Function 4 ##

# Prints all of the series for which the adf test rejects stationarity with 95% confidence

adfTest <- function(x)
{
  dd <- adf.test(na.trim(x))
  if(dd$p.value >= 0.05) dd
}

#################################################################################

## Function 5 ##

# Test for seasonality roots at the defined frequency

seasonality <- function(x, freq)
{
  ## test for unit root for given seasonality
  dat <- ts(x, frequency = freq) 
  
  # Test for the presence of seasonal patterns at the determined frequency
  res <- nsdiffs(dat)
}

#################################################################################

## Function 6 ##

# Analyse the most suspicious seasonality patterns for each series

suspectFrequency <- function(x)
{
  p = periodogram(x, plot = F) ## short lag determinant
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  top = head(order, 1)
  
  # convert frequency to time periods
  time = round(1/top$f)
  
  time[1]
}

#################################################################################

## Function 7 ##

# Perform a superior predictive ability check based on Hansen, 2005 JBES,
# Subsequently it extract p-values for the spa with different benchmark models based 
# on the bootstrap technique explaind by Hansen, Lunde, Nason - 2011 Econometrica -.
# Based on the obtained p-values the MCS with confidence 1-a is inferred.

spa <-function(per=perf, column_benchmark_model, num_predmodels, number_predictions,q=0.25, iter = 1, periodogram=T) 
{
  e<-column_benchmark_model # specify benchmark model
  d<-matrix(NA ,number_predictions, num_predmodels-1) # create empty matrix containing the error
  
  s<-0
  for (i in seq(1,num_predmodels,1)[-e])
  {
    s<-s+1
    d[,s]<-per[,e]-per[,i] ## compute the difference loss function
  }
  
  #colMeans(d)
  
  w<-rep(0,num_predmodels-1)
  for (k in 1:(num_predmodels-1))
  {
  e<-acf(d[,k],lag.max=number_predictions-1,type="covariance",plot=F)$acf
    if (periodogram==F)
    {
      w[k]<-sqrt(e[1]+2*sum(((number_predictions-seq(1,number_predictions-1,1))/number_predictions*(1-q)^{seq(1,number_predictions-1,1)}+seq(1,number_predictions-1,1)/number_predictions*(1-q)^{number_predictions-seq(1,number_predictions-1,1)})*e[2:number_predictions]))
    }
    else if (periodogram==T)
    {
      w[k]<-sqrt(spectrum(d[,k],plot=F)$spec[1])
    }
  }
  
  stat<-max(0,max(sqrt(number_predictions)*colMeans(d)/w))
  
  #Bootstrap:
  
  stat.boos<-rep(0,iter)
  for (r in 1:iter)
  {
    tau<-rep(0,number_predictions)
    tau[1]<-as.integer(number_predictions*runif(1))+1
    for (i in 2:number_predictions)
    {
      s<-runif(1)
      tau[i]<-(as.integer(number_predictions*runif(1))+1)*(s<q)+((tau[i-1]<number_predictions)*tau[i-1]+1)*(s>=q)
    }
    
    d.boos<-d[tau,] ## select the bootstrapped difference in the loss function. Understand better how the tau is computed.
    
    e<-d
    for (k in 1:(num_predmodels-1))
    {
      e[,k]<-d.boos[,k]-mean(d[,k])*(mean(d[,k])>= - sqrt(w[k]^2/number_predictions*2*log(log(number_predictions))))
    }
    
    stat.boos[r]<-max(0,max(sqrt(number_predictions)*colMeans(e)/w))
  }
  
  p.value<-mean((stat.boos>stat))
  
  list(p.value=p.value,stat.boos=stat.boos,stat=stat)
}