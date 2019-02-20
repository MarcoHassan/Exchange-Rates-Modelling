# Exchange-Rates-Modelling

Author: Marco Hassan

**Work in progress..**

Download the data using quantmod. Multiple data provider: OECD, FED, SNB, BJP etc...

Data cleaning:

Step 1: detrend the data series.

Step 2: check the seasonality frequency using FFT

Step 3: remove seasonality

Step 4: Run unit root tests

Step 5: Perform structural break tests

Linear Modelling:

Step 1: Random Walk forecasting with multiple lags.

Step 2: VAR 
    
      2.1 Lag estimation via information criteria and likelihood ratio tests.
      
      2.2 Model estimation via maximum likelihood. 
      
      2.3 Variance decomposition of the implied impulse response and the within implied vector moving average model in order to check
      for endogeneity in the series.
