# Exchange-Rates-Modelling

Author: Marco Hassan

**Work in progress..**

Download the data using quantmod. Multiple data provider: OECD, FED, SNB, BJP etc...

## Data cleaning:

Step 1: detrend the data series.

Step 2: check the seasonality frequency using FFT

Step 3: remove seasonality

Step 4: Run unit root tests

Step 5: Perform structural break tests

## Linear Modelling:

Step 1: Random Walk forecasting with multiple lags.

Step 2: VAR 
    
      2.1 Lag estimation via information criteria and likelihood ratio tests.
      
      2.2 Model estimation via maximum likelihood. 
      
      2.3 Variance decomposition of the implied impulse response and the within implied vector moving average model in order to check
      for endogeneity in the series.

Step 3: ARIMAX

## Out of Sample Performance:

#### In detrended and seasonally adjusted series

Step 1: Plot the different predictions to get a feeling of the model

![image](https://user-images.githubusercontent.com/42472072/53882217-689fa400-401e-11e9-9ae9-d06cb6e15cd4.png)

Step 2: Compute statistics for different distance measures from the realizations

Step 3: Perform an analysis of the predictive power using the Diebold Mariano test


#### In levels, with cointegration analysis and VECM

![image](https://user-images.githubusercontent.com/42472072/53899331-a44c6500-4042-11e9-95f6-7cbecded3982.png)

