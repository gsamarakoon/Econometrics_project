library(tidyverse)
library(readxl)
library(stats)
library(tseries)
library(forecast)
library(tidyquant)
library(ggfortify)

DF <- read_excel(path = "C:/Users/samar/Downloads/CSUSHPINSA.xlsx") %>% 
    
    rename(us_home_price_index = CSUSHPINSA,
           date = DATE)
plot
# Exploratory

plot <- DF %>% 
ggplot(aes(x = date, y = us_home_price_index)) +
geom_line() +
geom_smooth(method = "lm", se = FALSE) +

# Formmating
theme_tq() +
labs(
    title = "S&P/Case-Shiller U.S. National Home Price Index: 01-01-1987 to 01-06-2019",
    subtitle = "Overall upwards trends due to inflation/increased GDP, with a big dip due to the great recession ",
    x = "Date (years)",
    y = " US National Home Price Index",
    fill = ""
)

# Convert the data frame to a time series object
DF_ts <- ts(DF$us_home_price_index, start=1987, frequency=12)

#1 Implement the Augmented Dickey-Fuller Test for checking the existence of a unit root in Case-Shiller Index series
acf_results <- acf(DF_ts)
pacf_results <- pacf(DF_ts)
adf_results <- adf.test(DF_ts)


acf_results
pacf_results
adf_results




#2 Implement an ARIMA(p,d,q) model. Determine p, d, q using Information Criterion or Box-Jenkins methodology. Comment results
# fit a simple AR model with 12 lags, no differencing, no moving average terms - i.e. an ARIMA(12,0,0) model:
    AR_model1 <- arima(window(DF_ts,start=1987),
                       order=c(12,0,0), method = "ML")
summary(AR_model1)
# ARIMA (12,2,2)
ARIMA_model <- arima(window(DF_ts,start=1987),
                     order=c(12,2,2), method = "ML")
summary(ARIMA_model)
Box.test(ARIMA_model$residuals, lag = 12)

 
predictive_model <- auto.arima(window(DF_ts, start=1987), stationary = TRUE, trace = TRUE)
predictive_non_stationary <- auto.arima(window(DF_ts, start=1987), trace = TRUE)

predictive_model
predictive_non_stationary
ggtsdiag(predictive_non_stationary)

Box.test(predictive_non_stationary$residuals, lag = 3)

checkresiduals(predictive_non_stationary)

#3 Forecast the future evolution of Case-Shiller Index using the ARMA model. Test model using in-sample forecasts
fr<- forecast::forecast(predictive_non_stationary)
plot(fr)

#4 Suggest exogenous variables that can improve forecasts
#Interest rates, unemployment rates, National GDP, Global GDP, Gold prices, Inflation rates, population growth
#Disposable income levels, rental rates, Momentum, Shadow Inventory change
# Reference: https://www.spratings.com/documents/20184/86966/HPI+Article/6d1d10de-8c11-4506-81c8-216abb7ae455

