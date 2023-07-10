version[['version.string']]
# Loading Required Prerequisites

library(tidyverse)
library(tidyquant)
library(gridExtra)
library(tibbletime)
library(forecast)
library(itsmr)
library(here)
library(fpp2)
library(tseries)
library(ggplot2)
library(ggthemes)
library(patchwork)

#set theme
theme_set(theme_gdocs())
theme_set(theme_stata())
  

df = read.csv('inflation_data.csv')
head(df)

# Rural Consumer Price Index

#cDecomposition: Decomposing the Rural Consumer Price Index (CPI) into different time series components:
rural <- ts(df$ind_r, start=c(2013,1),frequency = 12)
forecast::autoplot(decompose(rural,type="multiplicative"))

#  From the above graph, it is clear that the rural CPI has an influence of trend and seasonal components. Those factors are need to be removed in order to fit a model over the data.
rur_decomp <- stl(rural, s.window = 12) # estimate the seasonal component
rur_adjseason <- rural - seasonal(rur_decomp) #deseason the data
plot1 <- autoplot(rur_adjseason,ylab='Index Value',main='Deseasonalized Rural Index Plot') #plot
plot2 <- ggAcf(rur_adjseason) + ggtitle('ACF of Deseasonalized Rural Index')
plot3 <- ggPacf(rur_adjseason) + ggtitle('PACF of Deseasonalized Rural Index')
plot1 /(plot2 | plot3)

  
  
# Model Building: Fitting the best fitted SARIMA model over the Rural Index Number

rur_sarima <- auto.arima(rural, stepwise = F, approximation = F, seasonal = T, allowdrift = F,
                           parallel = F, trace = F)
summary(rur_sarima)

plot(rur_sarima) # inspect the roots
plot1 <- autoplot(residuals(rur_sarima),ylab='residuals',main='Residuals of SARIMA model of Rural Index') 
plot2 <- ggAcf(residuals(rur_sarima)) +  ggtitle('ACF of Residuals')
plot3 <- ggPacf(residuals(rur_sarima))+  ggtitle('PACF of Residuals')

plot1 / (plot2 | plot3)

#  After successfully fitting the SARIMA model, the periodicity in the residuals have been removed which can be shown in the ACF and PACF plots.

# Forecasting: Performing forecasting from the given data

train_rur <- window(rural, end=c(2022,8))
test <- window(rural, start=c(2022,9))
sarimab2 <- auto.arima(train_rur,stepwise = F, approximation = F, seasonal = T, allowdrift = F, 
                       parallel = F, trace = F)

forecasts <- forecast::forecast(sarimab2, h = 40)
fore1 <-autoplot(forecasts)+autolayer(rural) + ggtitle("Rural CPI Number")
forecast::accuracy(forecasts,test)

  
  
# Urban Consumer Price Index
# Decomposition: Decomposing the Urban Consumer Price Index (CPI) into different time series components:
urban <- ts(df$ind_u, start=c(2013,1),frequency = 12)
forecast::autoplot(decompose(urban,type="multiplicative"))

#  From the above graph, it is clear that the urban CPI has an influence of trend and seasonal components. Those factors are need to be removed in order to fit a model over the data.

ur_decomp <- stl(urban, s.window = 12) # estimate the seasonal component
ur_adjseason <- urban - seasonal(rur_decomp) #deseason the data
plot1 <- autoplot(ur_adjseason,ylab='Index Value',main='Deseasonalized Urban Index Plot') #plot
plot2 <- ggAcf(ur_adjseason) + ggtitle('ACF of Deseasonalized Urban Index')
plot3 <- ggPacf(ur_adjseason) + ggtitle('PACF of Deseasonalized Urban Index')
plot1 /(plot2 | plot3)

# Model Building: Fitting the best fitted SARIMA model over the Urban Index Number

ur_sarima <- auto.arima(urban, stepwise = F, approximation = F, seasonal = T, allowdrift = F,
                          parallel = F, trace = F)
summary(ur_sarima)

  
plot(ur_sarima) # inspect the roots
plot1 <- autoplot(residuals(ur_sarima),ylab='residuals',main='Residuals of SARIMA model of Urban Index')
plot2 <- ggAcf(residuals(ur_sarima))+ ggtitle('ACF of Residuals')
plot3 <- ggPacf(residuals(ur_sarima)) + ggtitle('PACF of Residuals')

plot1 / (plot2 | plot3)

#  After successfully fitting the SARIMA model, the periodicity in the residuals have been removed which can be shown in the ACF and PACF plots.

# Forecasting: Performing forecasting from the given data.
train_ur <- window(urban, end=c(2022,8))
test <- window(urban, start=c(2022,9))
sarimab2 <- auto.arima(train_ur,stepwise = F, approximation = F, seasonal = T, allowdrift = F, 
                       parallel = F, trace = F)

forecasts <- forecast::forecast(sarimab2, h = 40)
fore2 <-autoplot(forecasts)+autolayer(urban)+ ggtitle("Urban CPI Number")
forecast::accuracy(forecasts,test)

# Rural and Urban Consumer Price Index
# Decomposition: Decomposing the Rural and Urban Combined Consumer Price Index (CPI) into different time series components:

ru <- ts(df$ind_ru, start=c(2013,1),frequency = 12)
#options(repr.plot.width=3,repr.plot.height=3)
forecast::autoplot(decompose(ru,type="multiplicative"))

#  From the above graph, it is clear that the rural and urban combined CPI has an influence of trend and seasonal components. Those factors are need to be removed in order to fit a model over the data.

ru_decomp <- stl(ru, s.window = 12) # estimate the seasonal component
ru_adjseason <- ru - seasonal(ru_decomp) #deseason the data
plot1 <- autoplot(ru_adjseason,ylab= 'Index Value', main='Deseasonalized Rural & Urban Index Plot') #plot
plot2 <- ggAcf(ru_adjseason) + ggtitle('ACF of Deseasonalized Rural & Urban Index')
plot3 <- ggPacf(ru_adjseason) + ggtitle('PACF of Deseasonalized Rural & Urban Index')
plot1 /(plot2 | plot3)

# Model Building: Fitting the best fitted SARIMA model over the Rural and Urban Combined Index Number
ru_sarima <- auto.arima(ru, stepwise = F, approximation = F, seasonal = T, allowdrift = F,
                          parallel = F, trace = F)
summary(ru_sarima)


plot(ru_sarima) # inspect the roots
plot1 <- autoplot(residuals(ru_sarima),ylab='residuals',main='Residuals of SARIMA model of Rural & Urban Index')
plot2 <- ggAcf(residuals(ru_sarima))+ ggtitle('ACF of Residuals')
plot3 <- ggPacf(residuals(ru_sarima))+ ggtitle('PACF of Residuals')

plot1 / (plot2 | plot3)

# After successfully fitting the SARIMA model, the periodicity in the residuals have been removed which can be shown in the ACF and PACF plots.

# Forecasting: Performing forecasting from the given data.}
train_ru <- window(ru, end=c(2022,8))
test <- window(ru, start=c(2022,9))
sarimab2 <- auto.arima(train_ru,stepwise = F, approximation = F, seasonal = T, allowdrift = F, 
                       parallel = F, trace = F)

forecasts <- forecast::forecast(sarimab2, h = 40)
fore3 <- autoplot(forecasts)+autolayer(ru)+ ggtitle("Rural & Urban CPI Number")
forecast::accuracy(forecasts,test)

# Forecasts for the given Index numbers
  fore3 / (fore1 | fore2)

