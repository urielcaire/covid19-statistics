library(forecast)
library(ggplot2)
require(data.table)
library(comprehenr)
library(urca)
require(tseries)

home <- getwd()

covid19 <- paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# Sort the data by date
covid19 <- covid19[order(as.numeric(as.Date(covid19$date, format = "%Y-%m-%d"))),]
# Getting first date of a 'new_death' report
day_zero <- first(covid19[covid19$new_deaths>=1,'date'])
day_zero
# Filtering data by day_zero
covid19 <- covid19[covid19$date>=as.Date(day_zero),]

# Getting the initial week
start_week <- as.numeric(strftime(as.Date(day_zero), format = '%U'))
start_week
# Getting initial day position in week range (1:7)
start_day <- as.numeric(strftime(as.Date(day_zero), format = '%w')) + 1
start_day
# Transforms data into daily TimeSeries with a weekly frequency
#https://otexts.com/fpp2/tspatterns.html
new_deaths <- ts(covid19[,c('new_deaths')], start = c(start_week, start_day),
                 frequency = 7)
new_deaths

################################################################################
# Exploratory Analysis
################################################################################

# Print some infos about new_deaths
summary(new_deaths)
# Plot it (we can observer a seasonality and "changing direction")
autoplot(new_deaths) + ggtitle("Óbitos por COVID-19 no Brasil (2020-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +  xlab("Semana") +
  ylab("Número de Óbitos") # plot just it
# Plot a histogram
hist(new_deaths)
# Plot a box plot
boxplot(new_deaths)

# Plot season
#it shows a
#number of deaths increases around week 58ª
#https://otexts.com/fpp2/seasonal-plots.html
ggseasonplot(new_deaths, season.labels = c("Dom","Seg","Ter","Qua","Qui","Sex","Sab"),
             polar=TRUE) +
  ylab("Número de Óbitos") +
  xlab("Dia da Semana") +
  ggtitle("Gráfico de sasonalidade: óbitos por Covid19") +
  scale_color_discrete(name= "Semana (2020-2021)", labels=paste0(11:72,"ª"))

# Seasonal subseries plots
#The horizontal lines indicate the means for each day
#https://otexts.com/fpp2/seasonal-subseries-plots.html
ggsubseriesplot(new_deaths, labels = c("Dom","Seg","Ter","Qua","Qui","Sex","Sab")) +
  ylab("Número de Óbitos") +
  xlab("Dia da Semana") +
  ggtitle("Gráfico de subséries sazonais: óbitos por Covid19")

# Plot lag plot (plotar o valor de um dado em relação ao seu antecessor)
#The relationship is strongly positive at lags 7 and 14,
#reflecting seasonality in the data
#https://otexts.com/fpp2/lag-plots.html
gglagplot(new_deaths, 16, do.lines = FALSE)

# Plot correlogram
#autocorrelation larger for lags multiples of 7, again indicating seasonality
#there is a decrease in the ACF as the lags increase, indicating a trend
#https://otexts.com/fpp2/autocorrelation.html
#https://rpubs.com/hudsonchavs/fac_facp#:~:text=FUN%C3%87%C3%83O%20DE%20AUTOCORRELA%C3%87%C3%83O&text=onde%20Var(r,rt%20%C3%A9%20fracamente%20estacion%C3%A1rio.
ggAcf(new_deaths)

# "Time series that show no autocorrelation are called white noise."
#so it is not an "white noise"
#https://otexts.com/fpp2/wn.html

# Plot TS seasonality infos(data, seasonal, trend, remainder)
#seasonal component does not change over time: we should use additive decomposition
#yt=St+Tt+Rt
#https://otexts.com/fpp2/components.html
plot(stl(new_deaths, "periodic"))

# Apply decomposition and Plot TS infos(data, seasonal, trend, remainder)
#STL will handle any type of seasonality
#t.window is the number of consecutive observations to be used when estimating the
#trend-cycle
#seasonal component does not change over time: we should use additive decomposition
#https://otexts.com/fpp2/stl.html
new_deaths %>%
  stl(t.window=21, s.window="periodic", robust=TRUE) %>%
  autoplot()
#getting decomposed values
decomposed_ts <- stl(new_deaths, t.window=21, s.window="periodic", robust=TRUE)
new_deaths_season <- decomposed_ts$time.series[,1]
new_deaths_trend  <- decomposed_ts$time.series[,2]
new_deaths_random <- decomposed_ts$time.series[,3]

# Saving the dataframe as a .csv file
#write.table(new_deaths, paste(home, '/data/datasets/new_deaths.csv', sep=''),
#sep = ',', row.names = FALSE, quote=FALSE)

# TO DO:
# Measuring strength of trend and seasonality
#https://otexts.com/fpp2/seasonal-strength.html
#max(0.1 - (var(new_deaths_random/(var(new_deaths_trend+new_deaths_random)))))
#max(0.1 - (var(new_deaths_random/(var(new_deaths_season+new_deaths_random)))))

################################################################################
# Preprocessing 
################################################################################
# Replace zeros with 14-day average deaths
0 %in% new_deaths # TRUE
length(new_deaths)
new_deaths_p <- new_deaths
for(i in 1:length(new_deaths_p)) {
  if(new_deaths_p[i]==0){
    past <- sum(new_deaths_p[(i-7):(i-1)])
    post <- sum(new_deaths_p[(i+1):(i+7)])
    new_deaths_p[i] <- as.integer((past + post)/14)
  }
}
0 %in% new_deaths_p # FALSE

# Apply decomposition and Plot TS infos(data, seasonal, trend, remainder)
new_deaths_p %>%
  stl(t.window=21, s.window="periodic", robust=TRUE) %>%
  autoplot()
#getting decomposed values
decomposed_ts_p <- stl(new_deaths_p, t.window=21, s.window="periodic", robust=TRUE)
new_deaths_season_p <- decomposed_ts_p$time.series[,1]
new_deaths_trend_p  <- decomposed_ts_p$time.series[,2]
new_deaths_random_p <- decomposed_ts_p$time.series[,3]

# TO DO:
# Mathematical transformations: to apply log
# "(...) If the data show variation that increases or decreases with the level of
# the series, then a transformation can be useful"
#https://otexts.com/fpp2/transformations.html#mathematical-transformations
(lambda <- BoxCox.lambda(new_deaths_p))
autoplot(BoxCox(new_deaths_p,lambda))

################################################################################
# FORECASTING (Simple Methods)
################################################################################

# Average method
#https://otexts.com/fpp2/simple-methods.html#average-method
meanf_model <- meanf(new_deaths_p, h=7)
# Seasonal naïve method
#https://otexts.com/fpp2/simple-methods.html#seasonal-na%C3%AFve-method
snaive_model <- snaive(new_deaths_p, drift=FALSE, h=7)
# Seasonal naive drift method
#https://otexts.com/fpp2/simple-methods.html#drift-method
naive_drift_model <- rwf(new_deaths_p, h=7, drift=TRUE)

autoplot(window(new_deaths_p, start=c(60,1))) +
  autolayer(meanf_model,
    series="Mean", PI=FALSE) +
  autolayer(snaive_model,
    series="Seasonal Naïve", PI=FALSE) +
  autolayer(naive_drift_model,
    series="Naïve Drift", PI=FALSE) +
  ggtitle("COVID-19 Simple methods forecasting") +
  xlab("Semana") + ylab("Número de Óbitos") +
  guides(colour=guide_legend(title="Forecast"))


################################################################################
# FORECASTING
################################################################################
# USING STL DECOMPOSITION
#https://otexts.com/fpp2/forecasting-decomposition.html
# Naïve forecasts of the seasonally adjusted data from an STL decomposition
naive_model <- stl(subset(new_deaths_p,end=length(new_deaths)-14), t.window=21,
                   s.window="periodic", robust=TRUE)
naive_model %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

# Adding the seasonal naïve forecasts of the seasonal component
naive_model %>% forecast(method="naive") %>%
  autoplot() + ylab("Número de Óbitos") + xlab('Semana') +
  ggtitle("Previsão (forecast) usando STL + Passeio Aleatório (Random Walk)")

# Forecast from STL using ETS method
ets_model <- stlf(new_deaths_p, t.window=21, s.window="periodic", robust=TRUE)
autoplot(ets_model, include = 70) + ylab("Número de Óbitos") + xlab('Semana')


################################################################################
# Exponential Smoothing
#This method is suitable for data with no clear trend or seasonal pattern
#Our data have a seosonal pattern, but the trend changes direction
#"Forecasts are calculated using weighted averages, where the weights decrease 
#exponentially as observations come from further in the past"
#https://otexts.com/fpp2/ses.html

#Holt-Winters method
#"The Holt-Winters method can be used for daily type of data, where the
#seasonal period is 7"
#https://otexts.com/fpp2/holt-winters.html#example-holt-winters-method-with-daily-data
#https://otexts.com/fpp2/taxonomy.html
holtw_model <- hw(subset(new_deaths_p,end=length(new_deaths)-14),
         damped = TRUE, seasonal="additive", h=21)
autoplot(window(new_deaths_p, start=c(60,1))) + ylab("Número de Óbitos") + xlab('Semana') +
  ggtitle("Previsão de Óbitos por Covid-19") +
  autolayer(holtw_model, series="Método Holt-Winters aditivo", PI=FALSE)+
  guides(colour=guide_legend(title="Previsão"))

################################################################################
# ARIMA

#Unit Root test
#"(...) to determine more objectively whether differencing is required or not."
#"In this test, the null hypothesis is that the data are stationary. So
#small p-values (e.g., less than 0.05) suggest that differencing is required"
#https://otexts.com/fpp2/stationarity.html#unit-root-tests
# kpss.test() also useful
new_deaths_p %>% ur.kpss() %>% summary() # p-value: 3.8086; not stationary.
#aply diff and test again
diff(new_deaths_p) %>% ur.kpss() %>% summary() # p-value: 0.0221; stationary.
#determining whether seasonal differencing is required
nsdiffs(new_deaths_p)# returned 1; so it is required too.
diff(diff(new_deaths_p),7) %>% ur.kpss() %>% summary()
#get diff data
diff_new_deaths_p <- diff(diff(new_deaths_p),7)
autoplot(diff_new_deaths_p) + xlab('Semana')

#Non-Seasonal Arima Model
#R uses maximum likelihood estimation (MLE) to estimate ARIMA models
#https://otexts.com/fpp2/arima-estimation.html
ns_arima_model <- auto.arima(diff_new_deaths_p, seasonal=FALSE, stepwise=FALSE,
                             approximation = FALSE)
ns_arima_model

ns_arima_model %>% forecast(h=7) %>% autoplot(include=100)

#There are a few significant spikes in the ACF, and the model fails the Ljung-Box test. 
#The model can still be used for forecasting, but the prediction intervals may not be
#accurate due to the correlated residuals.
checkresiduals(ns_arima_model) # Q* = 65.155, df = 9, p-value = 1.346e-10 = 0.0000000001346

#The prediction intervals for ARIMA models are based on assumptions that the
#residuals are uncorrelated and normally distributed. If either of these assumptions
#does not hold, then the prediction intervals may be incorrect. For this reason,
#always plot the ACF and histogram of the residuals to check the assumptions before
#producing prediction intervals.
hist(ns_arima_model$residuals)

#Seasonal Arima Model
sea_arima_model <- auto.arima(new_deaths_p, seasonal=TRUE, stepwise=FALSE,
                              approximation = FALSE)
sea_arima_model

sea_arima_model %>% forecast(h=7) %>% autoplot(include=100)

checkresiduals(sea_arima_model)

#ALL autocorrelations values are within the threshold limits, indicating that the
#residuals are behaving like WHITE NOISE
ggAcf(sea_arima_model$residuals)

hist(ns_arima_model$residuals)

# TO DO:
# ARIVA VS ETS https://otexts.com/fpp2/arima-ets.html

################################################################################
# Neural network models
#https://otexts.com/fpp2/nnetar.html

neural_model <- nnetar(new_deaths_p)
neural_model

autoplot(forecast(neural_model,h=7,PI=TRUE), include = 100)

################################################################################
# MODELS ANALYSIS
################################################################################
# Residuals
# 1.The residuals should to be uncorrelated
# 2.The residuals should to have zero mean
# and it is useful (but not necessary):
# 3.The residuals have constant variance.
# 4.The residuals are normally distributed.
#https://otexts.com/fpp2/residuals.html#residuals-1
checkresiduals(meanf_model)
checkresiduals(snaive_model)
checkresiduals(naive_drift_model)

checkresiduals(ets_model)

checkresiduals(holtw_model)

checkresiduals(ns_arima_model)
checkresiduals(sea_arima_model)

# we should use portmanteau test to check if it is really autocorrelated
checkresiduals(neural_model)
Box.test(neural_model$residuals,lag=14,type="Lj")# 
