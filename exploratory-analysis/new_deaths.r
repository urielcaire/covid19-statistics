library(forecast)
library(ggplot2)
require(data.table)
library(comprehenr)
require(tseries)

home <- getwd()

covid19 <- paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# Sort the data by date
covid19 <- covid19[order(as.numeric(as.Date(covid19$date, format = "%Y-%m-%d"))),]


# Getting first date of reports
day_zero <- first(covid19[, 'date'])
# Getting the initial week
start_week <- as.numeric(strftime(as.Date(day_zero), format = '%U'))
start_week
# Getting initial day position in week range (1:7)
start_day <- as.numeric(strftime(as.Date(day_zero), format = '%w')) + 1
start_day

# Transforms data into daily frequency TimeSeries with a weekly period
new_deaths <- ts(covid19[,c('new_deaths')], start = c(start_week, start_day),
                 frequency = 7)

# Print some infos about new_deaths
summary(new_deaths)
# Plot it
autoplot(new_deaths)
# Plot a histogram
hist(new_deaths)
# Plot a box plot
boxplot(new_deaths)
# Plot season
seasonplot(new_deaths)

# Residual analysis
res_model <- auto.arima(new_deaths)
autoplot(res_model$residuals)
hist(res_model$residuals)
# Check residuals: seems to be not correlated, p-value = 0.007076
# correlation table
checkresiduals(res_model)
# Check distribution: not normal, p-value < 2.2e-16
# https://rpubs.com/paternogbc/46768
shapiro.test(res_model$residuals)
# Check stationarity: not stationarity, p-value = 0.9216
adf_test <- adf.test(new_deaths,alternative = 'stationary')
print(adf_test)

# Check ndiffs
ndiffs(new_deaths, test = 'kpss')

# Apply diffs until find best parameter
alpha <- 0.05
best_p_value <- 2
best_param_diff <- 1
for(i in 1:450){
  diff_new_deaths <- diff(new_deaths, i)
  diff_stat_test <- Box.test(diff_new_deaths, type = 'Ljung-Box')
  # absolute
  if (abs(diff_stat_test$p.value - alpha) < abs(best_p_value - alpha)){
    best_p_value <- diff_stat_test$p.value
    best_param_diff <- i
  }
}
best_p_value
best_param_diff

diff_new_deaths <- diff(new_deaths, best_param_diff)
diff_stat_test <- Box.test(diff_new_deaths, type = 'Ljung-Box')
autoplot(diff_new_deaths)

# Compare original and diff new_death ts
split.screen(figs = c(2,1))
screen(1)
plot(new_deaths, main='new_deaths')
screen(2)
plot(diff_new_deaths, main='diff_new_deaths')

# Residual analysis of diff_new_deaths
res_model <- auto.arima(diff_new_deaths)
autoplot(res_model$residuals)
hist(res_model$residuals)
# Check residuals: has a weak correlation,p-value = 0.3915
checkresiduals(res_model)
# Check distribution: it is not a normal distribution, p-value = 0.02332
shapiro.test(res_model$residuals)
# Check stationarity: seems stationarity, p-value = 0.08105
adf_test <- adf.test(diff_new_deaths,alternative = 'stationary')
print(adf_test)

# Apply stlf decomposition
stlf_result <- stlf(diff_new_deaths, h=7)
autoplot(stlf_result)
seasonplot(diff_new_deaths)

# TODO: reduce outliers with moving average ()
