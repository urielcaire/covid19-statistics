library(forecast)
library(ggplot2)
require(data.table)
library(comprehenr)
home <- getwd()

covid19 <- paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# Sort the data by date
covid19 <- covid19[order(as.numeric(as.Date(covid19$date, format = "%Y-%m-%d"))),]


# Getting first date of 'new_deaths' report
day_zero <- first(covid19[covid19$new_deaths>0, 'date'])
# Getting the initial year
start_year <- as.numeric(strftime(as.Date(day_zero), format = '%Y'))
start_year
# Getting initial day position in year range (1:366)
firstDay_ofYear <- as.numeric(strftime(as.Date(day_zero), format = '%j'))
firstDay_ofYear

# Transforms data into daily frequency TimeSeries with a yearly period
new_deaths <- ts(covid19[,c('new_deaths')], start = c(start_year, firstDay_ofYear),
                 frequency = 365)

# Print some infos about new_deaths
summary(new_deaths)
# Plot it
autoplot(new_deaths)
# Plot a histogram
hist(new_deaths)
# Plot a box plot
boxplot(new_deaths)

# Residual analysis
res_model <- auto.arima(new_deaths)
autoplot(res_model$residuals)
hist(res_model$residuals)
# Check residuals: correlated
checkresiduals(res_model)
# Check distribution: it is not a normal distribution
shapiro.test(res_model$residuals)

# Check stationarity: not stationarity, p-value < 2.2e-16.
stat_test <- Box.test(new_deaths, type = 'Ljung-Box')
stat_test
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
# Check residuals: not correlated
checkresiduals(res_model)
# Check distribution: seems to be a normal distribution
shapiro.test(res_model$residuals)
# Check stationarity: stationarity, p-value = 0.04796
stat_test <- Box.test(diff_new_deaths, type = 'Ljung-Box')
stat_test
