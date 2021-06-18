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
#it shows a frequent jump on sundays to 2020 and 2021, and on weednesdays to 2021
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
#autocorrelation larger for lags multiples of 7, indicating seasonality
#there is a decrease in the ACF as the lags increase, indicating a trend
#https://otexts.com/fpp2/autocorrelation.html
#https://rpubs.com/hudsonchavs/fac_facp#:~:text=FUN%C3%87%C3%83O%20DE%20AUTOCORRELA%C3%87%C3%83O&text=onde%20Var(r,rt%20%C3%A9%20fracamente%20estacion%C3%A1rio.
ggAcf(new_deaths)

# "Time series that show no autocorrelation are called white noise."
#so it is not an "white noise"

# Plot TS seasonality infos(data, seasonal, trend, remainder)
#https://otexts.com/fpp2/components.html
plot(stl(new_deaths, "periodic"))
