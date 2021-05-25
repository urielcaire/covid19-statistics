library(forecast)
library(ggplot2)
require(data.table)
home <- getwd()

# Is this data an example of random walk?
covid19 <- paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# Sort the data by date
covid19 <- covid19[order(as.numeric(as.Date(covid19$date, format = "%Y-%m-%d"))),]
# Getting start and end dates
start_date <- first(covid19$date)
end_date <- last(covid19$date)
# Transforms dates into numeric vectors to input them into ts() function
library(comprehenr)
start_date <- to_vec(for(i in strsplit(start_date,"-")) as.numeric(i))
end_date <- to_vec(for(i in strsplit(end_date,"-")) as.numeric(i))

names(covid19)

# Transforms data into a TimeSeries with weekly frequency
# TO DO:
# - adapt the data to frequency 52
covid19_ts <- ts(covid19[,c('new_deaths', 'new_cases', 'new_vaccinations')], start = start_date, end = end_date,
                 frequency = 52)

autoplot(covid19_ts)

