library(tidyverse)
library(visdat)
library(naniar)
require(data.table)
home <- getwd()

# Load the main dataset as a dataframe
covid19 <- paste(home, '/data/datasets/owid-covid-data-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# filter by location='Brazil'
covid19_brazil <- covid19[covid19$location == "Brazil", c('location', 'date',
                                                          'total_cases', 'new_cases',
                                                          'total_deaths', 'new_deaths',
                                                          'total_vaccinations',
                                                          'new_vaccinations')]
# Apply NA if there are null values
covid19_brazil <- covid19_brazil %>%
                  na_if((""))

# Analyzing full dataset
#check data types
vis_guess(covid19_brazil)
#proportion of missing values
vis_miss(covid19_brazil)
#missing values graph
gg_miss_var(covid19_brazil)
#check NA values intersections
gg_miss_upset(covid19_brazil)
#check correlation bewtween all numeric columns
vis_cor(covid19_brazil[,c('total_cases', 'new_cases', 'total_deaths', 'new_deaths',
                          'total_vaccinations','new_vaccinations')])
#plot correlation between new_cases,new_deaths and new_vaccinations
vis_cor(covid19_brazil[,c('new_cases','new_deaths','new_vaccinations')])

# Saving the dataframe as a .csv file
write.table(covid19_brazil, paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep=''),
            sep = ',', row.names = FALSE, quote=FALSE)
