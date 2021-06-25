library(tidyverse)
library(visdat)
library(naniar)
require(data.table)
home <- getwd()

################################################################################
# PreExploratory Analysis
################################################################################

# Load the main dataset as a dataframe
covid19 <- paste(home, '/data/datasets/owid-covid-data-2021-05-22.csv', sep='')
covid19 <- read.csv(covid19)

# filter by location='Brazil'
covid19_brazil <- covid19[covid19$location == "Brazil", c('location', 'date',
                                                          'total_cases', 'new_cases',
                                                          'total_deaths', 'new_deaths',
                                                          'total_vaccinations',
                                                          'new_vaccinations')]
# Sort values by date
covid19_brazil <- covid19_brazil[order(as.numeric(as.Date(covid19_brazil$date, format = "%Y-%m-%d"))),]

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

# Show dates with NA in 'new_deaths'
covid19_brazil[is.na(covid19_brazil$new_deaths), c('new_deaths', 'date')] # 2020-02-26:2020-03-16
new_deaths_na_dates <- covid19_brazil[is.na(covid19_brazil$new_deaths), 'date']
# Get max date with NA in 'new_deaths'
max_date_na <- last(covid19_brazil[is.na(covid19_brazil$new_deaths), 'date'])
max_date_na
# Check if the max date with NA + 1 day has 'new_deaths' >=1
covid19_brazil[covid19_brazil$date==(as.Date(max_date_na)+1),'new_deaths'] >= 1 # TRUE
# Replace NA with 0's in 'new_deaths'
covid19_brazil$new_deaths <- ifelse(covid19_brazil$date %in% new_deaths_na_dates,
                             0, covid19_brazil$new_deaths)
# Also for 'total_deaths'
covid19_brazil$total_deaths <- ifelse(covid19_brazil$date %in% new_deaths_na_dates,
                               0, covid19_brazil$total_deaths)

# Saving the dataframe as a .csv file
write.table(covid19_brazil, paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep=''),
            sep = ',', row.names = FALSE, quote=FALSE)
