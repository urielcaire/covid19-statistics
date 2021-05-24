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
#View(covid19_brazil)
# Saving the dataframe as a .csv file
write.table(covid19_brazil, paste(home, '/data/datasets/covid-brazil-2021-05-22.csv', sep=''),
            sep = ',', row.names = FALSE, quote=FALSE)
