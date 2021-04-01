# This script is responsible for merging all raw data
# Here we are filtering the most recent information from each complementary dataset
# (child-mortality, economic-freedom, etc), droping useless columns and, finally,
# merging it with owid-covid-data.
# As a result, this script will generate a new data set called 'merged-data.csv'.
#
require(data.table)
home <- getwd()

# Load the main dataset as a dataframe
covid19 <- paste(home, '/data/datasets/owid-covid-data.csv', sep='')
covid19 <- read.csv(covid19)

# # Load and treat each complementary data set
child_mortality <- paste(home, '/data/datasets/child-mortality-igme.csv', sep='')
child_mortality <- data.table(read.csv(child_mortality))
child_mortality <- as.data.frame(child_mortality[child_mortality[, .I[which.max(Year)],
                                                                 by=Entity]$V1])
colnames(child_mortality)[which(names(child_mortality) == "Entity")] <- "location"
child_mortality <- child_mortality[ , -which(names(child_mortality) %in% c('Code',
                                                                           'Year'))]

economic_freedom <- paste(home, '/data/datasets/economic-freedom-ranking.csv',
                          sep='')
economic_freedom <- data.table(read.csv(economic_freedom))
economic_freedom <- as.data.frame(economic_freedom[economic_freedom[, .I[which.max(Year)],
                                                                    by=Entity]$V1])
colnames(economic_freedom)[which(names(economic_freedom) == "Entity")] <- "location"
economic_freedom <- economic_freedom[ , -which(names(economic_freedom) %in% c('Code',
                                                                              'Year'))]

economic_inequality <- paste(home, '/data/datasets/economic-inequality-gini-index.csv',
                             sep='')
economic_inequality <- data.table(read.csv(economic_inequality))
economic_inequality <- as.data.frame(economic_inequality[economic_inequality[, .I[which.max(Year)],
                                                                             by=Entity]$V1])
colnames(economic_inequality)[which(names(economic_inequality) == "Entity")] <- "location"
economic_inequality <- economic_inequality[ , -which(names(economic_inequality) %in% c('Code',
                                                                                       'Year'))]

population_urban <- paste(home, '/data/datasets/share-of-population-urban.csv', sep='')
population_urban <- data.table(read.csv(population_urban))
population_urban <- as.data.frame(population_urban[population_urban[, .I[which.max(Year)],
                                                                    by=Entity]$V1])
colnames(population_urban)[which(names(population_urban) == "Entity")] <- "location"
population_urban <- population_urban[ , -which(names(population_urban) %in% c('Code',
                                                                              'Year'))]

corruption_perception <- paste(home, '/data/datasets/TI-corruption-perception-index.csv',
                               sep='')
corruption_perception <- data.table(read.csv(corruption_perception))
corruption_perception <- as.data.frame(corruption_perception[corruption_perception[,.I[which.max(Year)],
                                                                                   by=Entity]$V1])
colnames(corruption_perception)[which(names(corruption_perception) == "Entity")] <- "location"
corruption_perception <- corruption_perception[ , -which(names(corruption_perception) %in% c('Code',
                                                                                             'Year'))]

alcohol_consumption <- paste(home, '/data/datasets/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv',
                             sep='')
alcohol_consumption <- data.table(read.csv(alcohol_consumption))
alcohol_consumption <- as.data.frame(alcohol_consumption[alcohol_consumption[, .I[which.max(Year)],
                                                                             by=Entity]$V1])
colnames(alcohol_consumption)[which(names(alcohol_consumption) == "Entity")] <- "location"
alcohol_consumption <- alcohol_consumption[ , -which(names(alcohol_consumption) %in% c('Code',
                                                                                       'Year'))]

# Merging all
covid19 <- merge(covid19, child_mortality, by = "location")
covid19 <- merge(covid19, economic_freedom, by = "location")
covid19 <- merge(covid19, economic_inequality, by = "location")
covid19 <- merge(covid19, population_urban, by = "location")
covid19 <- merge(covid19, corruption_perception, by = "location")
covid19 <- merge(covid19, alcohol_consumption, by = "location")

# Saving the merged dataframe as a .csv file
write.csv(covid19, paste(home, '/data/datasets/merged-data.csv', sep=''),
          row.names = FALSE)

#View(covid19)
