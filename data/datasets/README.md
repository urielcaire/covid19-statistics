# Datasets
All datasets were free downloaded from [OurWorldInData.org](https://ourworldindata.org/) on 2021-03-27. We give our thanks here to the whole team behind this amazing organization.

Below are all the data sets used by this project:

## owid-covid-data.csv
- Link: [owid/covid-19-data](https://github.com/owid/covid-19-data/tree/master/public/data)
- Citation: Max Roser, Hannah Ritchie, Esteban Ortiz-Ospina and Joe Hasell (2020) - "Coronavirus Pandemic (COVID-19)". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/coronavirus' [Online Resource]

## child-mortality-igme.csv
- Link: https://ourworldindata.org/child-mortality

- Citation: Max Roser, Hannah Ritchie and Bernadeta Dadonaite (2013) - "Child and Infant Mortality". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/child-mortality' [Online Resource]

## economic-inequality-gini-index.csv
- Link: https://ourworldindata.org/income-inequality

- Citation: Max Roser and Esteban Ortiz-Ospina (2013) - "Income Inequality". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/income-inequality' [Online Resource]

## share-of-population-urban.csv
- Link: https://ourworldindata.org/urbanization
- Citation: Hannah Ritchie and Max Roser (2018) - "Urbanization". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/urbanization' [Online Resource]

## TI-corruption-perception-index.csv
- Link: https://ourworldindata.org/corruption

- Citation: Esteban Ortiz-Ospina and Max Roser (2016) - "Corruption". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/corruption' [Online Resource]

## total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv
- Link: https://ourworldindata.org/alcohol-consumption

- Citation: Hannah Ritchie and Max Roser (2018) - "Alcohol Consumption". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/alcohol-consumption' [Online Resource]

# Merged Data
In order to simplify our analysis, we merged all this data together into `merged-data.csv`. To do so, we first classified our datasets into two groups:  

- **Main data**: `owid-covid-data.csv`;
- **Complementary data**: `child-mortality-igme.csv`, `economic-inequality-gini-index.csv`, `share-of-population-urban.csv`, `TI-corruption-perception-index.csv` and `total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv`.

## Merge Processes
All **complementary data sets** contain 4 columns (features): `Entity, Code, Year, X`, where `X` is the value related to the objective of the data set (e.g., child mortality rate). Therefore, we applied the same processes on them.  
First we renamed column `Entity` to `location`; then, through `Year`, we selected just the most recent data per location; next, we removed the columns `Code` and `Year`; and finally, we joined all with **Main data** by using column `location`.