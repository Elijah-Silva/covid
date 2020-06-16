# Coronavirus Dashboard

The purpose of this application is to share statistics about the coronavirus epidemic using data from the European Centre for Disease Prevention and Control (ECDC).
The user can observe a number of statistics related to the virus, on both a national and global level, including, number of cases, number of deaths, new cases, new deaths, cases per million individuals and deaths per million individuals.

## Deployment

This Shiny application may be accessed [here](https://elisilva.shinyapps.io/covid/)

## Bugs
1. Countries missing data for today's date (in the ECDC dataset) are not included in the dashboard (i.e. Spain)
2. When "World" is choosen as country, no map will be shown (no lat/lng data for "World" in shapefile)

## Authors

Elijah Silva -- [LinkdedIn](https://www.linkedin.com/in/elijahsilva/)

## Source Data

[European CDC (ECDC)](https://github.com/owid/covid-19-data/tree/master/public/data/)
