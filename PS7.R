library(ggplot2)
library(dplyr)

#take csv and put it into data frame
crimeStats <- read.csv("C:/Users/aaron/Downloads/March2018.CSV")

class(crimeStats) #crimeStats is just a dataframe. Need to make it a tbl for dplyr

crimeStats <- tbl_df(crimeStats) #makes crimeStats a tbl as well as a dataframe

class(crimeStats) #hooray
