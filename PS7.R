library(ggplot2)
library(dplyr)

#take csv and put it into data frame
crimeStats <- read.csv("C:/Users/aaron/Downloads/March2018.CSV")

class(crimeStats) #crimeStats is just a dataframe. Need to make it a tbl for dplyr

crimeStats <- tbl_df(crimeStats) #makes crimeStats a tbl as well as a dataframe

class(crimeStats) #hooray

#Cleans the crime stats with better descriptions and dates for future grouping
cleanCrimeStats <- crimeStats %>% 
                  mutate(Description, cleanDescpt = sapply(Description, function(x){ #cleans the description
                  strsplit(as.character(x),"[-/]")[[1]][1]
                  })) %>% 
                  mutate(cleanDescpt, cleanDescpt = sapply(cleanDescpt, function(x){ #Simplifies all the different types of assaults to just "ASSAULT". I know the grepl pattern is funny
                    if(grepl("ASS",x) == 1){
                      x = "ASSAULT"
                    }else{
                      x = x
                    }
                  }))%>%
                  mutate(cleanDescpt, cleanDescpt = sapply(cleanDescpt, function(x){ #Simplifies all the different types of assaults to just "ASSAULT". I know the grepl pattern is funny
                  if(grepl("LARC",x) == 1){
                    x = "LARCENY"
                  }else{
                    x = x
                  }
                  }))%>%
                  mutate(cleanDescpt, cleanDescpt = sapply(cleanDescpt, function(x){ #Simplifies all the different types of assaults to just "ASSAULT". I know the grepl pattern is funny
                  if(grepl("DESTRUC PROPERTY",x) == 1){
                    x = "DESTRUCTION OF PROPERTY"
                  }else{
                    x = x
                  }
                  }))%>%
                  filter(substr(as.character(DateOccur), start = 1, stop = 2) == "03") %>% #limits month to march. There are some odd januaries and februaries in there
                  mutate(DateOccur, cleanDay = sapply(DateOccur, function(x){
                  strsplit(as.character(x), "[//]")[[1]][2]
                  })) 

#groups the cleaned stats to figure out the number of crimes of each type that occured on each day
typeCrimePerDay <- cleanCrimeStats %>%
                   group_by(cleanDay, cleanDescpt) %>%
                   summarise(count = n())

#groups the cleaned stats to determine the total number of each crime in the month of march        
mostCommonCrime <- cleanCrimeStats %>%
                   group_by(cleanDescpt) %>%
                  summarise(count = n())
  
#The most common crime in March was Larceny with 894 counts 

#groups the cleaned stats to figure out the number of crimes per day in each neighborhood
crimePerDayByNeighborhood <- cleanCrimeStats %>%
                            group_by(Neighborhood, cleanDay) %>%
                            summarise(count = n())

#groups the cleaned stats to determine the sum of crime in each neighborhood  
mostCrimeByNeighborhood <- cleanCrimeStats %>%
                        group_by(Neighborhood) %>%
                        summarise(count = n())
#Neighborhood 35 had the largest amount of crime in March with 294 incidents
  
#All Robbery related crimes have a Crime ID starting with 3

crimeByDistrict <- cleanCrimeStats %>% 
                    group_by(District) %>%
                    summarise(count = n())
#proportion of crime related to rovery by district. District 5 have the largest proportion of crime related to robbery. 
robberyCrimeByDistrict <- cleanCrimeStats %>%
                          filter(Crime > 29999 & Crime < 40000) %>%
                          group_by(District) %>%
                          summarise(count = n()) %>%
                          mutate(count, propotionOfTotalCrime = count/crimeByDistrict$count[2:7])

#for question three I am going to divide crime by its crime code. For example a crime code in the 250,000 means loitering

class(cleanCrimeStats$Crime[1]) #Crime column is class integer. Can use logicals to seperate out

#reclassifies rows for ggplot
cleanCrimeStatsGGPLOT <- cleanCrimeStats %>%
                    mutate(Crime, generalCrime = sapply(Crime, function(x){ #Simplifies all the different types of assaults to just "ASSAULT". I know the grepl pattern is funny
                      if(x == 10000){
                        x = "HOMICIDE"
                      }
                      else if (x >= 20000 & x < 30000){
                        x = "RAPE"
                      }
                      else if (x >= 30000 & x < 40000){
                        x = "ROBBERY"
                      }
                      else if (x >= 40000 & x < 50000){
                        x = "ASSAULT"
                      }
                      else if (x >= 50000 & x < 60000){
                        x = "BURGLARY"
                      }
                      else if (x >= 60000 & x < 70000){
                        x = "LARCENY"
                      }
                      else if (x >= 70000 & x < 80000){
                        x = "AUTO THEFT"
                      }
                      else if (x >= 80000 & x < 90000){
                        x = "ARSON"
                      }
                      else if (x >= 90000 & x < 100000){
                        x = "ASSAULT"
                      }
                      else if (x >= 100000 & x < 110000){
                        x = "FORGERY"
                      }
                      else if (x >= 110000 & x < 120000){
                        x = "FRAUD/IDENTITY THEFT"
                      }
                      else if (x >= 120000 & x < 130000){
                        x = "EMBEZZLEMENT"
                      }
                      else if (x >= 130000 & x < 140000){
                        x = "STOLEN PROPERTY"
                      }
                      else if (x >= 140000 & x < 150000){
                        x = "DESTRUCTION OF PROPERTY"
                      }
                      else if (x >= 150000 & x < 160000){
                        x = "WEAPONS CHARGE"
                      }
                      else if (x >= 170000 & x < 180000){
                        x = "SEX OFFENSE/ABUSE"
                      }
                      else if (x >= 180000 & x < 190000){
                        x = "DRUG POSSESSION "
                      }
                      else if (x >= 200000 & x < 210000){
                        x = "FAMILY/CHILD ENDANGERMENT"
                      }
                      else if (x >= 210000 & x < 220000){
                        x = "DUI"
                      }
                      else if (x >= 220000 & x < 230000){
                        x = "LIQOUR LAWS"
                      }
                      else if (x >= 240000 & x < 250000){
                        x = "DISORDERLY CONDUCT"
                      }
                      else if (x >= 250000 & x < 260000){
                        x = "LOITERING"
                      }
                      else if (x >= 260000 & x < 270000){
                        x = "MISC VIOLATIONS"
                      }
                    }))

cleanCrimeStatsGGPLOTDay <- cleanCrimeStatsGGPLOT%>%
                            group_by(cleanDay, generalCrime) %>%
                            summarise(count = n()) %>%
                            arrange(generalCrime) 

#temp
#ggplot(cleanCrimeStatsGGPLOTDay, aes(x = cleanCrimeStatsGGPLOTDay$cleanDay, y = cleanCrimeStatsGGPLOTDay$count, colour = cleanCrimeStatsGGPLOTDay$generalCrime)) + geom_point() + xlab("Day") + ylab("Number of incidents") + ggtitle("St. Louis March Crime") + labs(color = "Type of Crime")

ggplot(cleanCrimeStatsGGPLOTDay, aes(x = cleanDay, y = count, colour = generalCrime)) + geom_point() + xlab("Day") + ylab("Number of incidents") + ggtitle("St. Louis March Crime") + labs(color = "Type of Crime")

cleanCrimeStatsGGPLOTDistrict <- cleanCrimeStatsGGPLOT %>% 
                                  group_by(District, cleanDay) %>%
                                  summarise(count = n())

ggplot(cleanCrimeStatsGGPLOTDistrict, aes(x = cleanCrimeStatsGGPLOTDistrict$cleanDay, y = cleanCrimeStatsGGPLOTDistrict$count, colour = cleanCrimeStatsGGPLOTDistrict$District)) + geom_line() + xlab("Day") + ylab("Number of incidents") + ggtitle("St. Louis March Crime") + labs(color = "Type of Crime")

