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
  
  
  
  
                    
select(typeCrimePerDay, )
  ?select
  
typeCrimePerDay
summarise





                    
test <- typeCrimePerDay$cleanDescpt
class(test)
test
test <- sapply(test, function(x){
  if(grepl("ASS", x) == 1){
    x = "ASSAULT"
  }
  else{
    x = x
  }
})

class(test)
test[1]
if(grepl("ASS", test[1]) == 1){
  test[1] <- "ASSAULT"
}
test
?vapply
