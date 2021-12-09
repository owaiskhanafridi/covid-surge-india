#---------------Libraries----------------

install.packages("textcat")
install.packages("tictoc")

install.packages("Shiny")

library(tidyr)
library(dplyr)
library(tidyverse)
library(textcat)
library(data.table)
library(shiny)

install.packages("rgeos")

library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)

library(maps)
library(ggplot2)



ind <- map(database = "world", regions = "india", exact = FALSE, boundary = T)
india <- map_data(ind , region = "india", exact = F)

(ggplot(aes(x=x, y=y, fill=z), data=state_wise_testing_details) + geom_tile()) + geom_polygon(data=india, aes(x=long, y=lat, group=group), colour="black", fill="red", alpha=0)


states_map <- map_data("state", country="india")
fee_map <- merge(states_map, spendstate, by.x = "region", by.y = "state")


?map_data

states_map %>% View()

IND<-getCountries("IND",level=2)

df1 = gpd.read_file('India_Districts/c44c9b96-f570-4ee3-97f1-ebad64efa4c2202044-1-1rb4x6s.8xx6.shp')

#-------------Reading Data-----------

india_wants_oxygen <- read_csv("IndiaWantsOxygen.csv")
day_wise  <- read_csv("day_wise.csv")
state_wise_testing_details <- read_csv("StatewiseTestingDetails.csv")
covid_19_clean_complete <- read_csv("covid_19_clean_complete.csv")
covid19_india <- read_csv("covid_19_india.csv")

#Get the maximum positive cases of each state
state_wise_testing_details %>% group_by(State) %>%  slice(which.max(Positive)) %>% View()

state_wise_testing_details %>% View()

jan1st  <- 

textcat("ஏழைகள் அவெஞ்சர்")



#-------------State Wise Positivity-----------

#Data Reading
state_wise_testing_details <- read_csv("StatewiseTestingDetails.csv")

#Data Cleaning

#Considering rows which have atleast one column (Positive, Negative) value available.
#Evaluating missing value from the available value.
#Calculating the covid positivity rate. 

cleaned_data <- state_wise_testing_details %>%  
  filter(!is.na(Positive) | !is.na(Negative)) %>% 
  mutate(
    Negative = ifelse(is.na(Negative), TotalSamples - Positive, Negative), 
    Positive = ifelse(is.na(Positive), TotalSamples - Negative, Positive),
    Positive_Rate = round((Positive/TotalSamples) * 100, 2)
    )


shp <- rgdal::readOGR('~/STA 518/STA518_Project/covid-surge-india/Admin2.shx')

shp %>% View()
