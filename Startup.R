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

#-------------Implementation-----------

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


