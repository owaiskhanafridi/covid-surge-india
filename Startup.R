#---------------Libraries----------------

install.packages("textcat")

library(tidyr)
library(dplyr)
library(tidyverse)
library(textcat)

#-------------Implementation-----------

    india_wants_oxygen <- read_csv("IndiaWantsOxygen.csv")
day_wise  <- read_csv("day_wise.csv")
state_wise_testing_details <- read_csv("StatewiseTestingDetails.csv")
covid_19_clean_complete <- read_csv("covid_19_clean_complete.csv")
covid_19_clean_complete.csvView(day_wise)

#Get the maximum positive cases of each state
state_wise_testing_details %>% group_by(State) %>%  slice(which.max(Positive)) %>% View()



textcat("ஏழைகள் அவெஞ்சர்")


