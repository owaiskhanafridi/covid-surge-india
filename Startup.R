#---------------Libraries----------------

library(tidyr)
library(dplyr)
library(tidyverse)

#-------------Implementation-----------

india_wants_oxygen <- read_csv("IndiaWantsOxygen.csv")
day_wise  <- read_csv("day_wise.csv")
state_wise_testing_details <- read_csv("StatewiseTestingDetails.csv")

View(day_wise)




