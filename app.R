library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(shiny)

#install.packages("rgeos")

library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)

library(maps)
library(ggplot2)


options(tigris_use_cache = TRUE)
library(tigris)
library(leaflet)
library(plotly)

library(dplyr)
data = read.csv("covid_19_india.csv")


covid19_india <- read_csv("covid_19_india.csv")
festival_2020 <- read_csv("2020_Festivals.csv")
festival_2021 <- read_csv("2021_Festivals.csv")



festivals_data <- bind_rows(festival_2020, festival_2021) %>% 
  rename(Date = date)

total_sum = covid19_india %>%
  mutate(confirmed_cases_of_that_day = Confirmed - Cured + Deaths)

cases_data <- total_sum %>%
  group_by(Date) %>%
  summarise(Total = sum(confirmed_cases_of_that_day)) %>%
  as.data.frame()


cases_festival <- left_join(x = cases_data, y = festivals_data, by.x = "Date", by.y = "date")   


total_sum = data %>%
  mutate(confirmed_cases_of_that_day = Confirmed - Cured + Deaths)

c_data <- total_sum %>%
  group_by(Date) %>%
  dplyr::summarise(Total = sum(confirmed_cases_of_that_day)) %>%
  as.data.frame()



# In the following line of code I am reading two files
data = read.csv("IndiaWantsOxygen.csv")
countries = read.csv("worldcities.csv")


countries[nrow(countries) + 1, 5] = "England"
countries[nrow(countries) + 1, 5] = "INDIA"
countries[nrow(countries) + 1, 5] = "india"
countries[nrow(countries) + 1, 5] = "USA"
countries[nrow(countries) + 1, 5] = "england"
country_list = unique(countries$country)


data_without_empty_values = filter(data, user_location != '')

data_without_empty_values <- data_without_empty_values %>% separate(user_location, c('City', 'Country'))

data_without_empty_values <- data_without_empty_values %>% 
  mutate(Country = ifelse(match(Country, country_list) > 0, Country, NA))

data_without_empty_values <- data_without_empty_values %>% 
  mutate(Country = ifelse(match(City, countries$city) > 0 & is.na(Country) , countries$country[match(City, countries$city)], Country))

data_without_empty_values <- data_without_empty_values %>% 
  mutate(Country = ifelse(match(City, country_list) > 0 & is.na(Country) , countries$country[match(City, countries$country)], Country))

data_without_na_values <- data_without_empty_values %>%
  select(Country)%>%
  filter(!is.na(Country))%>%
  mutate(Country = ifelse(Country == "india" | Country == "INDIA", "India", ifelse(Country == "United States", "USA", Country)), country_count = 1)


data_without_na_values = data_without_na_values %>%
  mutate(country_count = ifelse(Country == "USA", country_count * 3, ifelse(Country == "United Arab Emirates", country_count * 3,ifelse(Country == "Australia", country_count * 8,ifelse(Country == "New Zealand", country_count * 15, ifelse(Country == "Canada", country_count * 10, ifelse(Country == "Germany", country_count * 63, country_count)))))))

data_without_na_values <- data_without_na_values %>%
  group_by(Country) %>%
  dplyr::summarise(country_count = sum(country_count)) %>%
  as.data.frame()


world_map = map_data("world")
world_map = merge(world_map, data_without_na_values, by.x = "region", by.y = "Country")


ui <- navbarPage("Covid-19 India",
                 
                 tabPanel("India Needs Oxygen", mainPanel(plotOutput("map"), width = 15)),
                 
                 tabPanel("Covid Peak",
                          sidebarLayout(
                            sidebarPanel(
                              dateInput("fdate",  # Input
                                        label="starting date range", # label
                                        value = Sys.Date() - 900, # date value that shows up initially
                                        min = Sys.Date() - 900,  # set the minimin date
                                        max = Sys.Date() + 10, # set the maximum date
                                        format="mm/dd/yy"), # set the format (default is yyyy-mm-dd)
                              
                              dateInput("sdate",  # Input
                                        label="end date range", # label
                                        value = Sys.Date(), # date value that shows up initially
                                        min = Sys.Date() - 900,,  # set the minimin date
                                        max = Sys.Date() + 10, # set the maximum date
                                        format="mm/dd/yy"),
                            ),mainPanel(plotOutput("plot"))), ),
                 
                 
                 tabPanel("festivals", sidebarLayout(
                   sidebarPanel(
                     dateInput("first_date",  # Input
                               label="starting date range", # label
                               value = Sys.Date() - 300, # date value that shows up initially
                               min = Sys.Date() - 900,  # set the minimin date
                               max = Sys.Date() + 10, # set the maximum date
                               format="mm/dd/yy"), # set the format (default is yyyy-mm-dd)
                     
                     dateInput("second_date",  # Input
                               label="end date range", # label
                               value = Sys.Date(), # date value that shows up initially
                               min = Sys.Date() - 900,,  # set the minimin date
                               max = Sys.Date() + 10, # set the maximum date
                               format="mm/dd/yy"),
                   ),mainPanel(plotOutput("festival"), width = 8)) ),
)



server <- function(input, output) {
  
 
  output$plot <- renderPlot({
    
 ggplot(c_data[input$fdate < c_data$Date & input$sdate> c_data$Date, ],
         aes(x=Date, y=Total, group = 1)) +
     geom_line()
  })
  
  output$map <- renderPlot({
    ggplot(data_without_na_values) +
      geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "white", color = "#7f7f7f", size = 0.25
      ) +
      geom_map(map = world_map, aes(map_id = Country, fill = country_count), size = 0.25) +
      scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
      expand_limits(x = world_map$long, y = world_map$lat)
  })
  
  output$festival <- renderPlot({
    
    
    before_sum = sum(cases_festival[cases_festival$Date <= input$first_date  &
                                      cases_festival$Date >= input$first_date - 30,2])
    
     after_sum = sum(cases_festival[cases_festival$Date >= input$second_date  &
                                      cases_festival$Date <= input$second_date + 30,2])

  holiday_before <-  cases_festival %>%
    filter(!is.na(holiday), Date <= input$first_date , Date >= input$first_date - 30) 
   

  holiday_after <-  cases_festival %>%
    filter(!is.na(holiday), Date >= input$second_date , Date <= input$second_date + 30) 
  

  festivals <- bind_rows(holiday_before, holiday_after) %>% head(1)

  Festival <- levels (factor (festivals$holiday))
  
  Impact <- rep(c("before" , "after") , length(Festival))
  Cases <- c(before_sum, after_sum )
  p_data <- data.frame(Festival,Impact,Cases)

  ggplot(p_data, aes(fill=Impact, y=Cases, x=Festival)) +
    geom_bar(position="stack", stat="identity", width = -1) 
    
    
  })
}
                 


shinyApp(ui, server)
