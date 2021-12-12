#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)



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

  #Important
  holiday_before <-  cases_festival %>%
    filter(!is.na(holiday), Date <= input$first_date , Date >= input$first_date - 30) 
   # select(holiday)

  holiday_after <-  cases_festival %>%
    filter(!is.na(holiday), Date >= input$second_date , Date <= input$second_date + 30) 
  

  festivals <- bind_rows(holiday_before)

  p_festivals <- levels (factor (festivals$holiday))
  
  p_impact <- rep(c("before" , "after") , length(p_festivals))
  p_value <- c(before_sum, after_sum )
  p_data <- data.frame(p_festivals,p_impact,p_value)

  ggplot(p_data, aes(fill=p_impact, y=p_value, x=p_festivals)) +
    geom_bar(position="stack", stat="identity", width = -1) 
    
    
    
    
    
    

    # specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
    # condition <- rep(c("normal" , "stress" , "nitrogen") , 4)
    # value <- abs(rnorm(12 , 0 , 15))
    # data <- data.frame(specie,condition,value)
    # 
    # # grouped
    # ggplot(data, aes(fill=condition, y=value, x=specie)) +
    #   geom_bar(position="dodge", stat="identity")

    # set.seed(1)
    # 
    # age <- factor(sample(c("Child", "Adult", "Retired"),
    #                      size = 50, replace = TRUE),
    #               levels = c("Child", "Adult", "Retired"))
    # hours <- sample(1:4, size = 50, replace = TRUE)
    # city <- sample(c("A", "B", "C"),
    #                size = 50, replace = TRUE)
    # 
    # df <- data.frame(x = age, y = hours, group = city)
    
  })
}
                 


shinyApp(ui, server)
