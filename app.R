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
                            ),
                            mainPanel(plotOutput("plot"))
                          )
                 ),
                 
                 
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
  
  
}



shinyApp(ui, server)

