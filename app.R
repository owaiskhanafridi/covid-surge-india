#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Covid Surge India"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("correlation_plot"), width = 8),
    box(
      selectInput("features", "Features:",
                  c("Sepal.Width", "Petal.Length", "Petal.Width")
                  ), width = 4
    ),
    dateInput("fdate",  # Input
              label="starting date range", # label
              # use below to show the calendar icon inline
              # label = HTML("<i class='glyphicon glyphicon-calendar'></i> Date Input"),
              value = Sys.Date() - 900, # date value that shows up initially
              min = Sys.Date() - 900,  # set the minimin date
              max = Sys.Date() + 10, # set the maximum date
              format="mm/dd/yy"), # set the format (default is yyyy-mm-dd)
    dateInput("sdate",  # Input
              label="end date range", # label
              # use below to show the calendar icon inline
              # label = HTML("<i class='glyphicon glyphicon-calendar'></i> Date Input"),
              value = Sys.Date() - 300, # date value that shows up initially
              min = Sys.Date() - 900,,  # set the minimin date
              max = Sys.Date() + 10, # set the maximum date
              format="mm/dd/yy"),
  )
  
)



server <- function(input, output) {
output$correlation_plot <- renderPlot({
  plot(iris$Sepal.Length, iris[[input$features]], 
       xlab = "Sepal Length", ylab = "Feature"
       )
  ggplot(c_data[input$fdate < c_data$Date & input$sdate > c_data$Date, ], 
         aes(x=Date, y=Total, group = 1)) +
    geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=3)
})   
}

shinyApp(ui, server)