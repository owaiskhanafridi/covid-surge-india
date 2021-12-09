library(shiny)


shinyUI(fluidPage(
  titlePanel(title ="COVID SURGE INDIA"),
  sidebarLayout(
    sidebarPanel((title = "Enter personal Information"),
    ),
    mainPanel(("Personal Information"),
              
              textInput("name", "Enter your name", ""),
              textInput("age", "Enter your age", ""),
              
              textOutput("myname"),
              textOutput("myage")
  )
  )
) 
)


#for checking built-in shiny R examples
runExample()
  
  