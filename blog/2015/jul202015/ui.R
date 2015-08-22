
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Manilla ROute Map"),

  
  inputPanel(
    selectInput("r1", label = "Route 1",
                choices = 1:15, selected = 1),
    
    selectInput("r2", label = "Route 2",
                choices = 1:15, selected = 2)
  ),
  
  mainPanel(
    leafletOutput("myMap")
  )

))
