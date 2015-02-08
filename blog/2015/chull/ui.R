
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  
  # Create a bootstrap fluid layout
  fluidPage(
    
    # Add a title
    titlePanel("Convex Hull App"),
    
    # Add a row for the main content
    fluidRow(
      
      # Create a space for the plot output
      plotOutput(
        "clusterPlot", "100%", "500px", clickId="clusterClick"
      )
    )
    
    # Create a row for additional information
#     fluidRow(
#       # Take up 2/3 of the width with this element  
#       mainPanel("Points: ", verbatimTextOutput("numPoints")),
#       
#       # And the remaining 1/3 with this one
#       sidebarPanel(actionButton("clear", "Clear Points"))
#     )    
  )
)
