
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput('m','Stations', station, multiple = TRUE),
      dateRangeInput("daterange", "Date range:",
                     start = "2014-04-01",
                     end   = "2014-6-30")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$head(
        includeScript("d3.min.js")
      ),
      h2('Inter Station FLow'),
      htmlOutput('networkPlot')
      #plotOutput("distPlot")
    )
  )
))

  

