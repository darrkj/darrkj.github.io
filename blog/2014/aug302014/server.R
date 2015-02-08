
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
    
  output$view <- renderGvis({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    # read.csv(inFile)
    loc <- read.log(inFile$datapath)
    xx <- input$bins*4
    nn <- list(LatLong = loc$LatLong[1:xx], Tip = loc$timestamp[1:xx])
    
    gvisMap(nn, 'LatLong' , 'Tip',
                 options=list(showTip=TRUE, showLine=FALSE,
                              enableScrollWheel=TRUE,
                              mapType='hybrid', useMapTypeControl=TRUE,
                              width=800,height=400),
                 chartid="Run")
  })
  output$tabl <- renderTable({
    inFile <- input$file1
    head(read.log(inFile$datapath))
  })


})
