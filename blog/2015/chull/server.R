
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(R6)

Stack <- 
  R6Class("Stack",
          public = list(
            data = NA,
            #
            initialize = function(data, ...) {
              if (!missing(data)) {
                self$data <- as.list(c(data, ...))
              } else {
                self$data <- list()
              }
            },
            #
            size = function() length(self$data),
            #
            push = function(item) self$data[[self$size() + 1]] <- item,
            #
            pop = function() {
              if (self$size() == 0) return(NULL)
              value <- self$data[[self$size()]]
              self$data[[self$size()]] <- NULL
              value
            },
            top = function() self$data[[self$size()]],
            next_top = function() self$data[[self$size() - 1]]
          )
  )


convex_hull <- function(data) {
  # Graham Scan Algorithm
  data <- data[!duplicated(data[, 1:2]), ]
  # Get point with minimum y value.
  miny <- data[data$y == min(data$y), ]
  
  # If more than one point at min y, take point with min x
  if (nrow(miny) > 1) miny <- miny[miny$x == min(miny$x), ]
  
  # Remove min y point from data
  data <- data[data$y != miny$y & data$x != miny$x, ]
  
  delta_y <- (data$y - miny$y)
  delta_x <- (data$x - miny$x) 
  
  data$deg <- asin(delta_y / sqrt(delta_y ^ 2 + delta_x ^ 2))
  
  data$deg <- ifelse(delta_x < 0, (pi/2) - data$deg + (pi/2), data$deg)
  
  # Sort by polor angle
  data <- data[order(data$deg), ]
  
  # Add min y point to head and tail of data.
  new <- plyr::rbind.fill(miny, data, miny)
  new$id <- 1:nrow(new)
  
  # Create stack data structure, and init with 1, 2 and 3.
  S <- Stack$new()
  S$push(1)
  S$push(2)
  S$push(3)
  
  for (i in 4:max(new$id)) {
    cross <- -1
    while (cross < 0) {
      f <- S$next_top(); s <- S$top()
      cross <- (new$x[s] - new$x[f]) * (new$y[i] - new$y[f]) - 
        (new$y[s] - new$y[f]) * (new$x[i] - new$x[f])
      
      if (cross < 0) S$pop()
    }
    S$push(i)
  }
  
  result <- c()
  for(i in 1:S$size()) {
    result <- c(result, new[new$id == S$pop(), ]$id)
  }
  
  new[rev(result[-1]), -which(names(new) %in% c('deg', 'id'))]
}



d <- data.frame(x = c(5, 4), y = c(4, 5), city = c(1, 2))

library(shiny)
library(mclust)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  # Create a spot where we can store additional
  # reactive values for this session
  val <- reactiveValues(x = NULL, y = NULL)    
  

  output$clusterPlot <- renderPlot({
    # Initially will be empty
    if (is.null(input$clusterClick)){
      return()
    }
    
    isolate({
      aa <- input$clusterClick$x
      bb <- input$clusterClick$y
      val$x <- c(val$x, aa)
      val$y <- c(val$y, bb)
      d <<- rbind(d, data.frame(x = aa, y = bb, city = 1))
      d$city <<- 1:nrow(d)

    })

      
    plot(d$x, d$y, ylim = c(0, 10), xlim = c(0, 10))
    nn <- convex_hull(d)
    points(nn$x, nn$y, col = 'red', pch = 19)
    hpts <- c(nn$city, nn$city[1])
    lines(as.matrix(d[hpts, c('x', 'y')]))

  })
})




