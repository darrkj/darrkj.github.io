

convex_hull <- function(data) {
  # Graham Scan Algorithm
  # Need stack data structure.
  source('data_struc.R')
  
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



num <- 100
max <- 100
data <- data.frame(city = 1:num, x = rnorm(num, 20, 20), y = runif(num, -40, max))

plot(data$x, data$y, xlim = c(min(data$x)-10, max(data$x)+10), 
     ylim = c(min(data$y)-10, max(data$y)+10))

nn <- convex_hull(data)

points(nn$x, nn$y, col = 'red', pch = 19)
hpts <- c(nn$city, nn$city[1])
lines(as.matrix(data[hpts, c('x', 'y')]))

