library(googleVis)
library(lubridate)

read.log <- function(log) {
  firstLine <- readLines(log, 1)
  skip <- if ( firstLine == "--- LOG START  --- ") 1 else 0
  # These are seperated by semicolons and skip one becuase the first line
  # says log output.
  loc <- read.csv(log, sep = ';', header = F, skip = skip, stringsAsFactors = F)
  
  # There are teo semi colons in a row 
  loc$V3 <- NULL
  # The line ends with a semi colon
  loc$V11 <- NULL
  
  names(loc) <- c('inc', 'timestamp', 'lat', 'long', 'alt', 'acc', 'x', 'y', 'z')
  
  loc$date <- as.Date(substr(loc$timestamp, 1, 10))
  loc$time <- ymd_hms(loc$timestamp)
  loc$ind <- seq(nrow(loc))
  
  # These fields should all be numbers.
  for (i in c(3:9)) {
    # Suppress warnings when NA values are introduced.
    suppressWarnings(loc[, i] <- as.numeric(loc[, i]))
  }
  
  loc$LatLong <- paste(loc$lat, loc$long, sep = ':')
  
  loc
}



plotGPS <- function(path, v = 10) {
  if (v == -1) v <- nrow(path)
  if (v > nrow(path)) v <- nrow(path)
  nn <- list(LatLong = path$LatLong[1:v], Tip = path$timestamp[1:v])
  
  m <- gvisMap(nn, 'LatLong' , 'Tip',
               options=list(showTip=TRUE, showLine=FALSE,
                            enableScrollWheel=TRUE,
                            mapType='hybrid', useMapTypeControl=TRUE,
                            width=800,height=400),
               chartid="Run")
  
  plot(m)
}


haversine <- function(lat1, long1, lat2, long2) {
  # Source: http://www.movable-type.co.uk/scripts/latlong.html
  R <- 6371
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (long2 - long1) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  
  a <- sin(dLat / 2) * sin(dLat / 2) +
    sin(dLon / 2) * sin(dLon / 2) * cos(lat1) * cos(lat2); 
  c <- 2 * atan2(sqrt(a), sqrt(1 - a)) 
  R * c * 1000 # In m
}

outliers <- function(path) {
  x <- c()
  for (i in 1:(nrow(path)-1)) {
    x <- c(x, haversine(path$lat[i], path$long[i], path$lat[i+1], path$long[i+1]))
  }
  x
}


#latlong centroid javascript alg
#http://stackoverflow.com/questions/8564615/find-the-centroid-simple-mean-of-a-set-of-latlngs 
latlonCent <- function(data) {
  
  latXTotal <- 0
  latYTotal <- 0
  
  lonDegreesTotal <- 0
  n <- length(data)
  for (i in 1:n) {
    latDegrees <- as.numeric(strsplit(data[i], ":")[[1]][1])
    lonDegrees <- as.numeric(strsplit(data[i], ":")[[1]][2])
    
    latRadians <- pi * latDegrees / 180
    
    latXTotal <- latXTotal + cos(latRadians)
    latYTotal <- latYTotal + sin(latRadians)
    
    lonDegreesTotal <-lonDegreesTotal + lonDegrees;
  }
  
  finalLatRadians <- atan2(latYTotal, latXTotal)
  finalLatDegrees <- finalLatRadians * 180 / pi
  finalLonDegrees <- lonDegreesTotal / n
  
  return(list(lat = finalLatDegrees, 
              long = finalLonDegrees))
}
