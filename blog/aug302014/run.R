

source('global.R')

# loc <- read.csv("LOG_2014-04-06_20-39-37K330_3-axis_Accelerometer.log", 
#                 sep = ';', header = F)
# 
# loc <- read.csv("LOG_2014-04-07_08-38-37_DriveToWork.log", 
#                 sep = ';', header = F)




loc <- read.log("LOG_2014-04-07_08-38-37_DriveToWork.log")
loc <- read.log("LOG_2014-04-06_20-39-37_Random.log")
loc <- read.log("LOG_2014-04-07_08-38-37.log")

plotGPS(loc)

lat1 <- loc$lat[1] 
long1 <- loc$long[1]  
lat2 <- loc$lat[614]  
long2 <- loc$long[614] 


a <- loc[22:30, ]
b <- loc[22:30, ]

# add some noise to b
b$lat <- c(seq(-85,85,20), 85)
b$long <- b$long + rnorm(10)/100000
b[10, 2:4] <- c('333', -0.001, 104.0000)
#b$long <- seq(-180, 180, 40)
b$LatLong <- paste(b$lat, b$long, sep = ':')
plotGPS(b)
cc <- b[10, ]
cc <- rbind(cc,cc,cc,cc,cc,cc)

c <- rbind(a, b)
plotGPS(c, 20)

#http://geojson.org/geojson-spec.html

# Google maps 
#Latitude: -85 to +85 (actually -85.05115 for some reason)

#Longitude: -180 to +180


#


point2path <- function(points) {
  lat <- points$lat
  long <- points$long
  
  p <- ''
  for (i in 1:length(lat)) {
    p <- paste(p, '[', long[i], ', ', lat[i], '], ')
  }
  p
}
#[102.0, 0.0], [103.0, 1.0], [104.0, 0.0], [105.0, 1.0]


# The first number goes from zero (very zoomed out) to 19
# very zoomed in.
# The next two are the lat and long centers
# http://geojson.io/#map=6/54.400/93.340

loc <- read.log('LOG_2014-04-13_17-21-03K330_3-axis_Accelerometer.log')

loc$dist <- NA

for( i in 2:nrow(loc)) {
  loc$dist[i] <- haversine(loc$lat[i-1], loc$long[i-1], loc$lat[i], loc$long[i]) 
}












df <- data.frame(Postcode=c("45040", "22202"),
                 Tip=c("k", 
                       "m"))

M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)
