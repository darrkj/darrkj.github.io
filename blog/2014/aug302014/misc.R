loc <- read.log('LOG_2014-04-13_17-21-03K330_3-axis_Accelerometer.log')

loc$dist <- NA

for( i in 2:nrow(loc)) {
  loc$dist[i] <- haversine(loc$lat[i-1], loc$long[i-1], loc$lat[i], loc$long[i]) 
}

# I was at home this whole period of time.
loc <- loc[1:3050, ]


library(manipulate)

manipulate(plot(loc$lat[a:b], loc$long[a:b], 
                xlim = c(38.825, 38.838), ylim = c(-77.135, -77.115)), 
           a = slider(0, 3050, initial = 25),
           b = slider(10, 3050, initial = 25))
