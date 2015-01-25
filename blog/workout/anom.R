# inluxdb command
# sudo influxdb -config=/usr/local/etc/influxdb.conf


devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)



data(raw_data)
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot




x <- sin(seq(1, 100, .01))[1:5000] + 
  sin(seq(1, 10, .001))[1:5000] + runif(5000)

x[3000:3030] <- runif(31)
res <- AnomalyDetectionVec(x, period=625, plot = TRUE, 
                           #direction='both', 
                           max_anoms=0.02)
res$plot

x <- data.frame(timestamp = now() + 1:5000, count = x)

res = AnomalyDetectionTs(x, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
