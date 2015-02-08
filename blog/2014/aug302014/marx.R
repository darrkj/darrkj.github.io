getwd()
setwd("/Users/kdarrell/Desktop/interview/Interview_david")
data = read.csv('LOG_2014-04-07_08-38-37.log', sep=";", header=F)
head(data)

data2 =data[-1,-c(3,11)]
colnames(data2) = c(
  "millis",
  "datetime",
  "lat",
  "long",
  "elev",
  "acc_magn",
  "x_acc",
  "y_acc",
  "z_acc"
  )
data2[,1] = as.numeric(as.character(data2[,1]))


head(data2)
tail(data2)
# approx 15min worth of data

summary(data2)
 

head(datetime)

plot(density(data2$elev))
plot(data2)
data2$lat[1] - data2$lat[282]
data2$long[1] - data2$long[282]

nrow(data2)
plot(density(data2$elev))







old <- '/Library/Frameworks/R.framework/Versions/3.0/Resources/library'
ne <- '/Library/Frameworks/R.framework/Versions/3.1/Resources/library'

o <- list.files(old)
n <- list.files(ne)
