options(stringsAsFactors = FALSE)

library(lubridate)
library(devtools)
library(igraph)
library(XML)
library(plyr)
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/import.R')
import('initDF')
import('recurBind')


# Load raw data
raw <- read.csv('2014-Q2-Trips-History-Data.csv')[, c(2, 4, 5, 7)]

# Turn strings into dates
raw$Start.date <- mdy_hm(raw$Start.date) + hours(5)
raw$End.date <- mdy_hm(raw$End.date) + hours(5)

# Order and get rid of colums that are not needed
raw <- raw[order(raw$Start.date), ]
raw <- raw[complete.cases(raw), ]


names(raw) <- c('sdate', 'sterm', 'edate', 'eterm')



url <- 'http://www.capitalbikeshare.com/data/stations/bikeStations.xml'

xm <- xmlToList(url)
xm <- ldply(xm[-length(xm)], function(x) data.frame(t(x)))
xm <- xm[, c('terminalName', 'lat', 'long')]

xm[, 1] <- unlist(xm[, 1])
xm[, 2] <- as.numeric(unlist(xm[, 2]))
xm[, 3] <- as.numeric(unlist(xm[, 3]))

# Get the midpoint between two points in time.
raw$mid <- raw$sdate + floor((raw$edate-raw$sdate)/2) 

raw$sdate <- NULL
raw$edate <- NULL

raw$sterm <- as.character(raw$sterm)
raw$eterm <- as.character(raw$eterm)

raw <- merge(raw, xm, all.x = T, by.x = 'sterm', by.y = 'terminalName')
names(raw)[4:5] <- c('slat', 'slong')
raw <- merge(raw, xm, all.x = T, by.x = 'eterm', by.y = 'terminalName')
names(raw)[6:7] <- c('elat', 'elong')

raw <- raw[raw$slat < 39.05, ]

gr2 <- function(tmp) { 
  data.frame(Degree = vcount(tmp),
             size = ecount(tmp),
             degree_cent = centralization.degree(tmp)$centralization,
             closeness_cent = centralization.closeness(tmp)$centralization,
             betweenness_cent = centralization.betweenness(tmp, directed = FALSE)$centralization,
             eigenvector_cent = centralization.evcent(tmp, directed = FALSE)$centralization,
             assortativity = assortativity.degree(tmp),
             average_path_length = average.path.length(tmp, directed = FALSE, unconnected = TRUE),
             clique = clique.number(tmp),
             diameter = diameter(tmp),
             radius = radius(tmp),
             girth = girth(tmp)$girth,
             adhesion = graph.adhesion(tmp),
             density = graph.density(tmp),
             reciprocity = reciprocity(tmp),
             transitivity = transitivity(tmp))
}


# All nodes
nodes <- unique(c(raw$sterm, raw$eterm))

nnn <- unique(raw[, c('sterm', 'slong', 'slat')])
nnn <- nnn[complete.cases(nnn), ]
plot(nnn[, 2:3])


# How to cut the dataset to a region of time
t1 <- now()
month(t1) <- 5
day(t1) <- 1
hour(t1) <- 12
minute(t1) <- 0
second(t1) <- 0

diff <- 1

gg <- list()
for (i in 1:1000) {
  print(i)
  t2 <- t1 + hours(2)
  dt <- raw[raw$mid >= t1 & raw$mid < t2, ]
  graw <- graph.data.frame(unique(dt[, 1:2]), vertices = nodes)
  x <- cbind(gr2(graw), date = t1)
  
  gg[[as.character(i)]] <- x
  #Sys.sleep(2)
  t1 <- t1 + hours(diff)
}

gg <- recurBind(gg)[[1]]
#

ggg <- gg
gg <- ggg[1:200, ]

plot(gg$date, gg$size, 'l')
plot(gg$date, gg$degree_cent, 'l')
plot(gg$date, gg$closeness_cent, 'l')
plot(gg$date, gg$betweenness_cent, 'l')
plot(gg$date, gg$eigenvector_cent, 'l')
plot(gg$date, gg$assortativity, 'l')
plot(gg$date, gg$average_path_length, 'l')
plot(gg$date, gg$clique, 'l')
plot(gg$date, gg$diameter, 'l')
plot(gg$date, gg$girth, 'l')
plot(gg$date, gg$density, 'l')
plot(gg$date, gg$reciprocity, 'l')
plot(gg$date, gg$transitivity, 'l')


#

