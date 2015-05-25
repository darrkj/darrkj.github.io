# msm (Jackson 2011) handles Multi-State Models for panel data;
install.packages('msm')
library(msm)
library(dplyr)
library(lubridate)
library(markovchain)

bike <- read.csv('2014-Q2-Trips-History-Data.csv')


bike %>% select(sdate = Start.date, edate = End.date,
                sterm = Start.terminal, eterm = End.terminal, 
                bike = Bike., type = Subscriber.Type) -> bik

bik$sdate <- mdy_hm(bik$sdate)
bik$edate <- mdy_hm(bik$edate)

clean <- function(id) {
  x <- bik[bik$bike == id, ]
  x <- x[order(x$sdate), ]
  
  t <- x[1, ]
  for (i in 1:(nrow(x) - 1)) {
    if (x$eterm[i] != x$sterm[i+1]) {
      dt <- x$edate[i] + difftime(x$sdate[i+1], x$edate[i]) / 2
      t <- rbind(t, data.frame(sdate = dt, sterm = x$eterm[i], 
                               edate = dt, eterm = x$sterm[i+1], 
                               bike = x$bike[1], type = 'align'))
    }
  }
  t <- t[-1, ]
  xx <- rbind(x, t)
  xx <- xx[order(xx$sdate), ]
}


xx <- clean(bik$bike[1])
y <- xx[, c('sterm', 'type')]

sequenceMatr<-createSequenceMatrix(x$sterm,sanitize=FALSE)

ind <- c(1, which(y$type == 'align'))
z <- list()
for (i in 1:(length(ind) - 1)) {
  z[[i]] <- y$sterm[ind[i]:ind[i+1]]
}


#cbind(x$sterm[-1], x$eterm[-nrow(x)])
#x$sterm[-1] == x$eterm[-nrow(x)]

#

