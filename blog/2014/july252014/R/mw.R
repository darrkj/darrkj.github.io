
require(XML)
require(plyr)
require(zoo)

#source('R/playbyplay.R')
source('R/game.R')



###############################################################################

season <- seasonify(2013)

# Init list
gameObj <- list()
len <- nrow(season)

for (i in 1:len) {
  print(i / len)
  gameObj[[i]] <- play_by_play(season[i, ])
}

rm(len, i, meta, cleanPBP, play_by_play, pull_day, seasonify, teams)

# Look at all of the scores over a season.
# Do teams have consistent offense and defense, is there scoring clumped 
# and does that matter.

# Build the 2012 season.
# Use the 2013 season to test.

y <- llply(gameObj, gameScoreTS)


x <- lapply(y, function(x) sax(zoo(x$diff, x$t2), 7, 25))
#x <- llply(y, function(x) sax(zoo(x$diff, x$t2), 5, 10))

len <- length(x)
v <- matrix(len * len, len, len)

for (i in 1:(length(x)-1)) {
  for (j in (i+1):length(x)) {
    tmp <- distSAX(x[[i]], x[[j]])
    v[i, j] <- tmp
    v[j, i] <- tmp
  }
  v[i, i] <- 0
  print(i)
}


rm(lk, a, b, i, j, len, xx, tmp)


###############################################################################


# Picka game and find the closest.


plotGame <- function(game) {
  
  idx <- order(v[, game])
  idx <- setdiff(idx, game)
  
  print(season[game, 1:5])
  par(mfrow=c(3,1))
  #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  plot(y[[idx[1]]])
  plot(y[[game]])
  plot(y[[idx[length(idx)]]])
}

###############################################################################
dd <- sort(v)

min(v)

# Which ones have zero distance but are not i == j

minVal <- 9999999999999999
id <- list()

for (i in 1:ncol(v)) {
  tmp <- which(v[-i, i] == min(v[-i, i])) + 1
  val <- v[i, tmp]
  if ( tmp < val) {
    minVal <- val
    id <- c(i, y)
  } else if (tmp == val) {
    minVal <- val
    id <- list(id, c(i, y))
  }
}



##########################




#z <- ldply(y, function(x) sax(zoo(x$diff, x$t2), 5, 10))

#ld <- unlist(lapply(x, length))

#x1 <- zoo(x1$diff, x1$t2)
#x2 <- zoo(x2$diff, x2$t2)

#x1 <- sax(x, 5, 10, plot = TRUE)
#a <- sax(x, 5, 10)

#dist(a, b)
#a
#b




#########

# plot(pp[, 1], pp$diff)
# plot(pp[, 1], pp[, w])
# plot(pp[, 1], pp[, l])
# 
# pp$inf <- season[i, ]$guid
# pp <- pp[, c(1, 4:5)]



#


#x <- ldply(gameObj, function(x) x$meta)
#y <- llply(gameObj, function(x) x$pbp[, c('points', 'score', 'team', 't2')])
# Pull out one game
#x1 <- x[1, ]
#y1 <- y[[1]]

#y1$as <- as.integer(unlist(lapply(strsplit(y1$score, '-'), '[', 1)))
#y1$hs <- as.integer(unlist(lapply(strsplit(y1$score, '-'), '[', 2)))

#if ((tail(y1, 1)$as > tail(y1, 1)$hs)) {
#  y1$diff <- y1$as - y1$hs
#} else {
#  y1$diff <- y1$hs - y1$as
#}

# t1 <- y1[y1$team == x$home[1] & !is.na(y1$points), c('hs', 't2')]
# t2 <- y1[y1$team == x$away[1] & !is.na(y1$points), c('as', 't2')]

#p <- y1[, c('t2', 'diff')]

#p <- p[complete.cases(p), ]

#p <- unique(p)

#p$d <- c(TRUE, p[1:(nrow(p)-1), 2] - p[-1, 2] != 0)
#!duplicated(p[, 2], fromLast = T)

#p <- p[p$d, 1:2]
#library(tseries)
#t <- Sys.time()
#t1$time <- as.POSIXct(t) + t1$t2
#t2$time <- as.POSIXct(t) + t2$t2

#ts1 <- irts(t1$time, t1$hs)
#ts2 <- irts(t2$time, t2$as)

#plot(ts1)
#plot(ts2)
################
