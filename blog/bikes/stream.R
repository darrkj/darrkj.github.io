options(stringsAsFactors = FALSE)

library(lubridate)

# Load raw data
raw <- read.csv('2014-Q2-Trips-History-Data.csv')

# Turn strings into dates
raw$Start.date <- mdy_hm(raw$Start.date)
raw$End.date <- mdy_hm(raw$End.date)

# Order and get rif of colums that are not needed
raw <- raw[order(raw$Bike., raw$Start.date), c(2, 4, 5, 7, 8)]
raw <- raw[complete.cases(raw), ]

#head(raw)

#yy <- xx[xx$Bike. == 'W21070', c(2, 4, 5, 7)]


#yy <- yy[order(yy$Start.date), ]

#len <- nrow(xx) + 1

# Create two groups to move on each. this is becuase shifts help to find
# the time at station instead of out of station
forward <- raw[-1, ]
backward <- raw[-nrow(raw), ]

names(forward) <- paste('e', names(forward), sep = '')

merge <- cbind(backward, forward)

# Get rid of the edges cases
merge <- merge[merge$Bike. == merge$eBike., ]
merge$Bike. <- NULL
merge$eBike. <- NULL


# Lots of restructuring
sum(merge[, 4] != merge[, 6])


# For the case where the do relocate and the time is unkown
# just use the midpoint.

merge$Start.date <- NULL
merge$Start.terminal <- NULL

merge$eEnd.date <- NULL
merge$eEnd.terminal <- NULL


clean <- merge[merge$End.terminal == merge$eStart.terminal, ]
reloc <- merge[merge$End.terminal != merge$eStart.terminal, ] 


reloc2 <- reloc

reloc$eStart.terminal <- reloc$End.terminal
reloc$eStart.date <- as.POSIXct(mean(c(as.integer(reloc[, 1]), 
                                       as.integer(reloc[, 3]))), 
                                origin = "1970-01-01")


reloc2$End.terminal <- reloc2$eStart.terminal
reloc2$End.date <- as.POSIXct(mean(c(as.integer(reloc2[, 1]), 
                                     as.integer(reloc2[, 3]))), 
                          origin = "1970-01-01")

final <- rbind(clean, reloc, reloc2)

# Cleanup
rm(raw, backward, forward, merge, clean, reloc, reloc2)

final$eStart.terminal <- NULL


head(final)


names(final) <- c('start', 'terminal', 'end')

final <- final[, c(2, 1, 3)]
final <- final[order(final$terminal, final$start), ]

row.names(final) <- NULL


#

final$int <- new_interval(final$start, final$end)

min <- min(c(final$start, final$end))
max <- max(c(final$start, final$end))


t1 <- seq.POSIXt(min, max, "hours")

t2 <- t3 <- t4 <- t1

minute(t1) <- 0
minute(t2) <- 15
minute(t3) <- 30
minute(t4) <- 45

t <- c(t1, t2, t3, t4)
rm(t1, t2, t3, t4, min, max)

t <- sort(t)

#
# Just take a few stations

m <- head(unique(final$terminal))

mm <- final[final$terminal %in% m, ]



stream <- data.frame(time = t, m1 = 0, m2 = 0, m3 = 0,
                     m4 = 0, m5 = 0, tot = 0)


#t[1] %in% mm[mm$terminal == m[1], ]$int


for (i in 1:length(t)) {
  stream[i, ]$m1 <- sum(t[i] %within% mm[mm$terminal == m[1], ]$int)
  stream[i, ]$m2 <- sum(t[i] %within% mm[mm$terminal == m[2], ]$int)
  stream[i, ]$m3 <- sum(t[i] %within% mm[mm$terminal == m[3], ]$int)
  stream[i, ]$m4 <- sum(t[i] %within% mm[mm$terminal == m[4], ]$int)
  stream[i, ]$m5 <- sum(t[i] %within% mm[mm$terminal == m[5], ]$int)
#  stream[i, 7] <- sum(t[i] %within% mm[mm$terminal == m[6], ]$int)
  stream[i, ]$tot <- sum(stream[i, 2:7])
  print(i)
}

stream2 <- stream


stream2 <- stream[stream$tot != 0, ]



#stream2$m1 <- stream2$m1 / stream2$tot
#stream2$m2 <- stream2$m2 / stream2$tot
#stream2$m3 <- stream2$m3 / stream2$tot
#stream2$m4 <- stream2$m4 / stream2$tot
#stream2$m5 <- stream2$m5 / stream2$tot
#stream2$m6 <- stream2$m6 / stream2$tot



#stream2$tot <- NULL


s1 <- data.frame(key = 'AA', value = stream2[, 2], date = stream2[, 1])
s2 <- data.frame(key = 'BB', value = stream2[, 3], date = stream2[, 1])
s3 <- data.frame(key = 'CC', value = stream2[, 4], date = stream2[, 1])
s4 <- data.frame(key = 'DD', value = stream2[, 5], date = stream2[, 1])
s5 <- data.frame(key = 'EE', value = stream2[, 6], date = stream2[, 1])
#s6 <- data.frame(key = 'FF', value = stream2[, 7], date = stream2[, 1])

#

s <- rbind(s1, s2, s3, s4, s5)
rm(s1, s2, s3, s4, s5)

write.csv(s, file = 'data2.csv', row.names = FALSE)

stream3 <- stream2

stream3$time <- as.Date(stream3$time)
stream3[, 2] <- 1/6
stream3[, 3] <- 1/6
stream3[, 4] <- 1/6
stream3[, 5] <- 1/6
stream3[, 6] <- 1/6
stream3[, 7] <- 1/6


stream3 <- unique(stream3)

s1 <- data.frame(key = 'AA', value = stream3[, 2], date = stream3[, 1])
s2 <- data.frame(key = 'BB', value = stream3[, 3], date = stream3[, 1])
s3 <- data.frame(key = 'CC', value = stream3[, 4], date = stream3[, 1])
s4 <- data.frame(key = 'DD', value = stream3[, 5], date = stream3[, 1])
s5 <- data.frame(key = 'EE', value = stream3[, 6], date = stream3[, 1])
s6 <- data.frame(key = 'FF', value = stream3[, 7], date = stream3[, 1])

#

s <- rbind(s1, s2, s3, s4, s5, s6)
rm(s1, s2, s3, s4, s5, s6)

write.csv(transform(s, date = format(date, "%m/%d/%y")), file = 'data2.csv', row.names = FALSE) 

write.csv(s, file = 'data2.csv', row.names = FALSE)


