options(stringsAsFactors = FALSE)

library(lubridate)
library(devtools)
library(dplyr)
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/import.R')
import('initDF')
import('recurBind')


# Load raw data, Turn strings into dates
read.csv('2014-Q2-Trips-History-Data.csv') %>%
  mutate(Start.date = mdy_hm(Start.date), End.date = mdy_hm(End.date)) %>%
  arrange(Bike., Start.date) %>%
  select(Start.date, Start.Station, End.date, End.Station, Bike.) ->
  raw

# Create two groups to move on each. this is becuase shifts help to find
# the time at station instead of out of station
forward <- raw[-1, ]
backward <- raw[-nrow(raw), ]

names(backward) <- paste('e', names(forward), sep = '')

# Get rid of the edges cases
cbind(forward, backward) %>%
  filter(Bike. == eBike.) %>%
  select(-c(eBike., eStart.date, eStart.Station, End.date, End.Station)) ->
  merge

# For the case where the do relocate and the time is unkown
# just use the midpoint.


clean <- merge[merge$eEnd.Station == merge$Start.Station, ]
clean$Start.Station <- clean$eEnd.Station



reloc <- 
  
merge %>% 
  filter(eEnd.Station != Start.Station) %>%
  mutate(e = as.integer(Start.date), s = as.integer(eEnd.date)) %>%
  rowwise() %>%
  mutate(mid = as.POSIXct(mean(e, s), origin = "1970-01-01")) %>%
  select(-c(e, s))->
  reloc
           


reloc2 <- reloc

reloc$Start.Station <- reloc$eEnd.Station
#reloc$Start.date <- as.POSIXct(mean(c(as.integer(reloc$eEnd.date), 
#                                      as.integer(reloc$Start.date))), 
#                                origin = "1970-01-01")
reloc$Start.date <- reloc$mid
reloc$mid <- NULL


#reloc2$End.Station <- reloc2$eStart.Station
#reloc2$End.date <- as.POSIXct(mean(c(as.integer(reloc2$End.date), 
#                                     as.integer(reloc2$eStart.date))), 
#                              origin = "1970-01-01")
reloc2$eEnd.date <- reloc2$mid
reloc2$mid <- NULL


final <- rbind(clean, reloc, reloc2)

# Cleanup
rm(raw, backward, forward, merge, clean, reloc, reloc2)

final$eEnd.Station <- NULL


#head(final)


names(final) <- c('end', 'terminal', 'bike', 'start')

final <- final[, c(2, 3, 4, 1)]
final <- final[order(final$terminal, final$start), ]

final <- final[final$terminal != '', ]

row.names(final) <- NULL


#

final$int <- new_interval(final$start, final$end)

min <- min(c(final$start, final$end))
max <- max(c(final$start, final$end))


t1 <- seq.POSIXt(min, max, "hours")
#t2 <- t1
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

#m <- head(unique(final$terminal))

#mm <- final[final$terminal %in% m, ]



stations <- unique(final$terminal)

n <- paste('m', 1:length(stations), sep = '')

stream <- initDF(n, length(t))


for (j in 1:length(stations)) {
  st <- stations[j]
  mm <- final[final$terminal == st, ] 
  for (i in 1:length(t)) {
    stream[i, j] <- sum(t[i] %within% mm$int)
  }
  print(j)
}


#stat <- sample(stations, 5)
#d <- which(stations %in% stat)

#stream2 <- stream[, d]
#x <- rowSums(stream2)
#stream3 <- stream2[x > 0, ]


l <- list()


j <- 1
for ( i in stations) {
  l[[i]] <- data.frame(key = i, value = stream[, j], date = t)
  j <- j + 1
}



s <- recurBind(l)[[1]]

bike_stream <- s

save(bike_stream, file = 'bike_stream.rda')



# Test


#dd <- s[s$key %in% d, ]

#ddd <- s[s$date < s$date[420], ]

#write.csv(ddd, file = 'data2.csv', row.names = FALSE)





#date <- xx[xx$value == 431, ]$date[1]

