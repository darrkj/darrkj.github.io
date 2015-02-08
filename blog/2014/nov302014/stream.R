options(stringsAsFactors = FALSE)

library(lubridate)

# Load raw data
bike <- read.csv('2014-Q2-Trips-History-Data.csv')



bike %>% 
  select(sdate = Start.date, sterm = Start.terminal, 
         edate = End.date, eterm = End.terminal, Bike.) %>%
  # Change timestamps to local time.
  mutate(sdate = force_tz(mdy_hm(sdate), 'EST'), 
         edate = force_tz(mdy_hm(edate), 'EST'),
         sterm = as.character(sterm), eterm = as.character(eterm)) %>%
  na.omit() %>%
  arrange(Bike., sdate) -> raw
  

# Create two groups to move on each. this is becuase shifts help to find
# the time at station instead of out of station
forward <- raw[-1, ]
backward <- raw[-nrow(raw), ]

names(forward) <- paste('e', names(forward), sep = '')

merge <- cbind(backward, forward)

# Get rid of the edges cases
merge %>%
  filter(Bike. == eBike.) %>%
  select(-Bike., -eBike., -sdate, -sterm, -eedate, -eeterm) -> merge


# For the case where the do relocate and the time is unkown
# just use the midpoint.


clean <- merge[merge$eterm == merge$esterm, ]
reloc2 <- reloc <- merge[merge$eterm != merge$esterm, ] 

reloc$esterm <- reloc$eterm
reloc$esdate <- reloc$edate + floor((reloc$esdate - reloc$edate) / 2)


reloc2$eterm <- reloc2$esterm
reloc2$edate <- reloc2$edate + floor((reloc2$esdate - reloc2$edate) / 2)

final <- rbind(clean, reloc, reloc2)

# Cleanup
rm(raw, backward, forward, merge, clean, reloc, reloc2)

final$esterm <- NULL


head(final)


names(final) <- c('start', 'terminal', 'end')

final <- final[order(final$terminal, final$start), c(2, 1, 3)]

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
  stream[i, ]$tot <- sum(stream[i, 2:7])
  print(i)
}

stream2 <- stream


stream2 <- stream[stream$tot != 0, ]



stations <- unique(final$terminal)

n <- paste('m', 1:length(stations), sep = '')

source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/initDF.R')

stream <- initDF(n, length(t))


for (j in 1:length(stations)) {
  st <- stations[j]
  mm <- final[final$terminal == st, ] 
  for (i in 1:length(t)) {
    stream[i, j] <- sum(t[i] %within% mm$int)
  }
  print(j)
}


library("BreakoutDetection")

res <- breakout(stream[50:300, 11], method='multi', plot=TRUE)
res$plot


#

