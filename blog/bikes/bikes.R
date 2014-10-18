options(stringsAsFactors = FALSE)

xx <- read.csv('2014-Q2-Trips-History-Data.csv')


library(lubridate)

xx$Start.date <- mdy_hm(xx$Start.date)
xx$End.date <- mdy_hm(xx$End.date)



library(igraph)


yy <- graph.data.frame(xx[, c('Start.terminal', 'End.terminal')])


w2 <- xx[xx$Bike. == 'W21384', c('Start.Station', 'End.Station')]
library(plyr)
w3 <- ddply(w2, .(Start.Station, End.Station), summarise, count = nrow)


W2 <- graph.data.frame(W21384[, c('Start.Station', 'End.Station')])
plot(W2)


head(rev(sort(table(xx$Bike.))))





library(dplyr)
library(d3Network)

xx %>%
  filter(Bike. == "W21384") %>%
  group_by(Bike., Start.terminal, End.terminal) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) -> rr

w2 <- rr[, c('Start.terminal', 'End.terminal')]

w3 <- graph.data.frame(w2[, c('Start.Station', 'End.Station')])
plot(w3)

devtools::source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/import.R')
import('igraph_2_d3')
import('d3plot')

xx %>%
  filter(Bike. == "W21384") %>%
  mutate(total = n()) %>%
  group_by(Start.terminal, End.terminal) %>%
  mutate(route = n() / total) %>%
  select(Start.terminal, End.terminal, route) %>%
  arrange(desc(route)) %>%
  graph.data.frame %>%
  d3plot


w2 <- rr[, c('Start.terminal', 'End.terminal')]

w3 <- graph.data.frame(w2[, c('Start.Station', 'End.Station')])
plot(w3)







