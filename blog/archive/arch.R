
# This is useful for importing data.
options(stringsAsFactors = FALSE)

# Create dir for data files.
dir <- "bikeData"
dir.create(dir, showWarnings = FALSE)
temp <- tempfile()

# Location of the files.
url1 <- "http://www.capitalbikeshare.com/assets/files/trip-history-data/2014-2nd-quarter.zip"

# Take the base of the file name at this loaction.
file <- basename(url1)

# Download the file from the internet.
download.file(url1, file)

# Extract zipped contents to directory.
unzip(file, exdir = dir)

# The list of unzipped files.
dataFile <- paste(dir, list.files(dir), sep = '/')

# Read data into R.
#xx <- read.csv()

library(devtools)

install_github("archivist", "pbiecek")

library(archivist)
library(lubridate)
library(dplyr)
library(d3Network)
library(igraph)

source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/import.R')
import('igraph_2_d3')
import('d3plot')




exampleRepoDir <- getwd()
createEmptyRepo( exampleRepoDir )

xx$Start.date <- mdy_hm(xx$Start.date)
xx$End.date <- mdy_hm(xx$End.date)



dataFile %>%
  read.csv %>%
  filter(Bike. == "W21384") %>%
  mutate(total = n()) %>%
  group_by(Start.terminal, End.terminal) %>%
  mutate(route = n() / total) %>%
  select(Start.terminal, End.terminal, route) %>%
  arrange(desc(route)) %>%
  saveToRepo( exampleRepoDir )



#dataFile %>%
  xx %>%
  filter(Bike. == "W20720") %>%
  mutate(total = n()) %>%
  group_by(Start.terminal, End.terminal) %>%
  mutate(route = n() / total) %>%
  select(Start.terminal, End.terminal, route) %>%
  arrange(desc(route)) %>%
  saveToRepo( exampleRepoDir )






showLocalRepo( exampleRepoDir )[, 2]

showLocalRepo( exampleRepoDir )

yy <- graph.data.frame(xx[, c('Start.terminal', 'End.terminal')])


w2 <- xx[xx$Bike. == 'W21384', c('Start.Station', 'End.Station')]
library(plyr)
w3 <- ddply(w2, .(Start.Station, End.Station), summarise, count = nrow)


W2 <- graph.data.frame(W21384[, c('Start.Station', 'End.Station')])
plot(W2)


head(rev(sort(table(xx$Bike.))))







xx %>%
  filter(Bike. == "W21384") %>%
  group_by(Bike., Start.terminal, End.terminal) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) -> rr

w2 <- rr[, c('Start.terminal', 'End.terminal')]

w3 <- graph.data.frame(w2[, c('Start.Station', 'End.Station')])
plot(w3)



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



