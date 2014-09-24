###############################################################################
#
# nba_perf.R
#
# All data comes from 
# http://www.basketballgeek.com/data/
#
# Download the "2008-2009 Regular Season:" zip file.
# Unzip this and move the contents to a folder.
#
# Author - Kenny Darrell
# email - darrell@datamininglab.com
#
# References
# http://r.789695.n4.nabble.com/Fast-dependable-way-to-quot-stack-together-quot-data-frames-from-a-list-td2532293.html
# http://r.789695.n4.nabble.com/Concatenating-data-frame-td799749.html
# 
###############################################################################
# Load packages used.
library(rbenchmark)
library(plyr)

# This is useful for importing data.
options(stringsAsFactors = FALSE)

#Folder where all the data files exist.
dir <- "data/basketballgeek"

# List of files in the R code directory.
fileList <- list.files(dir)

# Full location to the files.
fileList <- paste(dir, fileList, sep ="/")

# Subset for faster running.
files <- fileList[1:100]
###############################################################################
# Method which uses naive appending via rbind.
M1 <- function() {
  # Use rbind to append each file to the first.
  games <- read.csv(files[1], na.strings = "")
  # Drop first that was already loaded.
  file <- files[-1]
  # Loop over each file and append it to growing list.
  for (i in file) {
    tmpGame <- read.csv(i, na.strings = "")
    games <- rbind(games, tmpGame)
  }
  return(games)
}

Rprof("loop_rbind.out")
games1 <- M1()
Rprof(NULL)
summaryRprof('loop_rbind.out')
###############################################################################
# Useful data frame initialization function.
initDF <- function(name, row) {
  # String which start the data frame istantiation.
  init <- "df <- data.frame("
  for (i in name) {
    init <- paste(init, i, " = rep(NA, ", row, "), ", sep = "")
  }
  init <- substr(init, 1, nchar(init)-2)
  init <- paste(init, ")", sep = "")
  eval(parse(text=init))
  return(df)
}
###############################################################################
# Method which uses preallocation.
M2 <- function() {
  # Read first file to get names in the data.
  game <- read.csv(files[1], na.strings = "")
  # The number of rows in the data.
  rows <- nrow(game)
  # Its hard to know this exactly before hand so be conservative with quess.
  estRows <- ceiling(rows * 1.5)
  # Preallocate the data frame.
  games <- initDF(names(game), estRows)
  # Initialize index.
  j <- 1
  # Loop over each file.
  for (i in files) {
    game <- read.csv(i, na.strings = "")
    # How many rows are in this data set.
    len <- nrow(game)
    # Insert these rows into there spots in the preallocated data frame.
    games[j:(j+len-1),] <- game
    # Increment the index
    j <- j + len
  }
  # Remove the exccess from our conservative quess.
  games <- games[1:(j-1),]
  return(games)
}

Rprof("preallocation.out")
games2 <- M2()
Rprof(NULL)
summaryRprof('preallocation.out')
###############################################################################
# Method which uses do.call, rbind all at once.
M3 <- function() {
  # Initialize list that will store each loaded file.
  g <- vector("list", length(files))
  # Initialize the index.
  j <- 1
  # Loop over all of the files.
  for (i in files) {
    g[[j]] <- read.csv(i, na.strings = "")
    j <- j + 1
  }
  games <- do.call('rbind', g)
  return(games)
}

Rprof("native.out")
games3 <- M3()
Rprof(NULL)
summaryRprof('native.out')
###############################################################################
# Recursive row binding, you don't carry huge sets around,
# has divide and conquer on memory usage, large set only appear
# at the top level of the recursion.
recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- rbind(dList[[(i*2)-1]], dList[[i*2]])
    j <- j + 1
  }
  # In case length was odd, just add last set to the end.
  if (floor(len) != len) {
    data[[j]] <- dList[[len*2]]
  }
  # Less data to store on the stack, tail call optimization would be nice here.
  # Try removing this and check out the time diff.
  rm(dList, len, j)
  # Recursive call.
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}
###############################################################################
# Apply the recursive method.
M4 <- function() {
  # Initialize list that will store each loaded file.
  g <- vector("list", length(files))
  # Initialize the index.
  j <- 1
  # Loop over all of the files.
  for (i in files) {
    g[[j]] <- read.csv(i, na.strings = "")
    j <- j + 1
  }
  games <- recurBind(g)[[1]]
  return(games)
}
Rprof("recursive.out")
games4 <- M4()
Rprof(NULL)
summaryRprof('recursive.out')

###############################################################################
# Method which uses plyr, references say it is the fastest approach.
M5 <- function() {
  # Initialize list that will store each loaded file.
  g <- vector("list", length(files))
  # Initialize the index.
  j <- 1
  # Loop over all of the files.
  for (i in files) {
    g[[j]] <- read.csv(i, na.strings = "")
    j <- j + 1
  }
  games <- rbind.fill(g)
  return(games)
}

Rprof("plyr.out")
games5 <- M5()
Rprof(NULL)
summaryRprof('plyr.out')
###############################################################################
# Check that each method works.
all(games1 == games2, na.rm = TRUE)
all(games2 == games3, na.rm = TRUE)
all(games3 == games4, na.rm = TRUE)
all(games4 == games5, na.rm = TRUE)

# Time the first 100
files <- fileList[1:100]
benchmark(replications = rep(2, 1),
          M1(), M2(), M3(), M4(), M5(),
          columns=c('test', 'replications', 'elapsed'))
 
# Time the first 200
files <- fileList[1:200]
benchmark(replications=rep(2, 1),
          M1(), M2(), M3(), M4(), M5(),
          columns=c('test', 'replications', 'elapsed'))

# Time the first 500
files <- fileList[1:500]
benchmark(replications=rep(1, 1),
          M3(), M4(), M5(),
          columns=c('test', 'replications', 'elapsed'))

# Time whole set
files <- fileList
benchmark(replications=rep(1, 1),
          M3(), M4(), M5(),
          columns=c('test', 'replications', 'elapsed'))




