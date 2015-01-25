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
###############################################################################

# This is useful for importing data.
options(stringsAsFactors = FALSE)



#Folder where all the data files exist.
dir <- "data/basketballgeek"

# List of files in the R code directory.
files <- list.files(dir)

# Full location to the files.
files <- paste(dir, files, sep ="/")
files <- files[1:100]

###############################################################################
Rprof("loop_rbind.out")

# Use rbind to append each file to the first.
games1 <- read.csv(files[1], na.strings = "")

# Drop first that was already loaded.
file <- files[-1]

# Loop over each file and append it to growinf list.
for (i in file) {
  tmpGame <- read.csv(i, na.strings = "")
  games1 <- rbind(games1, tmpGame)
}

Rprof(NULL)
summaryRprof('loop_rbind.out')

###############################################################################
# Initialize functions

initDF <- function(name, row) {
  # TODO: document and comment initDF.
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

Rprof("preallocation.out")

# Read first file to get names in the data.
game <- read.csv(files[1], na.strings = "")

# The number of rows in the data.
rows <- nrow(game)

# Its hard to know this exactly before hand so be conservative with quess.
estRows <- ceiling(rows * 1.5)

# Preallocate the data frame.
games2 <- initDF(names(game), estRows)

# Initialize index.
j <- 1
# Loop over each file.
for (i in files) {
  game <- read.csv(i, na.strings = "")
  # How many rows are in this data set.
  len <- nrow(game)
  # Insert these rows into there spots in the preallocated data frame.
  games2[j:(j+len-1),] <- game
  # Increment the index
  j <- j + len
}

# Remove the exccess from our conservative quess.
games2 <- games2[1:(j-1),]

Rprof(NULL)
summaryRprof('preallocation.out')

#rm(game, dir, files, i, j, len, loc, m, n, name)
###############################################################################



recurBind <- function(dList) {
  len <- length(dList) / 2
  data <- list()
  j <- 1
  for (i in seq(len)) {
    data[[j]] <- rbind(dList[[(i*2)-1]], dList[[i*2]])
    j <- j + 1
  }
  if (floor(len) != len) data[[j]] <- dList[[len*2]]
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}

###############################################################################

Rprof("recursive.out")

# Initialize list that will store each loaded file.
g <- list()

# Initialize the index.
j <- 1

# Loop over all of the files.
for (i in files) {
  g[[j]] <- read.csv(i, na.strings = "")
  j <- j + 1
}
games3 <- recurBind(g)[[1]]


Rprof(NULL)
summaryRprof('recursive.out')

###############################################################################
all(games1 == games2, na.rm = TRUE)
all(games2 == games3, na.rm = TRUE)


summaryRprof('loop_rbind.out')[4]
summaryRprof('preallocation.out')[4]
summaryRprof('recursive.out')[4]
  