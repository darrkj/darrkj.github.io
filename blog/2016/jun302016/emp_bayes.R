
###############################################################################
#
# Purpose: This script will gather information on NBA players
# Created By: Kenny Darrell
# Created On: 6-23-2016
#
#
###############################################################################

options(stringsAsFactors = FALSE)

# Load libraries
library(lubridate)
library(dplyr)
library(httr)
library(XML)
library(ggplot2)


# This site is much better for player data

# This site has inforamation on all players. 
base <- 'http://www.basketball-reference.com/players/' 

# It does sort them by letter though, and the first table in the list is 
# garbage header info. The first field in the table is actually a link to 
# more info on that player.


# Initialize a storage container for each page (letter in the alphabet)
players <- list()

# Initialize the storage container for detailed player information.
urlList <- list()

# Loop through each page of players by last name.
for (i in letters) {
  url <- paste(base, i, sep = '')
  # There is only one piece of info here in the second element.
  players[[i]] <- readHTMLTable(url)$players
  
  # This is the percursoer to the link in the HTML
  pre <- paste('href="/players/', i, sep = '')
  
  tmp <- unlist(strsplit(as.character(GET(url)), split = "\ "))
  urlList[[i]] <- grep(pre, tmp, fixed = TRUE, value = TRUE)
}

# Check accuracy with:
sapply(players, nrow)
sapply(urlList, length)

# Data error 1
cbind(urlList[['a']], players[['a']][, 1])
players[['a']] <- players[['a']][-149, ]
# Data error 2
cbind(urlList[['i']], players[['i']][, 1])
players[['i']] <- players[['i']][-6, ]
# Data error 3
cbind(urlList[['h']], players[['h']][, 1])
players[['h']] <- players[['h']][-135, ]
# Data error 4
cbind(urlList[['w']], players[['w']][, 1])
players[['w']] <- players[['w']][-309, ]

# Pull all letters together.

players <- bind_rows(players)

urlList2 <- unlist(urlList)
names(urlList) <- NULL

# Remove the garbage text at the begining.
urlList <- gsub('href=\"/players/', '', urlList)
urlList <- substr(urlList, 1, regexpr('.html', urlList) + 4)
urlList <- paste(base, urlList, sep = '')

rm(pre, i, tmp, base, url)
###############################################################################
#
# Now get the detailed infor from the linkes created.
#
###############################################################################


# Count to display progress suring loop
cnt <- 1

pl_detail <- list()

for (url in setdiff(urlList, names(pl_detail))) {
  pl_detail[[url]] <- readHTMLTable(url)
  
  cnt <- cnt + 1
  print(cnt / length(urlList))
}

rm(cnt, url)

totals <- lapply(pl_detail, `[[`, 'totals')
advanced <- lapply(pl_detail, `[[`, 'advanced')


delta <- setdiff(names(advanced[[1]]), names(totals[[1]]))
delta <- delta[delta != '']
detail <- list()
for (i in 1:nrow(players)) {
  rmv <- grep('Did Not Play', totals[[i]]$Tm)
  tmp <- if(length(rmv) == 0) 1:nrow(totals[[i]]) else -rmv
  
  detail[[i]] <- cbind(
    players[i, ], 
    totals[[i]][tmp, ],     
    advanced[[i]][, delta], 
    urlList[i]
  )
  print(i)
}

rm(i, rmv, tmp, advanced, totals, delta)


details <- bind_rows(detail)

# Better name
names(details)[ncol(details)] <- 'URL'

details$From <- as.numeric(details$From)
details$To <- as.numeric(details$To)

details$Ht <- as.numeric(substr(details$Ht, 1, 1)) * 12 + 
  as.numeric(substr(details$Ht, 3, nchar(details$Ht)))

details$Wt <- as.numeric(details$Wt)


details$BirthDate <- mdy(details$`Birth Date`)

# Add a globally unique identifier
details$guid <- 1:nrow(details)

# Create fields for real years
details$span <- details$To - details$From

# Remove asterisk from name
details$Player <- gsub('*', '', details$Player, fixed = TRUE)


# Clean up
rm(urlList, detail)


info <- 'This data was captured on 6-23-2016 using the script emp_bayes.R and it mostly relies on data from http://www.basketball-reference.com/players.'

save(info, details, players, pl_detail, file = 'pl_emp_bayes.RData')
rm(list = ls())

