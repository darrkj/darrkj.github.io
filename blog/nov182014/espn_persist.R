# If I am going to collect data from many site and do entity resolution
# I need a better way to store the data.

# Try pushing it all into neo4j

# Start with ESPN data.

###############################################################################
#
# Setup and init
#
###############################################################################

# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(httr)
library(XML)
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(rvest)
library(igraph)
library(RNeo4j)

# Load functions that are common.
source('funcs.R')

# This has all of the needed functions for pulling data from ESPN.
source('espn_api.R')

###############################################################################
#
# Start pulling data
#
###############################################################################

# This is commented out so that it soen not run everytime. It can take a while.
# I have never ran it all at once, usually in batches of 1000 URL's. It first 
# all of the URL's via the create espn lookup function. Much work could be done
# to do later parts of the cleaning inline and not collect some of the useless 
# stuff.

if(FALSE) {
  lookup <- create_espn_lookup()
  fighter <- list()
  j <- 1
  for (i in lookup$url) {
    fighter[[i]] <- get_espn_metadata(i)
    print(j)
    j <- j + 1
  }
  rm(i, j, lookup)
  save(fighter, file = 'espn_fighter_110714.RData')
}

###############################################################################
#
# For some reason everything was not collected the first time.
# This is going back and getting those that are missing below.
# They all do appear to be in the lookup. Should run again and 
# see if the problem still exists. Keep running through this until 
# the miss variable is empty,
#
###############################################################################

if (FALSE) {
  miss <- unique(fights[!fights$oppid %in% data$id, ]$oppid)
  miss <- paste0('http://espn.go.com/mma/fighter/stats/_/id/', miss)
  j <- 1
  for (i in miss[j:length(miss)]) {
    fighter[[i]] <- get_espn_metadata(i)
    print(j)
    j <- j + 1
  }
  rm(i, j, miss)
  save(fighter, file = 'espn_fighter_110714_addons.RData')
}

# Done pulling the data from ESPN.
# Cleanup
rm(create_espn_lookup, espn_history, espn_stats, get_espn_metadata,
   anything_to_pull)

###############################################################################
#
# Don't repull, used saved data
#
###############################################################################

load('espn_fighter_110714_addons.RData')

###############################################################################
#
# Create date.frame of fighter attributes
#
###############################################################################

data <- initDF(c('id', 'name', 'weight', 'height', 'class', 'country', 'birth', 
                 'stance', 'nickname', 'reach', 'style'), length(fighter))

###############################################################################
#
# Clean and restructure data
#
###############################################################################

# Create the unique ID that ESPn uses for each figher.
id <- gsub('http://espn.go.com/mma/fighter/stats/_/id/', '', names(fighter))
id <- sapply(strsplit(id, '/'), `[`, 1)

# Add this id to main set of data.
data$id <- id

# Extract a lot of other fields related to a fighter from the large amount of 
# pulled from each fighter's various websites on ESPN, (stats, profile, history)

for ( i in 1:length(fighter) ) {
  bio <- fighter[[i]]$bio
  meta <- fighter[[i]]$meta
  
  data$name[i] <- fighter[[i]]$name
  
  data$class[i] <- maybe(grep('eight', bio, value = T))
  data$weight[i] <- str_extract(maybe(grep('lbs', bio, value = T)), '[0-9]{2,3}\\slbs')
  data$height[i] <- str_extract(maybe(grep('\"', bio, value = T)), "[0-9]'[0-9]{1,2}")
  
   data$country[i] <- clean('Country',    '^Country')
     data$birth[i] <- clean('Birth Date', 'Birth')
    data$stance[i] <- clean('Stance',     'Stance')
  data$nickname[i] <- clean('Nickname',   'Nickname')
     data$reach[i] <- clean('Reach',      'Reach')
     data$style[i] <- clean('Style',      'Style')
}

# Clean data fields
data$weight <- gsub(' lbs', '', data$weight)
data$height <- as.character(height(data$height))
data$birth <- sapply(strsplit(data$birth, ' (', fixed = T), '[', 1)

# Clean up intermediate fields
rm(i, bio, meta, clean, height)

###############################################################################
#
# Now start to put together the actual fights. This will come from the history
# part of the url's.
#
###############################################################################

fights <- list()

for (i in 1:length(fighter)) {
  if(!is.null(fighter[[i]]$hist)) {
    fights[[id[i]]] <- cbind(id = id[i], fighter[[i]]$hist, 
                             exp = nrow(fighter[[i]]$hist):1)
  }
}

fights <- recurBind(fights)[[1]]


###############################################################################
#
# Clean up the fight table
#
###############################################################################

# Give better names to data
names(fights) <- c('id', 'date', 'event', 'opp', 'result', 'dec', 'round', 'time', 
                   'oppid', 'exp')

# Make data a real date, and remove those that are not valid.
fights$date <- mdy(fights$date)
fights <- fights[!is.na(fights$date), ]

# For neo4j, dates dont work.
fights$date <- as.character(fights$date)

# Clean up the text of the opponent
fights$opp <- tolower(fights$opp)
len <- nchar(fights$opp)

fights$opp <- ifelse(substr(fights$opp, len, len) == ' ', 
                 substr(fights$opp, 1, nchar(fights$opp)-1),  fights$opp)

fights$opp <- gsub('-', ' ', fights$opp)
fights$opp <- gsub('.', '', fights$opp, fixed = T)
fights$opp <- gsub("'", '', fights$opp, fixed = T)
fights$opp <- gsub(",", '', fights$opp, fixed = T)

# These names are wrong 
fights[fights$opp == "ben mortimer",       ]$opp <- "ben morr"
fights[fights$opp == "michael mortimer",   ]$opp <- "michael morr"
fights[fights$opp == "kerry lattimer",     ]$opp <- "kerry latr"
fights[fights$opp == "bao she ri gu leng", ]$opp <- "bao ri gu leng"
fights[fights$opp == "bryan to hang lam",  ]$opp <- "bryan hang lam"
fights[fights$opp == "brian lo a njoe",    ]$opp <- "brian lo njoe"


rm(i, len, id)


###############################################################################
#
# This is where you should jump back up and add those not yet found
# to the fighter data list. This is a really ugly way to do this but
# will lead to very complex process.
#
#
           miss <- fights[!fights$oppid %in% data$id, ]$oppid
#
#
###############################################################################


###############################################################################
#
# Add this data to neo4j
#
###############################################################################

graph = startGraph("http://localhost:7474/db/data/")

clear(graph, FALSE)

person <- add_nodes(data, c('fighter', 'espn'), 'id', props = names(data), T)

add_rels(df = fights[-c(40401, 40402), ], from = person, f_name = 'id', 
         to = person, t_name = 'oppid', how = 'fought',
         props = c('date', 'event', 'result', 'dec', 'round', 'time', 'exp'), verbose = T)


save(data, fights, fighter, person, file = 'final_espn.RData')

rm(data, fights, fighter, person)


###############################################################################
#
# Add this data to neo4j
#
###############################################################################

load('final_espn.RData')

q <- 'MATCH (a)-[r:fought]->(b) 
      WHERE r.result = "Win"
      RETURN a.id as wid, a.name as wname, a.weight as ww, a.height as wh, 
             r.dec as dec, r.date as date,
             b.id as lid, b.name as lname, b.weight as lw, b.height as lh
      UNION ALL
      MATCH (a)-[r:fought]->(b) 
      WHERE r.result = "Loss"
      RETURN b.id as wid, b.name as wname, b.weight as ww, b.height as wh,
             r.dec as dec, r.date as date,
             a.id as lid, a.name as lname, a.weight as lw, a.height as lh;'

f <- unique(cypher(graph, q))



#####################

gg <- graph.data.frame(f[1:100, c('id', 'oid')])


d3plot(gg)






# 
# exp <- 'MATCH (a)-[r]->(b)
# WHERE labels(a) <> [] AND labels(b) <> []
# RETURN DISTINCT head(labels(a)) AS This, type(r) as To, head(labels(b)) AS That;'
# 
# e <- cypher(graph, exp)
# 
# 
# p <- shortestPath(person[[100]], "fought", person[[3]], max_depth = 4)
# 
