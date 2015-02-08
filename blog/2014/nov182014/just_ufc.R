

options(stringsAsFactors = FALSE)


library(httr)
library(XML)
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(rvest)
library(igraph)


source('espn_api.R')


if(FALSE) {
  
  lookup <- create_espn_lookup()
  lookup <- lookup[order(lookup$name), ]
  lookup <- lookup[-c(1:3), ]
  
  fighter <- list()
  j <- 1
  for (i in lookup$url[(j+12):nrow(lookup)]) {
    fighter[[i]] <- get_espn_metadata(i)
    print(j)
    j <- j + 1
  }
  save(fighter, file = 'fighter.RData')
}

load('fighter.RData')

############################
rm(create_espn_lookup, espn_history, espn_stats, get_espn_metadata, get)

statURL <- 'http://espn.go.com/mma/fighter/stats/_/id/'


# Create a unique ID
id <- names(fighter)
id <- gsub(statURL, '', id)

id <- sapply(strsplit(id, '/'), `[`, 1)


#################


name <- c()
class <- list()
weight <- list()
height <- list()

for (  i in 1:length(fighter) ) {
  name <- c(name,  fighter[[i]]$name)
  # Class
  tmp <- grep('eight', fighter[[i]]$bio, value = T)
  class[[i]] <- if (length(tmp) == 0) NA else tmp
  # Weight
  tmp <- grep('lbs', fighter[[i]]$bio, value = T)
  weight[[i]] <- if (length(tmp) == 0) {
    NA 
  } else {
    str_extract(tmp, '[0-9]{2,3}\\slbs')
  }
  # Height
  tmp <- grep('\"', fighter[[i]]$bio, value = T)
  height[[i]] <- if (length(tmp) == 0) {
    NA 
  } else {
    str_extract(tmp, "[0-9]'[0-9]{1,2}")
  }
}


class <- unlist(class)
weight <- unlist(weight)
height <- unlist(height)


##########

data <- data.frame(id, name, weight, height, class, country = NA, 
                   birth = NA, stance = NA, nickname = NA, reach = NA, 
                   style = NA)


for ( i in 1:length(fighter) ) {
  tmp <- grep('^Country', fighter[[i]]$meta, value = T)
  data[i, 7] <- if (length(tmp) == 0) NA else tmp
  
  tmp <- grep('Birth', fighter[[i]]$meta, value = T)
  data[i, 8] <- if (length(tmp) == 0) NA else tmp
  
  tmp <- grep('Stance', fighter[[i]]$meta, value = T)
  data[i, 9] <- if (length(tmp) == 0) NA else tmp
  
  tmp <- grep('Nickname', fighter[[i]]$meta, value = T)
  data[i, 10] <- if (length(tmp) == 0) NA else tmp
  
  tmp <- grep('Reach', fighter[[i]]$meta, value = T)
  data[i, 11] <- if (length(tmp) == 0) NA else tmp
  
  tmp <- grep('Style', fighter[[i]]$meta, value = T)
  data[i, 12] <- if (length(tmp) == 0) NA else tmp
}

########



data$country <- gsub('Country', '', data$country)
data$birth <- gsub('Birth Date', '', data$birth)
data$stance <- gsub('Stance', '', data$stance)
data$nickname <- gsub('Nickname', '', data$nickname)
data$reach <- gsub('Reach', '', data$reach)
data$style <- gsub('Style', '', data$style)




rm(class, name, i, tmp, lookup, statURL, height, weight)

######



fg <- list()

for ( i in 1:length(fighter)) {
  fg[[id[i]]] <- cbind(id = id[i], fighter[[i]]$hist)
  print(i)
}

fg <- recurBind(fg)[[1]]

# Just keep the fights from the ufc
fg <- fg[fg$EVENT %in% grep('^UFC[ :]', fg$EVENT, value = T), ]

fg$EVENT <- NULL

names(fg) <- c('id', 'date', 'opp', 'result', 'dec', 'round', 'time')



# Clean up the fight table

fg$date <- mdy(fg$date)
fg <- fg[!is.na(fg$date), ]

# Keep only the win or loss for now
fg <- fg[fg$result %in% c('Win', 'Loss'), ]

#
fg$opp <- tolower(fg$opp)


len <- nchar(fg$opp)

fg$opp <- ifelse(substr(fg$opp, len, len) == ' ', 
                 substr(fg$opp, 1, nchar(fg$opp)-1),  fg$opp)
fg$opp <- gsub('-', ' ', fg$opp)
fg$opp <- gsub('.', '', fg$opp, fixed = T)
fg$opp <- gsub("'", '', fg$opp, fixed = T)
fg$opp <- gsub(",", '', fg$opp, fixed = T)



# Take out tbd
fg <- fg[fg$opp != 'tbd', ]

rm(i, len)

# Pull on the name of the fighter
fights <- merge(fg, data[, c('id', 'name')], by = 'id')


# Make names with winner and loser
fights$winner <- ifelse(fights$result == 'Win', fights$name, fights$opp)
fights$loser <- ifelse(fights$result == 'Loss', fights$name, fights$opp)




fights$opp <- NULL

left <- fights[fights$result == 'Win', c(1, 3:7, 2, 8:9)]
right <- fights[fights$result == 'Loss', c(1, 3:7, 2, 8:9)]

names(left)[1:6] <- paste('wn_', names(left)[1:6], sep = '')
names(right)[1:6] <- paste('ls_', names(right)[1:6], sep = '')

left <- left[order(left$date), ]
right <- right[order(right$date), ]


key <- fights[, c('date', 'winner', 'loser')]
key <- unique(key)

join <- merge(key, left, by = c('date', 'winner', 'loser'))
join <- merge(join, right, by = c('date', 'winner', 'loser'))



join <- unique(join)

join$wn_name <- NULL
join$ls_name <- NULL
join$wn_result <- NULL
join$ls_result <- NULL


join$round <- ifelse(join$wn_round == '-', join$ls_round, join$wn_round)

join$wn_round <- NULL
join$ls_round <- NULL


join$time <- ifelse(join$wn_time == '-', join$ls_time, join$wn_time)

join$wn_time <- NULL
join$ls_time <- NULL


join$dec <- ifelse(join$wn_dec == '-', join$ls_dec, join$wn_dec)

join$wn_dec <- NULL
join$ls_dec <- NULL




join$matchid <- as.character(1:nrow(join) + 100000) 

rm(key, left, right)

# Winner data

pl_lk <- data
pl_lk$name <- NULL

names(pl_lk) <- paste('wn_', names(pl_lk), sep = '')

join2 <- merge(join, pl_lk, by = 'wn_id')

names(pl_lk) <- gsub('wn_', 'ls_', names(pl_lk))

join3 <- merge(join2, pl_lk, by = 'ls_id')


#

fight <- join3

fight$wn_weight <- as.numeric(gsub(' lbs', '', fight$wn_weight))
fight$ls_weight <- as.numeric(gsub(' lbs', '', fight$ls_weight))

height <- function(h) {
  feet <- as.numeric(sapply(strsplit(h, "'"), `[`, 1))
  inch <- as.numeric(sapply(strsplit(h, "'"), `[`, 2))
  feet * 12 + inch
}


fight$wn_height <- height(fight$wn_height)
fight$ls_height <- height(fight$ls_height)

ind <- unlist(gregexpr('(Age:', fight$wn_birth, fixed = T))
fight$wn_birth <- substr(fight$wn_birth, 1, ind-2)

ind <- unlist(gregexpr('(Age:', fight$ls_birth, fixed = T))
fight$ls_birth <- substr(fight$ls_birth, 1, ind-2)


fight$wn_birth <- mdy(fight$wn_birth)
fight$ls_birth <- mdy(fight$ls_birth)

fight$dec <- tolower(fight$dec)
fight$method <- NA

extract <- function(string) {
  ifelse(is.na(fight$method), str_extract(fight$dec, string), fight$method)
}


fight$method <- extract("submission")
fight$method <- extract("tko"       )
fight$method <- extract("decision"  )
fight$method <- extract("ko"      )
fight$method <- extract("dq"        )
fight$method <- extract("draw"       )
fight$method <- extract("sumission"        )
fight$method <- extract("other"        )
fight$method <- extract("no contest"      )
fight$method <- extract("win"   )

fight$dec <- NULL
fight$ls_nickname <- NULL
fight$wn_nickname <- NULL

# Get rid of womens ufc
women <- c("Women's Bantamweight", "Women's Strawweight", 
           "Women's Flyweight",  "Women's Strawweight", "Woment's Strawweight")

fight <- fight[!fight$wn_class %in% women, ]
fight <- fight[!fight$ls_class %in% women, ]


rm(fights, join, join2, join3, pl_lk, fighter, id, ind)
rm(data, fg, women, extract, height)
#

rm(join, join2, join3, pl_lk, data, fights, fighter, i, id, len, fg)

fight$dec <- tolower(fight$dec)
fight$method <- NA



#




g <- graph.data.frame(fight[, c('wn_id', 'ls_id')])
#write.csv(fight, file = 'fight_data.csv', row.names = F)

d <- degree.distribution(g)

#
source_url('https://raw.githubusercontent.com/darrkj/darrkj.github.io/master/rcode/import.R')
import('igraph_2_d3')
import('d3plot')


# How many fights per year
fight$year <- year(fight$date)

plot(table(fight$year))

# How has the network changed
y <- sort(unique(fight$year))

num_fg <- c()
for(i in y) {
  tmp <- fight[fight$year < i, ]
  num_fg <- c(num_fg, length(unique(c(tmp$wn_id, tmp$ls_id))))
}
plot(num_fg)

num_fg <- c()
for(i in y) {
  tmp <- fight[fight$year == i, ]
  num_fg <- c(num_fg, length(unique(c(tmp$wn_id, tmp$ls_id))))
}
plot(num_fg)


f1 <- fight[fight$year <= y[2], ]
g1 <- graph.data.frame(f1[, c('winner', 'loser')])
d3plot(g1)


grTab <- function(gr) {
  data.frame(
    name = vertex.attributes(gr)$name,
    degree = centralization.degree(gr)$res,
    closeness = centralization.closeness(gr)$res,
    betweeness = betweenness(gr),
    eigenvector = centralization.evcent(gr)$vector,
  # alpha = alpha.centrality(gr),
    bonpow = bonpow(gr),
    hub = hub.score(gr)$vector,
    auth = authority.score(gr)$vector,
    page = page.rank(gr)$vector
  )
}




#####################################
time <- sort(unique(fight$date))
acc <- c()

for(i in 1:(length(time) - 1)) {
r <- 5

f1 <- fight[fight$date <= time[r], 1:5]
g1 <- graph.data.frame(f1[, c('ls_id', 'wn_id')])
#g1 <- decompose.graph(g1, max.comps = 1)[[1]]

f2 <- fight[fight$date == time[r + 1], 1:5]


alp <- grTab(g1)[, c('name', 'page')]
id <- f2[, 1] %in% alp$name & f2[, 2] %in% alp$name

f2 <- f2[id, c('ls_id', 'wn_id')]

x1 <- merge(f2, alp, by.x = 'wn_id', by.y = 'name')
names(x1)[3] <- 'wn_alp'

x1 <- merge(x1, alp, by.x = 'ls_id', by.y = 'name')
names(x1)[4] <- 'ls_alp'

acc <- c(acc, sum(x1[, 3] > x1[, 4]) / nrow(x1))
}
#x1

###########################





d3plot(g1, 700, 700)
i <- i + 1
#



f2 <- fight[fight$year > 2012, ]

g2 <- graph.data.frame(f2[, c('winner', 'loser')])
#d3plot(g2, 1700, 1700)






gr2 <- function(tmp) { 
  data.frame(
    Degree_Centralization = centralization.degree(tmp)$centralization, 
    Closeness_Centralization = centralization.closeness(tmp)$centralization,
    Betweenness_Centralization = centralization.betweenness(tmp, directed = FALSE)$centralization, 
    Eigenvector_Centralization = centralization.evcent(tmp, directed = FALSE)$centralization,
    Assortativity_Coefficient = assortativity.degree(tmp), 
    Average_Path_Length = average.path.length(tmp, directed = FALSE, unconnected = TRUE),
    #Clique_Number = clique.number(tmp), 
    Clusters = no.clusters(tmp),
    Max_clus_size = max(clusters(tmp)$csize),
    Diameter = diameter(tmp), 
    Radius = radius(tmp), 
    Girth = girth(tmp)$girth,
    Adhesion = graph.adhesion(tmp), 
    Density = graph.density(tmp), 
    #Chordal = is.chordal(tmp)$chordal, 
    Connected = is.connected(tmp)
  )
}

#save(fight, file = 'temp_ufc.rda')
rm(fg, fights, join, join2, join3, just_ufc)
rm(fighter, pl_lk, tmp)
rm(id, ind, len)
rm(g, g1, g2, i, num_fg)
rm(data, f1, f2, d, y)
rm(extract, height)


#save(fight, file = 'temp_ufc.rda')
load('temp_ufc.rda')


time <- sort(unique(fight$date))
t <- list()
j <- 1
for (i in time) {
  tmp <- fight[fight$date <= time[j], ]
  g2 <- graph.data.frame(tmp[, c('wn_id', 'ls_id')])
  t[[j]] <- gr2(g2)
  print(j)
  j <- j + 1
  rm(g2)
  gc()
}


ufc <- recurBind(t)[[1]]

#


