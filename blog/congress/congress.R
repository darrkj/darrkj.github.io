# Always do this
options(stringsAsFactors = FALSE)

# Load libraries needed for pulling data
library(dplyr)
library(rvest)
library(RNeo4j)
library(lubridate)
library(pryr)


if(F) {
  # This is the base URL and the seed.
  base <- 'http://www.senate.gov'
  main <- 'http://www.senate.gov/pagelayout/legislative/a_three_sections_with_teasers/votes.htm'
  
  
  # Pull all of the URLs that point to collections of bill.
  main %>% html() %>% html_nodes('tr td p a') -> x
  
  # Pull out the URL and the name.
  x %>% html_attrs() %>% paste0(base, .) %>% `[`(2:27) -> collection_url
  x %>% html_text()  %>% `[`(2:27) -> collection_name
  rm(x)
  
  # Function to get bill URL given a url
  get_bills <- function(url) {
    url %>% html() %>% html_nodes('tr td a') %>% html_attrs() %>%
      grep('LIS', ., value = T, fixed = T) %>% 
      paste0('http://www.senate.gov', .)
  }
  
  # Actually get all of the bills.
  bills <- unlist(lapply(y, get_bills))
  
  
  
  # Function to get the vote information about given bill.
  get_vote <- function(bill) {
    a <- XML::readHTMLTable(bill)
    a <- a[!sapply(a, is.null)]
    meta <- a[[1]]
    data <- a[[which(sapply(a, function(x) nrow(x) == 49))]]
    data <- rbind(names(data), data)
    names(data) <- c('state', 'a', 'b')
    meta <- data.frame(number = meta[1, 2], date = meta[1, 4], req = meta[2, 2], 
                       outcome = meta[2, 4], nom = meta[3, 2])
    cbind(data, meta)
  }
  
  # Loop to get all of the votes.
  congress <- list()
  j <- 1
  
  for (i in bills) {
    congress[[i]] <- get_vote(i)
    print(j)
    j <- j + 1
  }
  rm(i, j)
  
  congress <- recurBind(congress)[[1]]
  
  
  # Cleanup now that all data is collected
  rm(main, base, get_bills, get_vote)
  save.image("congress2.RData")
}

load("~/Desktop/darrkj.github.io/blog/congress/congress2.RData")
# Need to clean some of the data frame
c1 <- congress[, -2]  
c2 <- congress[, -3]  
names(c1)[2] <- 'name'  
names(c2)[2] <- 'name'  
  
c3 <- rbind(c1, c2)
rm(c1, c2)

# Take colon out of state
c3$state <- gsub(':', '', c3$state)

# Take the part out
party <- strsplit(c3$name, ' (', fixed = T)
name <- sapply(party, `[`, 1)
party <- sapply(party, `[`, 2)
vote <- sapply(strsplit(party, '), ', fixed = T), `[`, 2)
party <- substr(party, 1, 1)

c3$name  <- NULL
c4 <- cbind(c3, name, party , vote)


c4$ts <- lapply(strsplit(c4$date, ',  '), `[`, 1)
c4$ts <- mdy(c4$ts)
c5 <- c4[c4$ts >= ymd("2014-01-01"), ]

c5$year <- sapply(strsplit(c5$date, ', '), `[`, 2)

c5 <- c5[complete.cases(c5), ]
#
# This will be all of the people
sen <- c5[, c('name', 'state', 'party', 'date')]

sen$year <- sapply(strsplit(sen$date, ', '), `[`, 2)
sen$date <- NULL

sen <- unique(sen)

# Give every senator a uniq id
sen2 <- unique(sen[, -4])
sen2 <- sen2[complete.cases(sen2), ]
#
rm(party, vote, c3, c4, congress, bills, collection_name, collection_url, name)

#Sys.getenv
# password = "NDvpiv0IX2Vg5pB321gg"
# url = "http://test.sb02.stations.graphenedb.com:24789/db/data/"
# username = "test"
# 
# # Connect to Graphene DB.
graph = startGraph(url = url,
                    username = username,
                    password = password)

# Create connection to graph database
graph <- startGraph("http://localhost:7474/db/data/")
#clear(graph, F)


c5$nom <- gsub('\n', '', c5$nom, fixed = T)

c5$domain <- 'senate'
  


c5 %>% select(state) %>% distinct %>% mutate(state_id = 1:nrow(.)) %>%
  inner_join(c5) -> c6


c6 %>% select(name, state) %>% distinct %>% mutate(sen_id = 1:nrow(.)) %>%
  inner_join(c6) -> c7


# Add states
c7 %>% select(state, state_id, domain) %>% distinct -> state

states <- add_nodes2(df = state, label = 'state', id = 'state_id', 
                    props = c('state', 'domain'), verbose = TRUE)



c7 %>% select(sen = name, sen_id, domain) %>% distinct -> senator 
                      
senators <- add_nodes2(df = senator, label = 'senator', id = 'sen_id',
                      props = c('sen', 'domain'), verbose = T)

party <- data.frame(p = c('R', 'D', 'I'), domain = 'senate',
                    party = c('Republican', 'Democrate', 'Independent'))

party <- add_nodes2(df = party, label = 'party', id = 'p',
                      props = c('party', 'domain'), verbose = T)



bills <- c5

bills <- add_nodes2(df = bills, label = 'bill', id = 'number',
                   props = c('date', 'req', 'outcome', 'nom', 'domain'), verbose = T)


# Now add relations

c7 %>% select(sen_id, state_id, domain) %>% distinct -> st

add_rels(df = st, from = senators, f_name = 'sen_id', to = states, t_name = 'state_id',
         how = 'is_from', props = 'domain', verbose = T)

c7 %>% select(sen_id, party, domain) %>% distinct -> p

add_rels(df = p, from = senators, f_name = 'sen_id', to = party, t_name = 'party',
         how = 'is_a', props = 'domain', verbose = T)



add_rels(df = c7, from = senators, f_name = 'sen_id', to = bills, 
         t_name = 'number', how = 'voted_on', props = c('vote', 'domain'), verbose = T)
#


http://test.sb02.stations.graphenedb.com:24789/browser/

rstudio::viewer('http://localhost:7474/browser/', height = NULL)



#
