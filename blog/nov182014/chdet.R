
# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(igraph)
library(RNeo4j)


graph = startGraph("http://localhost:7474/db/data/")


q2 <- 'MATCH (a)-[r:fought]->(b)
WHERE r.result = "Win"
RETURN a.id as id, a.name as name, r.date as date, 
b.id as oid, b.name as oname;'


q2 %>% cypher(graph, .) %>% unique() %>% mutate(date = ymd(date)) -> f2 


gr2 <- function(tmp) { 
  data.frame(Degree = length(V(g)),
             Degree_Centralization = centralization.degree(tmp)$centralization,
             Closeness_Centralization = centralization.closeness(tmp)$centralization,
             Betweenness_Centralization = centralization.betweenness(tmp, directed = FALSE)$centralization,
             Eigenvector_Centralization = centralization.evcent(tmp, directed = FALSE)$centralization,
             Assortativity_Coefficient = assortativity.degree(tmp),
             Average_Path_Length = average.path.length(tmp, directed = FALSE, unconnected = TRUE),
             Clique_Number = clique.number(tmp),
             Diameter = diameter(tmp),
             Radius = radius(tmp),
             Girth = girth(tmp)$girth,
             Adhesion = graph.adhesion(tmp),
             Density = graph.density(tmp),
             #Chordal = is.chordal(tmp)$chordal,
             Connected = is.connected(tmp))
}


time <- sort(unique(f2$date))


#i <- 2345
#g <- graph.data.frame(f2[f2$date < time[i], c('oid', 'id')])
#x <- cbind(gr2(g), date = time[i])


###############################

gcdet <- list()
for (i in 1:length(time)) {
  print(i)
  g <- graph.data.frame(f2[f2$date < time[i], c('oid', 'id')])
  x <- cbind(gr2(g), date = time[i])
  
  gcdet[[as.character(i)]] <- x
  #Sys.sleep(2)
  gc()
}

gcdet <- recurBind(gcdet)[[1]]



load('gcdet.RData')
#

page <- pageRank[, c('page', 'opage')]
page <- page[complete.cases(page), ]
sum(page[, 1] < page[, 2])
sum(page[, 1] > page[, 2])

