

# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(igraph)
library(RNeo4j)
library(magrittr)
library(rlist)
library(pryr)

# Create connection to graph database
graph <- startGraph("http://localhost:7474/db/data/")


clearDom <- function(dom) {
  cypher(graph, paste0("match (a)-[r]->(b) where r.domain = '", dom, "' delete r;"))
  cypher(graph, paste0("match a where a.domain = '", dom, "' delete a;"))
}



clearDom('potter')
