library(igraph)
source('util.R')
rm(html, igraph2neo4j, json)

load('terror.RData')

ID <- function(loc) {
  #'terrorist_organization_profile.asp?id=3544'
  num <- gregexpr('=', loc)[[1]]
  as.integer(substr(loc, num+1, nchar(loc)))
}

# Initialze the network edge list
net <- data.frame(from = NA, fromn = NA, to = NA, ton = NA, type = NA)

# Loop through all o fhte detail info and pull out individual edges
for (i in seq(details)) {
  cur <- details[[i]]
  # Get the ID (4324) for the current org
  cur$ID <- ID(cur$ID)
  
  # Add the ID to the edge list
  if (!is.null(cur$net$ID)) {
    cur$net$from <- cur$ID 
    cur$net$fromn <- cur$name
    cur$net$to <- ID(cur$net$ID)
    
    # Clean up the edge list, add name connection type
    tmp <- strsplit(cur$net$name, ' -- ')
    cur$net$ton <- unlist(lapply(tmp, `[`, 1))
    cur$net$type <- unlist(lapply(tmp, `[`, 2))
    # remove original fields
    cur$net <- cur$net[, 3:7]
    net <- rbind(net, cur$net)
  } 
}
net <- net[-1, ]
rm(cur, i, tmp, ID)




library(RNeo4j)




graph = startGraph("http://localhost:7474/db/data/")
# clear(graph)

nodes <- rbind(data.frame(id = net$from, name = net$fromn),
               data.frame(id = net$to,   name = net$ton))

nodes <- unique(nodes)

nodes$name <- as.character(nodes$name)
nodes$id <- as.character(nodes$id)

neoNodes <- list()

for(i in 1:nrow(nodes)) {
  neoNodes[[i]] = createNode(graph, label = "Terror", name = nodes[i, ]$id, 
                           aka = nodes[i, ]$name) 
}

# Now all nodes are in

# Turn suspected allys into allies
ally <- c("Ally", 'Ally (Suspected)', 'Armed Wing', 'Founding Group', 
          'Founding Group (Suspected)', 'Founding Group and Faction', 
          'General Command (PFLP-GC)', "Political Wing", 'Successor', 
          'Suspected Alias/Ally', "Shared Members", 'Other Affiliation', 
          'Supported Cause', 'Umbrella Organization',
          "Umbrella Organization (Suspected)", "Other Affiliation")


enemy <- c("Competing Faction", "Enemy", "Faction", "Rival", 
           "Rival and Ally", "Splinter Group",
           "Splinter Group (Suspected)") 

net$conn <- ifelse(net$type %in% ally, 'likes', 'hates')


for (i in 1:nrow(net)) {
  from <- which(nodes == as.character(net[i, ]$from))
  to <- which(nodes == as.character(net[i, ]$to))
  if(length(to) == 2) to <- to[2]
  if(length(from) == 2) from <- from[2]
  createRel(neoNodes[[from]], net$conn[i], neoNodes[[to]], how = net$type[i])
  
}



#

q <- 'MATCH (a)-[r:hates]->(b) return a.aka limit 10;'

q <- 'MATCH (a)-->(b)<--(c), (a)-->(c) 
      return a.aka, b.aka, c.aka;'

q <- 'MATCH (a)-->(b)-->(c)-->(d)-->(a), (a)<-->(d)<--(b) 
      return a.aka, b.aka, c.aka, d.aka;'

tri <- cypher(graph, query = q)






q <- 'MATCH (a)-[r:likes]->(b) WHERE r.how = "Ally" 
      return a.aka, b.aka, r.how;'


q <- 'MATCH (a)-[r:likes]->(b) 
      return a.aka, b.aka;'
tri <- cypher(graph, query = q)


d3plot(data.frame(Source = tri[, 1], Target = tri[, 2]), 800, 800)

#









#
