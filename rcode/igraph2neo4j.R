igraph2neo4j <- function(igr, label = 'b', type = 'bb') {
  library(d3Network)
  library(igraph)
  if ( is.null(V(igr)$name) ) V(igr)$name <- as.character(V(igr))
  n <- lapply(V(igr)$name, function(x) 
    createNode(graph, label, name = x))
  
  
  x <- apply(get.edges(igr, seq(E(igr))), 1,
             function(x) createRel(n[[e[1]]], type, n[[x[2]]]))
  list(nodes = n, edges = x)
}
