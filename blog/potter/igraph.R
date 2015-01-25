library("igraphdata")


data(karate)

igraph_2_neo4j <- function(g, how, domain) {
  
  nodes <- as.data.frame(vertex.attributes(g))
  nodes$domain <- domain
  
  name <- list()
  for (i in 1:nrow(nodes)) {
    name[[i]] <- do_call(createNode, quote(graph), as.list(nodes[i, ]))
  }  
  
  n <- sapply(name, `[[`, 'name')
  
  edges <- as.data.frame(get.edgelist(g))
  edges <- cbind(edges, as.data.frame(edge.attributes(g)), domain = domain)
  
  for (i in 1:nrow(edge)) {
    from <- name[which(edges$V1[i] == n)][[1]]
    to <- name[which(edges$V2[i] == n)][[1]]
    do_call(createRel, quote(from), how, quote(to), as.list(edges[i, -c(1, 2)]))
    if (!is.directed(karate)) {
      do_call(createRel, quote(to), how, quote(from), as.list(edges[i, -c(1, 2)]))
    }
  }
  return(invisible(NULL))
}
#

igraph_2_neo4j(karate, 'knows', 'karate')

igraph_2_neo4j(xx, 'gggg', 'food')

xx <- foodwebs[[1]]

nodes <- as.data.frame(vertex.attributes(xx))
nodes$domain <- 'foodwebs'
name <- list()
for (i in 1:nrow(nodes)) {
  name[[i]] <- do_call(createNode, quote(graph), as.list(nodes[i, ]))
}


n <- sapply(ll, `[[`, 'name')

