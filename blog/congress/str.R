library("RNeo4j")
graph = startGraph("http://localhost:7474/db/data/")

rels <- function() {
  query <- 'MATCH (a)-[r]->(b)
            WHERE labels(a) <> [] AND labels(b) <> []
            RETURN DISTINCT head(labels(a)) AS This, 
                  type(r) as To, head(labels(b)) AS That'

  cypher(graph, query)
}



relCount <- function() {
  q <- 'MATCH (n)-[r]->() RETURN type(r), count(*);'
  r <- cypher(graph, q)

  names(r) <- c('To', 'cnt')
  r$cnt <- paste0(r$To, ' (', r$cnt, ') ')
  r
}


nodeCount <- function() {
  rels <- rels()
  types <- unique(c(rels$This, rels$That))

  nodeCounts <- function(type) { 
    cypher(graph, paste0('MATCH (n:', type, ') RETURN count(DISTINCT n);'))
  }

  r <- data.frame(types, cnt = unlist(sapply(types, nodeCounts)), row.names = NULL)
  r$cnt <- paste0(r$types, ' (', r$cnt, ') ')
  r
}



str.graph <- function(graph)
 {a <- rels()
b <- nodeCount()
c <- relCount()


d <- merge(a, b, by.x = 'This', by.y = 'types', all.x = T)
names(d)[4] <- 'this'
d <- merge(d, c, all.x = T)
names(d)[5] <- 'to'
d <- merge(d, b, by.x = 'That', by.y = 'types', all.x = T)
names(d)[6] <- 'that'
d[, 4:6]
}

str.graph <- function(graph) {
  schema <- cypher(graph, 'MATCH (a)-[r]->(b)
  WHERE labels(a) <> [] AND labels(b) <> []
  RETURN DISTINCT head(labels(a)) AS This, type(r) as To, head(labels(b)) AS That')
  
  rels <- cypher(graph, 'MATCH (n)-[r]->() RETURN type(r) as To, count(*) as to;')
  rels$to <- paste0(rels$To, ' (', rels$to, ') ')
  
  types <- unique(c(schema$This, schema$That))
  
  cnt <- unlist(sapply(types, function(type) 
    cypher(graph, paste0('MATCH (n:', type, ') RETURN count(DISTINCT n);'))))
  cnt <- paste0(types, ' (', cnt, ')')
  
  verts <- data.frame(types, cnt, row.names = NULL)
  
  layout <- merge(schema, verts, by.x = 'This', by.y = 'types', all.x = T)
  layout <- merge(layout, rels, all.x = T)
  layout <- merge(layout, verts, by.x = 'That', by.y = 'types', all.x = T)
  names(layout)[4:6] <- c('this', 'to', 'that')
  layout[4:6]
}
