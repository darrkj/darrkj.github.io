xx <- getNodes(graph, 'match n where n.domain = "karate" return n;')

xxID <- sapply(xx, getID)


yy <- getRels(graph, 'match (n)-[r]->(m) where n.domain = "karate" return r;')
y <- lapply(yy, function(x) as.data.frame(t(unlist(x))))
y <- recurBind::recurBind(y)[[1]]

zz <- cypher(graph, "match (n)-[r]->(m) where n.domain = 'karate' return ID(n), ID(m);")



y <- cbind(zz, y)

g <- graph.data.frame(y)
