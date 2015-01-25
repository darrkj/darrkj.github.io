x <- cypher(graph, "match n where n.domain = 'potter' return n limit 25;")
a <- cypher(graph, 'start a = node(162135) return a;')






#potter$n <- gsub('http://localhost:7474/db/data/node/', '', potter$n)

nodes1 = getNodes(graph, "MATCH n where n.domain = 'techTime' RETURN n, n.name;")

nodes2 = getNodes(graph, "MATCH n where n.domain = 'potter' RETURN n")

sapply(nodes2, getID)


potter <- cypher(graph, "match n where n.domain = 'potter' return n, n.name;")

potter$n <- unlist(lapply(potter$n, `[[`, 'labels'))
potter$n <- gsub('http://localhost:7474/db/data/node/', '', potter$n)
potter$n <- gsub('/labels', '', potter$n)


#################################################################


#potter <- cypher(graph, "match n where n.domain = 'potter' return n, n.name as name;")

#potter$n <- unlist(lapply(potter$n, `[[`, 'labels'))
#potter$n <- gsub('http://localhost:7474/db/data/node/', '', potter$n)
#potter$n <- gsub('/labels', '', potter$n)
