library(arules)

# http://ilpubs.stanford.edu:8090/575/1/2003-10.pdf
sen <- cypher(graph)




# Use sankey from d3network


fight <- cypher(graph, 'match (n)-[r:`fought`]->(m) return ID(n), n.name, n.class, n.county, n.style, 
                ID(m), m.name, m.class, m.county, m.style, r.date, r.event, r.result, r.dec, r.round, r.exp')


library("d3Network")




nod <- data.frame(name = paste0(letters[1:10], 1:10))

s1 <- sample(0:3, 25, T)
s2 <- sample(4:6, 25, T)
s3 <- sample(7:9, 25, T)

xx <- as.data.frame(rbind(cbind(s1, s2), cbind(s2, s3)))
names(xx) <- c('source', 'target')

xx$value <- rnorm(nrow(xx)) + 10





library(RCurl)
URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/sankey/JSONdata/energy.json"
Energy <- getURL(URL, ssl.verifypeer = FALSE)


EngLinks <- JSONtoDF(jsonStr = Energy, array = "links")
EngNodes <- JSONtoDF(jsonStr = Energy, array = "nodes")



d3Sankey(Links = EngLinks, Nodes = EngNodes, Source = "source",
         Target = "target", Value = "value", NodeID = "name",
         fontsize = 12, nodeWidth = 30, file = "~/Desktop/TestSankey.html")






##############


# First get all paths in the database

whatAndHow <- "MATCH (a)-[r]->(b)
WHERE labels(a) <> [] AND labels(b) <> []
RETURN DISTINCT head(labels(a)) AS This, type(r) as To, head(labels(b)) AS That"

w <- cypher(graph, whatAndHow)


fight <- cypher(graph, 'match (n)-[r:`fought`]->(m) return ID(n), n.name, n.class, n.country, n.style, 
                ID(m), m.name, m.class, m.country, m.style, r.date, r.event, r.result, r.dec, r.round, r.exp')

################################################

#fight <- fight[duplicated(fight), ]
#fight <- fight[duplicated(fight), ]

# You need to pick the (a)-[b]->(c)
# Say it is (fighter)-[fought]->(fighter)

# Then you need a property from each.


