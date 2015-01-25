library("RNeo4j")
library("labyrinth")

graph <- startGraph("http://localhost:7474/db/data/")


data <- data.frame(Origin = c("SFO", "AUS", "MCI"),
                   FlightNum = c(1, 2, 3),
                   Destination = c("PDX", "MCI", "LGA"))

query = "
MERGE (origin:Airport {name:{origin_name}, domain:'test'})
MERGE (destination:Airport {name:{dest_name}, domain:'test'})
CREATE (origin)<-[:ORIGIN {domain:'test'}]-(:Flight {number:{flight_num}})-[:DESTINATION {domain:'test'}]->(destination)
"

t = newTransaction(graph)

for (i in 1:nrow(data)) {
  
  appendCypher(t, query, 
               origin_name = data[i, ]$Origin, 
               dest_name = data[i, ]$Dest, 
               flight_num = data[i, ]$FlightNum)
}

commit(t)

cypher(graph, "MATCH (o:Airport)<-[:ORIGIN]-(f:Flight)-[:DESTINATION]->(d:Airport)
               RETURN o.name, f.number, d.name")
