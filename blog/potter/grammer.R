# Grammer for networks

# A person who works at place probably knows other people who work at said place

# therefor

# (person)-[works_at]->(company)<-[works_at]-(person())

# The pattern [works_at]->(company)<-[works_at]
# Should be transferable to some pattern of person to person.
# Can we also learn these patter

library(RNeo4j)
library(labyrinth)


clearDomain('potter')

graph = startGraph("http://localhost:7474/db/data/")


# We should create some characters from the Harry potter movies.

# Wrapper to create characters.
createVert <- function(n) {
  createNode(graph, .label = c('potter', 'character'), name = n, domain = 'potter')
}


harry      <- createVert('Harry Potter')
ron        <- createVert('Ron Weasley')
hermione   <- createVert('Hermione Granger')
neville    <- createVert('Neville Longbottom')
rubeus     <- createVert('Rubeus Hagrid')
dumbledore <- createVert('Professor Dumbledore')
mcgonagall <- createVert('Professor McGonagall')
petunia    <- createVert('Petunia Dursley')
vernon     <- createVert('Vernon Dursley')
dudley     <- createVert('Dudley Dursley')
draco      <- createVert('Draco Malfoy')
quirrell   <- createVert('Professor Quirrell')
snape      <- createVert('Professor Snape')
filch      <- createVert('Filch')
norris     <- createVert('Mrs. Norris')
sprout     <- createVert('Professor Sprout')
flitwick   <- createVert('Professor Flitwick')
binns      <- createVert('Professor Binns')
hooch      <- createVert('Madam Hooch')
lily       <- createVert('Lily Potter')
james      <- createVert('James Potter')


# An easy rule is to denote non-directional and rotate the edge.


createRel(harry, 'friend_of', ron, domain = 'potter')
createRel(harry, 'friend_of', hermione, domain = 'potter')
createRel(ron, 'friend_of', hermione, domain = 'potter')
createRel(ron, 'friend_of', harry, domain = 'potter')
createRel(hermione, 'friend_of', harry, domain = 'potter')

x <- cypher(graph, 'match (n)-[r:`friend_of`]->(m) where n.domain = "potter" return ID(n), ID(m);')

# We can now check that the reverse exists.

y <- x[, c(2, 1)]

names(x) <- names(y) <- c('V1', 'V2')

z <- rbind(x, y)

miss <- y[(!duplicated(z))[(nrow(y)+1):nrow(z)], ]

a <- getSingleNode(graph, 'START n=node(189247) RETURN n;')
b <- getSingleNode(graph, 'START n=node(189246) RETURN n;')

createRel(a, 'friend_of', b)





clearDomain('potter')


###


# One clear case of this is closing triangles.


harry      <- createVert('Harry Potter')
ron        <- createVert('Ron Weasley')
hermione   <- createVert('Hermione Granger')
neville    <- createVert('Neville Longbottom')
rubeus     <- createVert('Rubeus Hagrid')
dumbledore <- createVert('Professor Dumbledore')
mcgonagall <- createVert('Professor McGonagall')
petunia    <- createVert('Petunia Dursley')
vernon     <- createVert('Vernon Dursley')
dudley     <- createVert('Dudley Dursley')
draco      <- createVert('Draco Malfoy')
quirrell   <- createVert('Professor Quirrell')
snape      <- createVert('Professor Snape')
filch      <- createVert('Filch')
norris     <- createVert('Mrs. Norris')
sprout     <- createVert('Professor Sprout')
flitwick   <- createVert('Professor Flitwick')
binns      <- createVert('Professor Binns')
hooch      <- createVert('Madam Hooch')
lily       <- createVert('Lily Potter')
james      <- createVert('James Potter')

#
