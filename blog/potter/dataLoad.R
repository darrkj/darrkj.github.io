
# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(igraph)
library(RNeo4j)


graph = startGraph("http://localhost:7474/db/data/")



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

#

edge <- function(what, rels, ...) {
  lapply(rels, function(x) createRel(what, x[[1]], x[[2]], domain = 'potter', ...))
}


# Harry 
edge(harry, list(list('knows', lily, how = 'son'), 
                  list('knows', james, how = 'son'), 
                  list('knows', petunia, how = 'nephew'), 
                  list('knows', vernon, how = 'nephew'), 
                  list('knows', dudley, how = 'cousin')))

# Lily
edge(lily, list(list('knows', harry, how = 'mother'), 
                 list('knows', petunia, how = 'sister'),
                 list('knows', james, how = 'wife'), 
                 list('knows', vernon, how = 'sister_in_law'), 
                 list('knows', dudley, how = 'aunt')))

# Petunia
edge(petunia, list(list('knows', harry, how = 'aunt'), 
                    list('knows', lily, how = 'sister'),
                    list('knows', vernon, how = 'wife'), 
                    list('knows', james, how = 'sister_in_law'),
                    list('knows', dudley, how = 'mother')))

# Dudley
edge(dudley, list(list('knows', harry, how = 'cousin'), 
                   list('knows', lily, how = 'nephew'),
                   list('knows', vernon, how = 'son'), 
                   list('knows', james, how = 'nephew'),
                   list('knows', harry, how = 'cousin')))

# Vernon
edge(vernon, list(list('knows', harry, how = 'uncle'), 
                   list('knows', lily, how = 'brother_in_law'),
                   list('knows', petunia, how = 'wife'), 
                   list('knows', james, how = 'brother_in_law'),
                   list('knows', dudley, how = 'father')))

# James
edge(james, list(list('knows', harry, how = 'father'), 
                  list('knows', lily, how = 'husband'),
                  list('knows', petunia, how = 'brother_in_law'), 
                  list('knows', vernon, how = 'brother_in_law'),
                  list('knows', dudley, how = 'uncle')))


# Ron
edge(ron, list(list('knows', harry, how = 'friend'), 
                list('knows', hermione, how = 'friend')))

# Hermoine
edge(hermione, list(list('knows', ron, how = 'friend'), 
                  list('knows', harry, how = 'friend')))


#
