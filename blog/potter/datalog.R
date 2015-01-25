
# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(igraph)
library(RNeo4j)
library(magrittr)
library(rlist)
library(pryr)

# Create connection to graph database
graph <- startGraph("http://localhost:7474/db/data/")

# Partially apllied function to pre-specify domain and which db connection
createVert <- partial(createNode, graph = graph, domain = 'potter')


# Functions to create nodes of type character and location in db
. %>% createVert(c('potter', 'character'), name = .) -> character
. %>% createVert(c('potter', 'location'), name = .) -> location




# These are all of the people in the first movie
characters <- c('Harry Potter', 'Ron Weasley', 'Hermione Granger', 
                'Neville Longbottom', 'Rubeus Hagrid', 'Professor Dumbledore', 
                'Professor McGonagall', 'Petunia Dursley', 'Vernon Dursley', 
                'Dudley Dursley', 'Draco Malfoy', 'Professor Quirrell', 
                'Professor Snape', 'Filch', 'Mrs. Norris', 'Professor Sprout', 
                'Professor Flitwick', 'Professor Binns', 'Madam Hooch', 
                'Lily Potter', 'James Potter')

# Add these to the database.
characters %>% lapply(character) %>% `attr<-`('class', 'nodeList') -> chars



# Add them.
c('Hogwarts', 'England') %>% lapply(location) %>% 
  `attr<-`('class', 'nodeList') -> locs



# What about things with properties.
stuff <- function(...) {
  x <- list(createVert(c('potter', 'misc'), ...))
  attr(x, 'class') <- 'nodeList'
  x
}

misc <- stuff(name = 'car', type = 'dog', age = 3)






. %>% eval(subs(quote())) -> ev

findNode <- function(x) {
  for (i in getNeo()) {
    tmp <- list.filter(eval(parse(text = i)), x %in% name)[[1]]
    if(length(tmp) > 0) return(tmp)
  }
  NULL
}


findNode('Harry Potter')

#

edge <- function(what, rels, ...) {
  lapply(rels, function(x) createRel(what, x[[1]], x[[2]], domain = 'potter', ...))
}


# Harry 
knows(harry, list(list('knows', lily, how = 'son'), 
                  list('knows', james, how = 'son'), 
                  list('knows', petunia, how = 'nephew'), 
                  list('knows', vernon, how = 'nephew'), 
                  list('knows', dudley, how = 'cousin')))

# Lily
knows(lily, list(list('knows', harry, how = 'mother'), 
                 list('knows', petunia, how = 'sister'),
                 list('knows', james, how = 'wife'), 
                 list('knows', vernon, how = 'sister_in_law'), 
                 list('knows', dudley, how = 'aunt')))

# Petunia
knows(petunia, list(list('knows', harry, how = 'aunt'), 
                    list('knows', lily, how = 'sister'),
                    list('knows', vernon, how = 'wife'), 
                    list('knows', james, how = 'sister_in_law'),
                    list('knows', dudley, how = 'mother')))

# Dudley
knows(dudley, list(list('knows', harry, how = 'cousin'), 
                   list('knows', lily, how = 'nephew'),
                   list('knows', vernon, how = 'son'), 
                   list('knows', james, how = 'nephew'),
                   list('knows', harry, how = 'cousin')))

# Vernon
knows(vernon, list(list('knows', harry, how = 'uncle'), 
                   list('knows', lily, how = 'brother_in_law'),
                   list('knows', petunia, how = 'wife'), 
                   list('knows', james, how = 'brother_in_law'),
                   list('knows', dudley, how = 'father')))

# James
knows(james, list(list('knows', harry, how = 'father'), 
                  list('knows', lily, how = 'husband'),
                  list('knows', petunia, how = 'brother_in_law'), 
                  list('knows', vernon, how = 'brother_in_law'),
                  list('knows', dudley, how = 'uncle')))


# Ron
knows(ron, list(list('knows', harry, how = 'friend'), 
                list('knows', hermoine, how = 'friend'),
                list('knows', petunia, how = ), 
                list('knows', vernon, how = ),
                list('knows', dudley, how = )))

# Hermoine
knows(james, list(list('knows', harry, how = ), 
                  list('knows', lily, how = ),
                  list('knows', petunia, how = ), 
                  list('knows', vernon, how = ),
                  list('knows', dudley, how = )))

clearDom <- function(dom) {
  cypher(graph, paste0("match (a)-[r]->(b) where r.domain = '", dom, "' delete r;"))
  cypher(graph, paste0("match a where a.domain = '", dom, "' delete a;"))
}



clearDom('test')
