
# Need this to communicate, unless we want to curl everything over REST, not recommended
library(RNeo4j)


# Point to the server which we want to communicate
graph = startGraph("http://localhost:7474/db/data/")


# Create our first slide
slide1 <- createNode(graph, .label = 'techTime', name = 'Neo4j', author = 'Kenny Darrell',
                date = 'December 5, 2014', domain = 'techTime')

# This slide has a differnt setup, but we just give the attributes it does have.
slide2 <- createNode(graph, .label = 'techTime', name = 'What is Neo4j?', 
                text = "It is a Graph Database. That's great, but what the hell does that mean!",
                domain = 'techTime')


slide3 <- createNode(graph, .label = 'techTime', name = 'What is a Database', 
                text = 'A database is an organized collection of data. The data is typically organized to model aspects of reality in a way that supports processes requiring information.',
                domain = 'techTime')


slide4 <- createNode(graph, .label = 'techTime', name = 'Usually Means', 
                text = 'Most people take this to mean RDBMS or a relational database management system. This model organizes data into one or more tables (or "relations") of rows and columns, with a unique key for each row. Generally, each entity type described in a database has its own table, the rows representing instances of that entity and the columns representing the attribute values describing each instance. .........',
                domain = 'techTime')

slide5 <- createNode(graph, .label = 'techTime', name = 'SQL', 
                text = 'SQL, Structured Query Language, is a special-purpose programming language designed for managing data held in a relational database management system (RDBMS), or for stream processing in a relational data stream management system (RDSMS).',
                domain = 'techTime')

slide6 <- createNode(graph, .label = 'techTime', name = 'These is not the only options', 
                text = 'Oracle, MySQL, Microsoft SQL Server, PostgreSQL and IBM DB2.',
                domain = 'techTime')

# How do we add and image, just give it a new attribute. We do not need to worry about
# giving allother slides a null image.
slide7 <- createNode(graph, .label = 'techTime', name = 'other Kinds', 
                image = 'link', domain = 'techTime')

# This slide does not even have a name, which is okay. It does have two images though.
# Given a relation database this slide would most likely warrent a schema change. Images
# would no longer be nullable, but have there own table with a key to the slide table.
slide8 <- createNode(graph, .label = 'techTime', 
                image1 = 'link1', image2 = 'link2',
                text = 'These let us store data in almost in format',
                domain = 'techTime')


# Here is where we create the relationships.
createRel(slide1, 'Background',   slide2, domain = 'techTime')
createRel(slide2, 'Explanation',  slide3, domain = 'techTime')
createRel(slide3, 'In_Reality',   slide4, domain = 'techTime')
createRel(slide4, 'Reasoning',    slide5, domain = 'techTime')
createRel(slide5, 'So_What',      slide6, domain = 'techTime')
createRel(slide6, 'Options',      slide7, domain = 'techTime')
createRel(slide7, 'Implications', slide8, domain = 'techTime')

browse(graph)








clearDom <- function(dom) {
  cypher(graph, paste0("match (a)-[r]->(b) where r.domain = '", dom, "' delete r;"))
  cypher(graph, paste0("match a where a.domain = '", dom, "' delete a;"))
}



clearDom('senate')

