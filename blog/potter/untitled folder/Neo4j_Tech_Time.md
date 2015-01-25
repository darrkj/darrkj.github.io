Neo4j
========================================================
author: Kenny Darrell
date: December 5, 2014

What is Neo4j?
========================================================



It is a Graph Database

Thats great what the hell does that mean!


Defs Slide
========================================================

A little background

What is a Database? 
A database is an organized collection of data. The data is typically organized to model aspects of reality in a way that supports processes requiring information. For example, modelling the availability of rooms in hotels in a way that supports finding a hotel with vacancies.

- Bullet 1
- Bullet 2
- Bullet 3


http://en.wikipedia.org/wiki/Database

People usually conotate this with RDBMS

A relational database management system (RDBMS) is a database management system (DBMS) that is based on the relational model as invented by E. F. Codd, of IBM's San Jose Research Laboratory. Many popular databases currently in use are based on the relational database model.

http://en.wikipedia.org/wiki/Relational_database_management_system

relational database
http://en.wikipedia.org/wiki/Relational_database
A relational database is a digital database whose organization is based on the relational model of data, as proposed by E.F. Codd in 1970. This model organizes data into one or more tables (or "relations") of rows and columns, with a unique key for each row. Generally, each entity type described in a database has its own table, the rows representing instances of that entity and the columns representing the attribute values describing each instance. Because each row in a table has its own unique key, rows in other tables that are related to it can be linked to it by storing the original row's unique key as an attribute of the secondary row (where it is known as a "foreign key"). Codd showed that data relationships of arbitrary complexity can be represented using this simple set of concepts.

Oracle, MySQL, Microsoft SQL Server, PostgreSQL and IBM DB2.[3]

There are many other kinds though

—————
http://en.wikipedia.org/wiki/Hierarchical_database_model
A hierarchical database model is a data model in which the data is organized into a tree-like structure. The data is stored as records which are connected to one another throughlinks. A record is a collection of fields, with each field containing only one value. The entity type of a record defines which fields the record contains.

http://en.wikipedia.org/wiki/Network_model
The network model is a database model conceived as a flexible way of representing objects and their relationships. Its distinguishing feature is that the schema, viewed as a graph in which object types are nodes and relationship types are arcs, is not restricted to being a hierarchy or lattice.

http://en.wikipedia.org/wiki/Object_database
An object database (also object-oriented database management system) is a database management system in which information is represented in the form of objects as used in object-oriented programming. Object databases are different fromrelational databases which are table-oriented. Object-relational databases are a hybrid of both approaches.


http://en.wikipedia.org/wiki/SQL
SQL (i/ˈɛs kjuː ˈɛl/,[4] or i/ˈsiːkwəl/;[5] Structured Query Language[6][7][8][9]) is a special-purpose programming language designed for managing data held in a relational database management system (RDBMS), or for stream processing in a relational data stream management system (RDSMS).


http://en.wikipedia.org/wiki/NoSQL
A NoSQL (often interpreted as Not Only SQL[1][2][unreliable source?]) database provides a mechanism for storage and retrieval of data that is modeled in means other than the tabular relations used in relational databases. Motivations for this approach include simplicity of design, horizontal scaling and finer control over availability. The data structures used by NoSQL databases (e.g. key-value, graph, or document) differ from those used in relational databases, making some operations faster in NoSQL and some faster in relational databases. The particular suitability of a given NoSQL database depends on the problem it must solve.




A document-oriented database is a computer program designed for storing, retrieving, and managing document-oriented information, also known as semi-structured data. Document-oriented databases are one of the main categories of NoSQL databases and the popularity of the term "document-oriented database" (or "document store") has grown[1]with the use of the term NoSQL itself. In contrast to relational databases and their notions of"Tables" and "Relations" (between Tables), these systems are designed around an abstract notion of a "Document”.

MongoDB and CouchDB


Key-value stores[edit]

Key-value (KV) stores use the associative array (also known as a map or dictionary) as their fundamental data model. In this model, data is represented as a collection of key-value pairs, such that each possible key appears at most once in the collection.[11][12]

Riak, redis, Big Table (Google)

A column of a distributed data store is a NoSQL object of the lowest level in a keyspace. It is a tuple (a key-value pair) consisting of three elements:[1]

Unique name: Used to reference the column
Value: The content of the column. It can have different types, like AsciiType, LongType, TimeUUIDType, UTF8Type among others.
Timestamp: The system timestamp used to determine the valid content.

Accumulo, Cassandra, Druid, HBase, Vertica


In computing, a graph database is a database that uses graph structures with nodes, edges, and properties to represent and store data. A graph database is any storage systemthat provides index-free adjacency.[1] This means that every element contains a direct pointer to its adjacent elements and no index lookups are necessary. General graph databases that can store any graph are distinct from specialized graph databases such as triplestores and network databases.
Graph: Allegro, Neo4J, InfiniteGraph, OrientDB, Virtuoso, Stardog


http://en.wikipedia.org/wiki/Time_series_database
A time series database (TSDB) is a software system that is optimized for handling time series data, arrays of numbers indexed by time (a datetime or a datetime range). In some fields these time series are called profiles, curves, or traces. A time series of stock prices might be called a price curve. A time series of energy consumption might be called a load profile. A log of temperature values over time might be called a temperature trace.

Druid
Geras
InfluxDB
KairosDB
KDB+
OneTick
OpenTSDB
SiteWhere
TempoDB
Treasure Data
Informix TimeSeries


That is all good, but what does it mean. It means that the world of data is becoming a more complicated and complex place. An oracle database can no longer serve all of the data needs that one may have. One thing pushing this is the large amount android/iphone and web apps. People/companies making these app may want to add a new feature to there app, collecting a new data point. In the RDBMS world this ma require them to change there schema to allow for efficient access. They may do this every week. Data migrations take time, they may only get further and further behind. Who has heard a client slow to make a change to a database are mention that the database is fragile and giving us access could bring it down. This is because they have lapsed on migrations and will never catch up.

We can gain a lot by using one of these newer systems. We need to pick the right one for the job though which is becoming a bigger and bigger challenge. They are also changing quickly so don’t get used to something. Do we lose anything. In some cases we lose acid transactions and we may move to a different portion of the cap triangle. (http://ofirm.files.wordpress.com/2013/01/scalability-cap-theorem1.png?w=584&h=452, http://maxivak.com/wp-content/uploads/2011/07/media_httpfarm5static_mevIk.png

Who cares right.


There are lots of graph databases.  http://en.wikipedia.org/wiki/Graph_database
The are still coming out. Neo4j is one of the more mature though, and it has some commercial backing. The dust has still not settled though.

http://neo4j.com/
http://neo4j.com/download/
http://neo4j.com/download-thanks/?edition=community&flavour=unix&_ga=1.117291649.605033748.1413577085

Need a driver, http://neo4j.com/contrib/


resr api, curl
http://www.hacksparrow.com/neo4j-tutorial-rest-api.html
http://neo4j.com/blog/the-neo4j-rest-server-part1-get-it-going/

or use cypher
http://neo4j.com/docs/stable/
Which looks and fells LIKE sql


There are also other unmanaged extensions like gremlin


Give a quick intro to graphene
create the harry potter data




Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](Neo4j_Tech_Time-figure/unnamed-chunk-2-1.png) 
