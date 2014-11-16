library(RNeo4j)

graph = startGraph("http://localhost:7474/db/data/")


q <- 'MATCH (a)-[r:fought]->(b)-[s:fought]->(a)
      WHERE r.result = "Win" and r.date = s.date
      RETURN a.name as name, a.id as id, 
             a.weight as weight, a.height as height, a.class as class, 
             a.birth as birth, a.stance as stance, a.reach as reach,
             a.style as style, r.result as result,
             r.dec as dec, r.date as date, r.exp as exp, s.exp as oexp,
             b.id as oid, b.weight as oweight, b.height as oheight,
             b.class as oclass, b.birth as obirth, b.stance as ostance, 
             b.reach as oreach, b.style as ostyle
      UNION ALL
      MATCH (a)-[r:fought]->(b)-[s:fought]->(a)
      WHERE r.result = "Loss" and r.date = s.date
      RETURN b.name as name, b.id as id, 
             b.weight as weight, b.height as height, b.class as class, 
             b.birth as birth, b.stance as stance, b.reach as reach,
             b.style as style, r.result as result,
             r.dec as dec, r.date as date, s.exp as exp, r.exp as oexp,
             a.id as oid, a.weight as oweight, a.height as oheight,
             a.class as oclass, a.birth as obirth, a.stance as ostance, 
             a.reach as oreach, a.style as ostyle;'

f <- unique(cypher(graph, q))


# People usually do not win on there first fight

sum(f$exp == 1)
sum(f$oexp == 1)

sum(f$exp == 2)
sum(f$oexp == 2)

sum(f$exp == 3)
sum(f$oexp == 3)

sum(f$exp == 4)
sum(f$oexp == 4)
#
x1 <- as.data.frame(table(f$exp))
x2 <- as.data.frame(table(f$oexp))

names(x2)[2] <- 'Freq2'
x3 <- merge(x1, x2, all.x = T, all.y = T)



mean(f$exp - f$oexp)

#
f$weight <- as.numeric(f$weight)
f$oweight <- as.numeric(f$oweight)

f$weight <- f$weight - f$oweight 
f$oweight <- NULL

# Weight does bot seem to help much.
mean(f[f$result == 'Win', ]$weight, na.rm = T)
mean(f[f$result == 'Loss', ]$weight, na.rm = T)

#
f$height <- as.numeric(f$height)
f$oheight <- as.numeric(f$oheight)

f$height <- f$height - f$oheight 
f$oheight <- NULL

# Weight does bot seem to help much.
mean(f[f$result == 'Win', ]$height, na.rm = T)
mean(f[f$result == 'Loss', ]$height, na.rm = T)

f$class <- NULL
f$oclass <- NULL
f$dec <- NULL
f$weight <- NULL
f$height <- NULL
f$stance <- NULL
f$ostance <- NULL


#

q <- 'MATCH (a)-[r:fought]->(b)
      WHERE r.result = "Win"
      RETURN a.id as id, r.date as date, b.id as oid;'


grTab <- function(gr) {
  data.frame(         id = vertex.attributes(gr)$name,
                      closeness = centralization.closeness(gr)$res,
                      betweeness = betweenness(gr),
                      eigenvector = centralization.evcent(gr)$vector,
                      hub = hub.score(gr)$vector,
                      auth = authority.score(gr)$vector,
                      page = page.rank(gr)$vector)
}

library(igraph)

library(devtools)
source_url('https://raw.githubusercontent.com/darrkj/darrkj.github.io/master/rcode/import.R')
import('igraph_2_d3')
import('d3plot')

library(lubridate)



f <- unique(cypher(graph, q))

f$date <- ymd(f$date)
f <- f[order(f$date), ]

time <- sort(unique(f$date))
g <- graph.data.frame(f[f$date < time[10], c(3, 1)])

d3plot(g)

plot(g)

###############################


f <- unique(cypher(graph, q))

f$date <- ymd(f$date)
f <- f[order(f$date), ]

hh <- list()
for (i in 2:length(time)) {
  
  g <- graph.data.frame(f[f$date < time[i], c(3, 1)])
  y <- x <- cbind(grTab(g), date = time[i])
  names(x) <- c("id", "cl", "bet", "eig", "hub", "auth", "page", "date") 
  names(y) <- c("oid", "ocl", "obet", "oeig", "ohub", "oauth", "opage", "date") 
  
  h <- f[f$date == time[i], ]
  h <- merge(h, x, by = c('id', 'date'), all.x = T)
  h <- merge(h, y, by = c('oid', 'date'), all.x = T)
  
  hh[[as.character(i)]] <- h
  print(i)
}

h <- recurBind(hh)[[1]]
h <- h[order(h$date), ]
head(h)
pageRank <- h
#

page <- h[, c('page', 'opage')]
page <- page[complete.cases(page), ]
sum(page[, 1] < page[, 2])
sum(page[, 1] > page[, 2])


> sum(is.na(h$page) & !is.na(h$opage))
[1] 1343
> sum(!is.na(h$page) & is.na(h$opage))


