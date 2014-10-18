stat <- station[station != '']

stat <- stat[order(stat)]
x <- data.frame(tmp = stat)

l <- length(stat)
ss <- matrix(rep(0, l * l), nrow = l)

rownames(ss) <- stat
colnames(ss) <- stat

for (i in stat) {
  tmp <- bike[bike$inn == i, ]
  tmp <- tmp[tmp$inn != tmp$out, ]$out
  tmp <- as.data.frame(table(tmp))
  t <- merge(x, tmp, all.x = T)
  t$Freq <- ifelse(is.na(t$Freq), 0, t$Freq)
  ss[, i] <- t$Freq / sum(t$Freq)
  print(i)
}

image(ss)

av <- (ss + t(ss)) / 2

rr <- av[322:1, ]

image(sqrt(sqrt(rr)))

#s1 <- ss
#s2 <- t(ss)

#rownames(s1) <- NULL
#rownames(s2) <- NULL

#xy <- ss
xy <- ifelse(ss > .05, ss, 0)

#rr <- xy[322:1, ]

image(sqrt(sqrt(xy)))


########


av <- (ss + t(ss)) / 2

rr <- av[322:1, ]
# Ward Hierarchical Clustering
d <- dist(rr, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")





#################################



library(igraph)
tt <- graph.adjacency(rr)

tt <- graph.data.frame(unique(bike))




d3net <- graph.data.frame(unique(bike)[1:100,])
import('d3plot')
import('igraph_2_d3')
d3plot(d3net)


