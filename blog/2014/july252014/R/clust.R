# Try to see if I can cluster dists

# What about some form of online learning, show me the extremes and let 
# me classify them.


load('gameSAX.RData')
source('R/SAX.R')



# Error from last post using length - 1

len <- length(gameSAX)




#max <- max(gameDist)
#heatmap(gameDist[1:10, 1:10])

#x <- hclust(gameDist[1:10, 1:10])

y <- dist(matrix(rnorm(len * len), nrow = len, ncol = len))

cnt <- 0

for (i in 1:length(gameSAX)) {
  for (j in (i):length(gameSAX)) {
    if (i != j) {
      cnt <- cnt + 1
      y[cnt] <- distSAX(gameSAX[[i]], gameSAX[[j]])
    }
  }
  print(i)
}




rm(i, j)
rm(paa, distSAX, distTS, lookUp, sax, token, breakPoints)


save(y, file = 'dist.Rdata')
x <- hclust(y)



