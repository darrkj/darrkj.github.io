library(igraph)
options(stringsAsFactors = FALSE)


load('ig.RData')
load('info.RData')

info <- unlist(info)


info <- gsub('<link href=\"/images/favicon.ico\" type=\"image/x-icon\" rel=\"icon\" /><link href=\"/images/favicon.ico\" type=\"image/x-icon\" rel=\"shortcut icon\" /><meta name=\"keywords\" content=\"', '', info, fixed = T)
info <- gsub('\" /><!-- G+ -->', '', info, fixed = T)
info <- gsub('social graph, movies, social network, moviegalaxies, ', '', info, fixed = T)


inf <- sapply(strsplit(info, '\" /><meta name=\"description\"'), `[`, 1)
inf <- inf[-5]

#names(inf) <- gsub(',', '', names(inf))


inf <- gsub("&#039;", "'", inf)
inf <- gsub("amp;", "", inf, fixed = T)

#x <- strsplit(inf, ', ')

xx <- data.frame(x = 'a', y = 'b', z = 1)

for (i in seq(length(inf))) {
  tmp <- inf[i]
  tmp <- gsub(names(inf)[i], ',', tmp)
  tmp <- strsplit(tmp, ', ')
  if (length(tmp[[1]]) > 0) {
    xx <- rbind(xx, data.frame(x = names(inf)[i], y = tmp[[1]], 
                              z = 1:length(tmp[[1]])))
  }
  print(i)
}


xx <- xx[-1, ]
names(xx) <- c('movie', 'item', 'n')



yy <- xx[xx$item != ',', ]


type <- c('Drama', 'Thriller', 'Comedy', 'Crime', 'Action', 'Romance', 'Mystery', 
  'Adventure', 'Sci-Fi', 'Fantasy', 'Horror', 'Biography', 'Family', 'Musical',
  'History', 'War', 'Animation', 'Sport', 'Music', 'Western', 'N/A') 
zz <- yy[!yy$item %in% type, ]

year <- as.character(1900:2020)

zz <- zz[!zz$item %in% year, ]


head(rev(sort(table(zz$item))), 30)
table(yy$n)
yy[yy$n == 1, ]
