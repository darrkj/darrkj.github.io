igraph_2_d3 <- function(igr) {
  library(igraph)
  Source <- c()
  Target <- c()
  
  x <- V(igr)
  for (i in seq(length(E(igr)))) {
    e <- get.edge(igr, i)
    
    s <- if(is.null(x[e[1]]$name)) x[e[1]] else x[e[1]]$name
    t <- if(is.null(x[e[2]]$name)) x[e[2]] else x[e[2]]$name
    Source <- c(Source, s)
    Target <- c(Target, t)
  }
  
  data.frame(Source, Target)
}
