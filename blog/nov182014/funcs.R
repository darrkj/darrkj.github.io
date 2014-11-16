

recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- plyr::rbind.fill(dList[[(i * 2) - 1]], dList[[i * 2]])
    j <- j + 1
  }
  # In case length was odd, just add last set to the end.
  if (floor(len) != len) {
    data[[j]] <- dList[[len * 2]]
  }
  # Less data to store on the stack, tail call optimization would be nice here.
  rm(dList, len, j)
  # Recursive call.
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}


# Function to add a wrapper around a HTTP GET request to return the 
# text html frm a given URL
get <- function(site) {
  strsplit(as.character(GET(site)), '\n')[[1]]
}

initDF <- function(name, rows) {
  # String which start the data frame istantiation.
  init <- "df <- data.frame("
  for (i in name) {
    init <- paste(init, i, " = rep(NA, ", rows, "), ", sep = "")
  }
  init <- substr(init, 1, nchar(init)-2)
  init <- paste(init, ")", sep = "")
  eval(parse(text = init))
  return(df)
}


add_nodes <- function(df, label, id, props = NULL, verbose = FALSE, na.act = F) {
  nodes <- list()
  # cannot have name of NA value
  miss <- !is.na(df[, id])
  uniq <- unique(df[miss, c(id, props), drop = FALSE])
  
  for (i in 1:nrow(uniq)) {
    n <- createNode(graph, label, name = uniq[i, id])
    for (j in props) {
      eval(parse(text = paste0('updateProp(n, ',  j, ' = uniq[i, "', j, '"])')))
    }
    if (verbose) cat(i, 'Created node: ', n[[1]], '\n')
    nodes[[uniq[i, id]]] <- n
  }
  nodes
}


add_rels <- function(df, from, f_name, to, t_name, how, props = NULL, verbose = T) {
  rels <- list()
  df <- df[, c(f_name, t_name, props)]
  #df <- df[complete.cases(df), ]
  for ( i in 1:nrow(df) ) {
    x1 <- from[[df[i, f_name]]]
    x2 <- to[[df[i, t_name]]]
    n <- createRel(x1, how, x2)
    for (j in props) {
      eval(parse(text = paste0('updateProp(n, ',  j, ' = df[i, "', j, '"])')))
    }
    if (verbose) cat(i, 'Created rel: ', names(x1), how, names(x2), '\n')
    rels[[as.character(i)]] <- n
  }
}


inter <- function(a, b) {
  idx <- order(c(seq_along(a), seq_along(b)))
  unlist(c(a,b))[idx]
}


maybe <- function(exp, opt = NA, ...) {
  val <- tryCatch(exp, error = function(e) opt, ...)
  if(length(val) < 1) val <- NA
  val
}



params <- function(act, pred, cutoff = 0.5) {
  pred <- ifelse(pred > cutoff, 1, 0)
  tp <- sum(act & pred)
  tn <- sum(!act & !pred)
  fp <- sum(!act & pred)
  fn <- sum(act & !pred)
  return(list(tp = tp, fp = fp, fn = fn, tn = tn))
}

roc <- function(act, pred, gran = 0.1) {
  s1 <- rep(NA, (1/gran) + 1)
  s2 <- rep(NA, (1/gran) + 1)
  j <- 1
  for (i in seq(0, 1, gran)) {
    x <- params(act, pred, cutoff = i)
    s1[j] <- (x$tp/(x$tp + x$fn))
    s2[j] <- (x$fp/(x$fp + x$tn))
    j <- j + 1
  }
  return(cbind(s2, s1))
}


d3plot <- function(network, h = 300, w = 700, ...) {
  library(igraph)
  library(d3Network)
  # Create temporary html file
  htmlFile <- tempfile(fileext=".html")
  
  if(is.igraph(network)) network <- igraph_2_d3(network)
  # Write d3 network viz to html file
  d3SimpleNetwork(network, height = h, width = w, file = htmlFile, ...)
  
  # (code to write some content to the file)
  rstudio::viewer(htmlFile)
}



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
