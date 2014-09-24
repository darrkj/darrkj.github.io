cleanOrg <- function(html) {
  # This denotes the rows we want.
  org <- grep('<li><a href=', html, fixed = T, value = T)
  org <- clean(org, c('<li><a href=\"', '</a>', '</li>\r'))
  org <- strsplit(org, '\">')
  if (length(org) > 0) data.frame(ID = unlist(lapply(org, `[`, 1)), 
                                  name = unlist(lapply(org, `[`, 2)))
}


cleanDet <- function(html) {
  dets <- strsplit(as.character(GET(html)), '\n')[[1]]
  
  data <- dets[head(grep('<table', dets), 1):tail(grep('</table', dets), 1)]
  tmp <- cleanOrg(grep('<li>', data, value = TRUE))
  
  data <- grep('<td>', data, value = T)
  data <- clean(data, c('    <td>', '</td>\r'))
  data <- grep('<label>', data, invert = T, value = T)
  list(tmp, data)
}

ID <- function(loc) {
  #'terrorist_organization_profile.asp?id=3544'
  num <- gregexpr('=', loc)[[1]]
  as.integer(substr(loc, num+1, nchar(loc)))
}

clean <- function(data, strings) {
  for (i in strings) { data <- gsub(i, '', data, fixed = T)}
  data
}

d3plot <- function(network, h = 300, w = 700) {
  # Create temporary html file
  htmlFile <- tempfile(fileext=".html")
  
  if(is.igraph(network)) network <- igraph_2_d3(network)
  # Write d3 network viz to html file
  d3SimpleNetwork(network, height = h, width = w, file = htmlFile)
  
  # (code to write some content to the file)
  rstudio::viewer(htmlFile)
}

igraph_2_d3 <- function(igr) {
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
