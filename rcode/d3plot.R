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
