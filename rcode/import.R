import <- function(l = c()) {
  l <- paste('http://darrkj.github.io/home/rcode/', l, '.R', sep = '')
  for ( i in l ) {
    devtools::source_url(i)
  }
}



# import(c('recurBind', 'd3plot'))
