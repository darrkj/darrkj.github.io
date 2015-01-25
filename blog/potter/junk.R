findNode <- function(x) {
  ll <- c()
  for (i in getNeo()) {
    ll <- c(ll, eval(parse(text = i)), .GlobalEnv)
  }
  print(length(getNeo()))
  list.filter(ll, x %in% name)
}



list.filter(ll, "Harry Potter" %in% name)


findNode('Harry Potter')






List of 21
$ :List of 2
..$ domain: chr "potter"
..$ name  : chr "Harry Potter"

List of 25
$ :List of 2
..$ domain: chr "potter"
..$ name  : chr "Harry Potter"
