ops <- c('+', '-', '/', '*')


f <- Stack$new()


forth <- function() {
  options(warn = -1)
  input <- readline('korth > ')
  input <- unlist(strsplit(input, ' '))
  if (input == 'quit') {
    print("  Goodbye  ")
    options(warn=0)
    return(invisible(NULL))
  } else {
    for (i in input) {
      if(i %in% ops) {
        a <- as.numeric(f$pop())
        b <- as.numeric(f$pop())
        tmp <- do.call(i, list(a, b))
        f$push(tmp)
      } else {
        f$push(i)
      }  
    }
    print(f$pop()) # or pop
    forth()
  }
}



forth()
