

x <- list(f = list(5), c(1, 2))

x[[2]]
x[[2]][[4]]


maybe <- function(expr) {
  value <- try(expr, silent = TRUE)
  
  if ( class(value) == "try-error" ) value <- NA
  
  value
}





# code from 

maybe <- function(exp, opt = NA, ...) {
  val <- tryCatch(exp, error = function(e) opt, ...)
  if(length(val) < 1) val <- NA
  val
}


x[[2]][[4]]

maybe(x[[2]][[4]])
maybe(x[[2]])














tryCatch(1, finally = print("Hello"))

e <- simpleError("test error")
## Not run: 
stop(e)
tryCatch(stop(e), finally = print("Hello"))
tryCatch(stop("fred"), finally = print("Hello"))

