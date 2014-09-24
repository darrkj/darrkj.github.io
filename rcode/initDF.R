
#' Initialize data frame 
#'
#' This function will initialize a data.frame without
#' the verbose statements normally needed.
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return data.frame
#' 
#' @param name names of columns
#' @param row how man rows in the data frame
#' 
#' @keywords tools
#' @export
#'

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