
#' Recursive row binding 
#'
#' Don't carry huge sets around,
#' has divide and conquer on memory usage, large set only appear
#' at the top level of the recursion. 
#' 
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @return data.frame
#' 
#' @param dList List of data frames
#' @param row how man rows in the data frame
#' 
#' @keywords tools
#' @export
#'

recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- rbind(dList[[(i * 2) - 1]], dList[[i * 2]])
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