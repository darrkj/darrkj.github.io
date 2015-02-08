###############################################################################
#
#       breakPoints
#
###############################################################################
#' Order a data frame by its columns.
#'
#' This function takes a value n which is the number of regions
#' that you want to break a time series into.  If you want 4 regions
#' you will get a result of 3 value that create this cutoff.  This seems
#' odd to not give it 3, as that is the number of breakpoints.  This is the
#' convention from the paper though.
#'
#' @param n The number of pieces to break a time series into.
#' @keywords timeseries, sax
#' @author Kenny Darrell <darrell@@datamininglab.com>
#' @export
#' @references TODO: breakPoints: addRefToPaper
#' @examples
#' breakPoints(4)
#' # [1] -0.6744898  0.0000000  0.6744898

breakPoints <- function(n) {
  if (n < 2) stop("Input must be greater than one!")
  if (n > 26) stop("There are only 26 letters!")
  # TODO: breakPoints: Catch n not an integer.
  # Create uniform splits from 0 to 1, take all but first.
  x <- seq(0, 1, length.out = (n + 1))[-1]
  # Find the point which that has the above in area undeer CDF.
  return(qnorm(x)[1:(n - 1)])
}

###############################################################################
#
#       paa
#
###############################################################################
#' Piecewise Aggregate Approximation
#'
#' This function condenses a span of time in a time series into 
#' the mean over that time.  The result is a time series that 
#' contains only n observations.  
#'
#' @param sts The time series to condense.
#' @param n The number of piecewise sections to generate.
#' @param p Number of point in one aggregation, opposes n
#' @keywords timeseries, sax
#' @export
#' @examples
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14, 18) - 1
#' x <- xts(rnorm(6), x.Date)
#' paa(x, 2)

paa <- function(sts, n = 10, p = NA) {
  # Create a sequence of 1 to length of series.
  l <- seq_along(sts)
  # This will find the number of obs in order to have n groups. 
  grp <- length(l) / n
  # This creates an index for the ts of which group the obs belongs to.
  fac <- floor((l / grp) - .0001)
  # Allow mechanism for creating a period like week, 3 day.
  if (!is.na(p)) fac = rep(0:(length(sts) / p), each = p)[l]
  # Split the original ts into these groups.
  pa <- split(sts, fac)
  # Find the mean for each group.
  return(unlist(lapply(pa, mean)))
}

###############################################################################
#
#       token
#
###############################################################################
#' Function to tokenize the PAA of a time series.
#'
#' Tokenize the values in the piecewise agregate approximation
#' of a time series.  The result is a sequence.  
#'
#' @param sts The time series to condense.
#' @param bp The number of \code{\link{breakpoints}}, elements in your alpahbet.
#' @keywords timeseries, sax
#' @export
#' @examples
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14, 18) - 1
#' x <- xts(rnorm(6), x.Date)
#' y <- paa(x, 2)
#' token(y, 3)
 
token <- function(sts, bp = 3) {
  # Call to get the breakpoints.
  bp <- breakPoints(bp)
  # Turn to numeric vector, faster and easier to work with.
  sts <- as.numeric(sts)
  # Initialize the word to nothing.
  word <- NULL
  # Set first for values under first threshold.
  word[sts < bp[1]] <-  letters[1]
  # Loop throught the rest.
  for(i in seq(bp)) {
    # If they are greater than breakpoint replace.
    word[sts >= bp[i]] <- letters[i + 1]
  }
  return(word)
}


###############################################################################
#
#       sax
#
###############################################################################
#' sax
#'
#' This function condenses   
#'
#' @param ts The time series to condense.
#' @param bp The number of piecewise sections to generate.
#' @param n The size of the tokenizing alphebet.
#' @param plot fsdf
#' @keywords timeseries, sax
#' @export
#' @examples
#' x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14, 18) - 1
#' x <- xts(rnorm(6), x.Date)
#' paa(x, 2)

# TODO: paa: need to be able to give an inc like 7 days instead of want to groups.
sax <- function(ts, bp = 3, n = 10, plot = FALSE, p = NA) {
  # This allows for plots to work, kludge.
  index(ts) <- 1:length(ts)
  # Normalize the incoming time series.
  ts <- scale(ts)
  # Create the piecewise aggregates.
  x <- paa(ts, n, p)
  if (plot) {
    # Create plot of normalized time series.
    plot(ts) 
    points(rep(x, each = (length(ts) / n) + 1))[1:length(ts)]
    for(i in breakPoints(bp)) {
      lines(1:length(ts), rep(i, length(ts)))
    }
  }
  tokenTS <- token(x, bp)
  comment(tokenTS) <- as.character(bp)
  return(tokenTS)
}

# Lookup table.
lookUp <- function(x) {
  # Call to get the breakpoints.
  d <- breakPoints(x)
  # Initialize matrix of zeros.
  distM <- matrix(0, nrow = x, ncol = x)
  # Loop over each row and column.
  for(i in seq(x)) {
    for(j in seq(x)) {
      # Check to make sure its greater than one.
      if(abs(i - j) > 1) {
        # This comes from the paper for how it is created.
        distM[i, j] <- d[max(i, j) - 1] - d[min(i, j)]
      }
    }
  }
  return(distM)
}


distSAX <- function(x, y) {
  lx <- as.numeric(comment(x))
  ly <- as.numeric(comment(y))
  #if (lx != ly) stop("Time Series were not tokenized the same")
  xx <- NULL
  lk <- lookUp(lx)
  for(i in seq_along(x)) {
    a <- which(letters %in% x[i])
    b <- which(letters %in% y[i])
    xx <- c(xx, lk[a, b] ^ 2)
  }
  return(sqrt(sum(xx)))
}

distTS <- function(x, y, w = 5, s = 10) {
  xx <- sax(x, w, s)
  yy <- sax(y, w, s)
  v <- dist(xx, yy)
  v <- sqrt(length(x) / w) * v
  return(v)
}

# TODO: misc: add this code to an example
# x.Date <- as.Date("2003-02-01") + c(1, 3, 7, 9, 14) - 1
# x <- zoo(rnorm(5), x.Date)
# y <- xts(rnorm(5), x.Date)
# # Create a time series.
# x1 <- sin(seq(from = 1, to = 10, by= .01))
# y1 <- cos(seq(from = 1, to = 10, by= .01))
# x2 <- runif(length(x1))
# x3 <- 1:length(x1)
# x3 <- x3/max(x3) * 3
# 
# x <- zoo(x1+x2+x3)
# y <- zoo(y1+x2+x3)
# rm(x1, x2, x3, y1)
# x1 <- sax(x, 5, 10, plot=TRUE)
# a <- sax(x, 5, 10)
# b <- sax(y, 5, 10)
# dist(a, b)
# a
# b
# 
# 
# 
# l <- NULL
# for(i in seq(26)) {
#   l <- c(l, distTS(x, y, w=i+1, s=10))
# }
# 

# sts <- zoo(mydata$Open, mydata$Date)

# D <- function(ts1, ts2) {
#   sum(sqrt((ts1 - ts2) ^ 2))
# }
# 
# noGaps <- function(stream) {
#   stream[-1] - stream[1:(length(stream) - 1)]
# }
# 
# holidays <- list(Christmas=as.POSIXlt("2009-12-25"))



# This function generates a time series.
# It is pretty rough.
# genTS <- function(t) {
#   x <- rnorm(t)
#   z <- NULL
#   z[1] <- 0
#   for(i in seq(t)) {
#     j <- i + 1
#     z[j] <- x[j] + x[j - 1]
#   }
#   z <- as.zoo(z[1:t])
#   return(z) 
# }

#
#ts(1:10, start = 1, frequency = 1)
#ts(1:10, start = 1998, frequency = 1)


