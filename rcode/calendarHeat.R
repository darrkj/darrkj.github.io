
library(quantmod)
library(ggplot2)
library(plyr)
library(lubridate)

###############################################################################
#
#     cal.heatMap
#
###############################################################################
#' Calandar Heat Map
#' 
#' This function will take a vector of time values and create a 
#' calandar heat map.  This will have hot spots on days with more activity,
#' more observations appear as red and those with less in green.
#' 
#' @author Kenny Darrell
#' @return side effect, plot
#' @param data The time info of time data
#' @keywords graphics timeseries
#' @export
#' @examples
#' x.Date <- as.Date("2013-04-20") + rnorm(5000, 250, 100) - 1
#' cal.heatMap(x.Date)
#' x.Date <- as.Date("2013-04-20") + runif(5000, 0, 600)
#' cal.heatMap(x.Date)
#' 
#' # Example of using a list of dates, events
#' x.Date <- as.Date("2013-04-20") + runif(5000, 0, 600)
#' cal.heatMap(x.Date)
#' 
#' Example of using th evalue of a time series
#' z.Date <- as.Date("2013-04-20") + 1:500
#' a.Date <- zoo(runif(500, 0, 60), z.Date)
#' cal.heatMap(a.Date)

cal.heatMap <- function(x, ...) {
  UseMethod("cal.heatMap", x)
}

#' Date version of cal.heatMap
#' 
#' @author Kenny Darrell
#' @param data The time info of time data
#' @keywords graphics timeseries
#' @export

cal.heatMap.Date <- function(ts) {
  ts <- count.Date(ts)
  calheat(ts)
}

#' Date version of cal.heatMap
#' 
#' @author Kenny Darrell
#' @param data The time info of time data
#' @keywords graphics timeseries
#' @export

#cal.heatMap.zoo <- function(ts, col = names(ts)[2]) {
cal.heatMap.zoo <- function(ts, col = "x") {
  ts <- ts[, col] 
  ts <- as.data.frame(list(Date = time(ts), value = ts), 
                      row.names = 1:length(ts))
  calheat(ts)
}

cal.heatMap.data.frame <- function(ts, x) {
  ts <- data.frame(Date = as.Date(ts$date), value = ts[, x], 
                      row.names = 1:nrow(ts))
  calheat(ts)
}

calheat <- function(ts) {
  ts <- merge(allDates, ts, by = "Date", all.y = TRUE)
  ts$value <- as.numeric(ts$value)
  ggplot(ts, aes(monthweek, weekdayf, fill = value)) +
    geom_tile(colour = "white") + 
    facet_grid(year ~ monthf) +
    scale_fill_gradient(high = "#D61818", low = "#B5E384") +
    labs(title = "Time-Series Calendar Heatmap") +
    xlab("Week of Month") + 
    ylab("")
}


month.factor <- function(month) {
  factor(month, levels = as.character(1:12), ordered = TRUE,
         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}

weekday.factor <- function(weekday) {
  factor(weekday, levels = rev(1:7), ordered = TRUE,
         labels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
}

#' Consolidate list of dates
#' 
#' @author Kenny Darrell
#' @param data The time info of time data
#' @keywords graphics timeseries
#' @export

count.Date <- function(data) {
  if (class(data) != "Date") stop("Input must be of class Date!")
  ts <- as.data.frame(table(data), stringsAsFactors = FALSE)
  names(ts) <- c("Date", "value")
  ts$Date <- as.Date(ts$Date)
  return(ts)
}

fillout.ts <- function() {
  Date <- as.Date("1900-04-20") + 0:50000
  ts <- as.data.frame(Date)
  ts$year <- year(ts$Date)
  ts$month <- month(ts$Date)
  ts$monthf <- month.factor(ts$month)
  ts$weekday <- wday(ts$Date)
  ts$weekdayf <- weekday.factor(ts$weekday)
  ts$yearmonthf <- as.yearmon(factor(ts$Date))
  ts$week <- as.numeric(format(as.Date(ts$Date),"%W"))
  ts$week <- ifelse(ts$weekday == 1, ts$week + 1, ts$week)
  ts <- ddply(ts, .(yearmonthf), transform, monthweek = 1 + week - min(week))
  ts <- ts[complete.cases(ts), c("Date", "year", "weekdayf", 
                                 "monthf", "monthweek")]
  return(ts)
}

allDates <- fillout.ts()



# 
# t3 = Quandl("GOOG/NASDAQ_GOOG", type = "zoo")
# 
# manipulate(
#   cal.heatMap(t3, col = factor), 
#   factor = picker("Open", "High", "Low", "Last", "Close"))
# 
# 
# cal.heatMap(t3, col = factor)
# 
# 
# # Is there a way to make this same tye of plot for time data.
# 
# z <- Sys.time()           # the current date, as class "POSIXct"
# 
# zz <- z + -1000:1000
# 
# zzz <- zoo(runif(500, 0, 60), zz)
# cal.heatMap(a.Date)


