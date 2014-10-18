# Load packages needed in this function.
library(XML)
library(lubridate)
# This is useful for imported data.
options(stringsAsFactors = FALSE)


mojo <- function(date) {
  # Source where data comes from.  
  api <- "http://www.boxofficemojo.com/daily/chart/?view=1day&sortdate="
  # Create string of HTML source of data for current day.
  site <- paste(api, date, "&p=.htm", sep = "")
  # Read data from that site.
  mList <- readHTMLTable(site)[[10]]
  # Strip out the data that I want, no headers.
  mov <- mList[-c(1, 2), c(3, 5, 11)]
  # This is the last row.
  mov <- mov[mov[, 1] != ">Yr >Mo \r\n>Wk \r\n>>Next Day", ]
  # Give more useful names
  names(mov) <- c("name", "daily", "day")
  # Capture dates.
  mov$date <- date
  
  # Add weekday variable
  mov$weekday <- wday(mov$date, label = TRUE)

  # Clean the characters out of the fields that need to be numbers.
  mov$daily <- as.numeric(gsub("[$,+%]", "", mov$daily, fixed = F))
  mov$day   <- as.numeric(gsub("[$,+%]", "", mov$day, fixed = F))
  
  mov
}

