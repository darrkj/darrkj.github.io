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
  mov <- mList[, -c(1:2, 6:7)]
  # Need to figure out why these came through, or just always clean 
  # them out. I don't think they still appear, but it hurts nothing
  # to leave this logic in.
  mov <- mov[mov[, 1] != "Title (Click to View)", ]
  mov <- mov[mov[, 1] != ">Yr >Mo \r\n>Wk \r\n>>Next Day", ]
  mov <- mov[!is.na(mov[, 1]), ]
  
  # Capture dates.
  mov$date <- date
  # Give more useful names
  names(mov) <- c("name", "studio", "daily","Theaters", "Avg", "Gross", 
                  "Day", 'date')
  
  # Add weekday variable
  mov$weekday <- wday(mov$date, label = TRUE)
  
  # Function used to strip characters used for web visualization.
  mojoClean <- function(x) {
    return(as.numeric(gsub("[$,+%]", "", x, fixed = F)))  
  }
  
  # Clean the characters out of the fields that need to be numbers.
  mov$daily    <- mojoClean(mov$daily)
  mov$Theaters <- mojoClean(mov$Theaters)
  mov$Avg      <- mojoClean(mov$Avg)
  mov$Gross    <- mojoClean(mov$Gross)
  mov$Day      <- mojoClean(mov$Day)
  
  mov
}

