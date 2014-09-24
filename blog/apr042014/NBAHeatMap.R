library(manipulate)
library(lubridate)
library(zoo)
library(plyr)
library(ggplot2)
library(XML)
options(stringsAsFactors = FALSE)



seasonify <- function(y, test = FALSE) {
  teams <- teams()
  day <- mdy('10-05-2012')
  year(day) <- y - 1
  quit <- TRUE
  season <- pull_day(day)
  cnt <- 1
  
  while (quit) {
    season <- rbind(season, pull_day(day))
    day <- day + days(1)
    if (as.Date(day) == today() | month(day) == 7) quit <- FALSE
    if (test & !is.null(season)) if(nrow(season) > 25) quit <- FALSE
    
    if (cnt == 4) {cat('='); cnt <- 0}  else cnt <- cnt + 1
  }
  
  if (y < 1960) teams[teams$symbol == "BAL", ]$symbol = "BLB"
  
  th <- teams[, c('symbol', 'ref')]
  names(th) <- c('hsym', 'home')
  
  ta <- teams[, c('symbol', 'ref')]
  names(ta) <- c('asym', 'away')
  
  season <- merge(season, th, by = 'home')#, all.x = TRUE)
  season <- merge(season, ta, by = 'away')#, all.x = TRUE)
  
  season$guid <- paste(season$hsym, season$asym, substr(season$date, 1, 4), 
                       substr(season$date, 6, 7), substr(season$date, 9, 10), sep = '')
  
  season$lk <- paste(substr(season$date, 1, 4), substr(season$date, 6, 7), 
                     substr(season$date, 9, 10), 0, season$hsym, sep = '')
  
  # Reorder by date games were played and return
  season[order(season$date), ]
}


pull_day <- function(day) {
  # oct 29 1947 is as far back as it goes
  # Date to collect data, ex: mdy('01-01-2004')
  site <- readHTMLTable(
    paste('http://www.basketball-reference.com/boxscores/index.cgi?month=', 
          month(day), '&day=', day(day), '&year=', year(day), sep = ''))
  
  if (length(site) > 0) {
    xx <- lapply(site, nrow)
    xx <- as.vector(unlist(lapply(xx, function(x) if(x == 2) T else F)))
    gameList <- data.frame(date = day, away = 'dd', as = 5, home = 'ff', hs = 4)
    y <- site[xx]
    for (i in y[names(y) == 'NULL']) {
      xx <- data.frame(date = day, away = i$V1[1], as = i$V2[1], 
                       home = i$V1[2], hs = i$V2[2])
      gameList <- rbind(gameList, xx)
    }
    gameList <- gameList[-1, ]
    gameList$as <- as.integer(gameList$as)
    gameList$hs <- as.integer(gameList$hs)
    gameList$season <- year(gameList$date)
    gameList$season <- ifelse(month(gameList$date) > 9, 
                              gameList$season + 1, gameList$season)
    return(gameList)
  }
}


teams <- function() {
  x <- c('WSC', 'Washington Capitols', 'Washington', 'Capitols',
         'BAL', 'Baltimore Bullets', 'Baltimore', 'Bullets',
         'CHS', 'Chicago Stags', 'Chicago', 'Stags',
         'NYK', 'New York Knicks', 'New York', 'Knicks',
         'PHW', 'Philadelphia Warriors', 'Philadelphia', 'Warriors',
         'BOS', 'Boston Celtics', 'Boston', 'Celtics',
         'PRO', 'Providence Steam Rollers', 'Providence', 'Steam Rollers',
         'STB', 'St. Louis Bombers', 'St. Louis', 'Bombers',
         'INJ', 'Indianapolis Jets', 'Indianapolis', 'Jets',
         'FTW', 'Fort Wayne Pistons', 'Fort Wayne', 'Pistons',
         'ROC', 'Rochester Royals', 'Rochester', 'Royals',
         'MNL', 'Minneapolis Lakers', 'Minneapolis', 'Lakers',
         'INO', 'Indianapolis Olympians', 'Indianapolis', 'Olympians',
         'WAT', 'Waterloo Hawks', 'Waterloo', 'Hawks',
         'AND', 'Anderson Packers', 'Anderson', 'Packers',
         'SHE', 'Sheboygan Red Skins', 'Sheboygan', 'Red Skins',
         'SYR', 'Syracuse Nationals', 'Syracuse', 'Nationals',
         'DEN', 'Denver Nuggets', 'Denver', 'Nuggets',
         'TRI', 'Tri-Cities Blackhawks', 'Tri-Cities', 'Blackhawks',
         'MLH', 'Milwaukee Hawks', 'Milwaukee', 'Hawks',
         'STL', 'St. Louis Hawks', 'St. Louis', 'Hawks',
         'CIN', 'Cincinnati Royals', 'Cincinnati', 'Royals',
         'DET', 'Detroit Pistons', 'Detroit', 'Pistons',
         'LAL', 'Los Angeles Lakers', 'Los Angeles', 'Lakers',
         'CHP', 'Chicago Packers', 'Chicago', 'Packers',
         'CHZ', 'Chicago Zephyrs', 'Chicago', 'Zephyrs',
         'SFW', 'San Francisco Warriors', 'San Francisco', 'Warriors',
         'PHI', 'Philadelphia 76ers', 'Philadelphia', '76ers',
         'CHI', 'Chicago Bulls', 'Chicago', 'Bulls',
         'SDR', 'San Diego Rockets', 'San Diego', 'Rockets',
         'SEA', 'Seattle SuperSonics', 'Seattle', 'SuperSonics',
         'ATL', 'Atlanta Hawks', 'Atlanta', 'Hawks',
         'MIL', 'Milwaukee Bucks', 'Milwaukee', 'Bucks',
         'PHO', 'Phoenix Suns', 'Phoenix', 'Suns',
         'CLE', 'Cleveland Cavaliers', 'Cleveland', 'Cavaliers',
         'POR', 'Portland Trail Blazers', 'Portland Trail', 'Blazers',
         'BUF', 'Buffalo Braves', 'Buffalo', 'Braves',
         'GSW', 'Golden State Warriors', 'Golden State', 'Warriors',
         'HOU', 'Houston Rockets', 'Houston', 'Rockets',
         'KCO', 'Kansas City-Omaha Kings', 'Kansas City-Omaha', 'Kings',
         'CAP', 'Capital Bullets', 'Capital', 'Bullets',
         'WSB', 'Washington Bullets', 'Washington', 'Bullets',
         'NOJ', 'New Orleans Jazz', 'New Orleans', 'Jazz',
         'KCK', 'Kansas City Kings', 'Kansas City', 'Kings',
         'NYN', 'New York Nets', 'New York', 'Nets',
         'IND', 'Indiana Pacers', 'Indiana', 'Pacers',
         'SAS', 'San Antonio Spurs', 'San Antonio', 'Spurs',
         'NJN', 'New Jersey Nets', 'New Jersey', 'Nets',
         'SDC', 'San Diego Clippers', 'San Diego', 'Clippers',
         'UTA', 'Utah Jazz', 'Utah', 'Jazz',
         'DAL', 'Dallas Mavericks', 'Dallas', 'Mavericks',
         'LAC', 'Los Angeles Clippers', 'Los Angeles', 'Clippers',
         'SAC', 'Sacramento Kings', 'Sacramento', 'Kings',
         'CHH', 'Charlotte Hornets', 'Charlotte', 'Hornets',
         'MIA', 'Miami Heat', 'Miami', 'Heat',
         'ORL', 'Orlando Magic', 'Orlando', 'Magic',
         'MIN', 'Minnesota Timberwolves', 'Minnesota', 'Timberwolves',
         'TOR', 'Toronto Raptors', 'Toronto', 'Raptors',
         'VAN', 'Vancouver Grizzlies', 'Vancouver', 'Grizzlies',
         'WAS', 'Washington Wizards', 'Washington', 'Wizards',
         'MEM', 'Memphis Grizzlies', 'Memphis', 'Grizzlies',
         'NOH', 'New Orleans Hornets', 'New Orleans', 'Hornets',
         'CHA', 'Charlotte Bobcats', 'Charlotte', 'Bobcats',
         'NOK', 'New Orleans/Oklahoma City Hornets', 'New Orleans/Oklahoma City', 'Hornets',
         'OKC', 'Oklahoma City Thunder', 'Oklahoma City', 'Thunder',
         'BRK', 'Brooklyn Nets', 'Brooklyn', 'Nets',
         'NOP', 'New Orleans Pelicans', 'New Orleans', 'Pelicans')
  xx <- matrix(x, ncol = 4, nrow = 67, byrow = T)
  xx <- as.data.frame(xx)
  names(xx) <- c('symbol', 'ref', 'loc', 'name')
  xx
}


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
  ts$season <- max(ts$year)
  ggplot(ts, aes(monthweek, weekdayf, fill = value)) +
    geom_tile(colour = "white") + 
    facet_grid(season ~ monthf) +
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
  Date <- as.Date("1945-04-20") + 0:30000
  ts <- as.data.frame(Date)
  ts$year <- year(ts$Date)
  ts$month <- month(ts$Date)
  ts$monthf <- month.factor(ts$month)
  ts$monthf <- factor(ts$monthf,levels(ts$monthf)[c(9:12, 1:8)])
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
