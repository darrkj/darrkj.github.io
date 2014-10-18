
# Can use a calander heat map, stream chart and bump graph


# Pull in code I use a lot.
devtools::source_url('https://raw.githubusercontent.com/darrkj/darrkj.github.io/master/rcode/import.R')
import('recurBind')
import('calendarHeat')
import('html')

date <- as.Date("2014-05-01")


range <- 0:150
day <- list()

for (i in range) {
  day[[as.character(i)]] <- mojo(date + i)
  print(i)
}


mov <- recurBind(day)[[1]]


mg <- data.frame(date = date + 0:150, rem = 0)

name <- unique(mov[mov$day == 1, ]$name)

daily <- list()

for (i in name) {
  tmp <- mov[mov$name == i, c('name', 'daily', 'date')]
  tmp <- merge(tmp, mg, by ='date', all.y = TRUE)
  tmp$name <- i
  tmp$daily <- ifelse(is.na(tmp$daily), 0, tmp$daily)
  tmp$rem <- NULL
  daily[[i]] <- tmp
}

daily <- recurBind(daily)[[1]]


save(daily, file = 'daily.rda')


