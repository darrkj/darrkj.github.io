options(stringsAsFactors = FALSE)

library(devtools)
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/import.R')
import('html')

bike <- read.csv('2014-Q2-Trips-History-Data.csv')[, c(3, 6)]
names(bike) <- c('inn', 'out')


#n <- table(c(bike$inn, bike$out))


station <- unique(c(bike$inn, bike$out))


#m <- station[1:5]

m <- c('15th & P St NW', 
       'Jefferson Dr & 14th St SW', 
       'Columbus Circle / Union Station', 
       'Massachusetts Ave & Dupont Circle NW', 
       'Lincoln Memorial')


m <- c('16th & Harvard St NW',
       '14th & Harvard St NW ',
       'Massachusetts Ave & Dupont Circle NW',
       'Georgetown Harbor / 30th St NW',
       'C & O Canal & Wisconsin Ave NW')


m <- c('Wilson Blvd & Franklin Rd',
       'Clarendon Blvd & Pierce St',
       'Clarendon Metro / Wilson Blvd & N Highland St',
       'Court House Metro / Wilson Blvd & N Uhle St',
       'Rosslyn Metro / Wilson Blvd & Ft Myer Dr')

#m <- as.numeric(c(names(head(rev(sort(n)), 6))))

z <- bike[bike$inn %in% m & bike$out %in% m, ]


val <- table(z$inn, z$out)
source('app.R')


chord_plot(val)

#

load('bike_stream.rda')


create



#





