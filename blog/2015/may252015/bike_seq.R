options(stringsAsFactors = FALSE)
library(dplyr)
library(lubridate)
library(ggplot2)
library(MCL) # http://www.micans.org/mcl/


'http://www.capitalbikeshare.com/data/stations/bikeStations.xml' %>%
  XML::xmlToList() -> geo

# Transform it into a more usable structure.
geo <- plyr::ldply(geo[-length(geo)], function(x) data.frame(t(x)))
geo$`.id` <- NULL





. %>%
  paste0('bike/', .) %>%
  read.csv() %>% 
  select(sdate = Start.date, edate = End.date, 
         sterm = Start.station, eterm = End.station, 
         bike = Bike, type = Type) -> get_bike_data


b <- list()
for (i in list.files('./bike')) {
  b[[i]] <- get_bike_data(i)
  print(i)
}

bike <- as.data.frame(bind_rows(b))

# Read original file
bike <- read.csv('2014-Q2-Trips-History-Data.csv') 
bike %>%   select(sdate = Start.date, edate = End.date, 
                  sterm = Start.Station, eterm = End.Station, 
                  bike = Bike, type = Type) -> bike


# There is only 351 but have 526 here
term <- unique(c(bike$sterm, bike$eterm))
term <- term[!is.na(term)]
term <- sort(term)

#############
# Cleaning

rmv <- paste0(' (', unlist(geo$terminalName), ')')

for (i in rmv) {
  bike$sterm <- gsub(i, '', bike$sterm, fixed = T)
  bike$eterm <- gsub(i, '', bike$eterm, fixed = T)
  print(i)
}

# This takes it down to 387
term <- unique(c(bike$sterm, bike$eterm))
term <- term[!is.na(term)]
term <- sort(term)

"Pentagon City Metro / 12th & S Hayes St"

pent <- c("12th & Hayes St", 
          "12th & Hayes St /  Pentagon City Metro",
          "Pentagon City Metro / 12th & Hayes St",
          "Pentagon City Metro / 12th & S Hayes St" )

bike[bike$sterm %in% pent, ]$sterm <- "Pentagon City Metro / 12th & S Hayes St"
bike[bike$eterm %in% pent, ]$eterm <- "Pentagon City Metro / 12th & S Hayes St"

# These are not in term
unlist(geo$name[!unlist(geo$name) %in% term])
c("Wisconsin Ave & Newark St NW", "Lee Hwy & N Kirkwood Rd",     
  "19th & G St NW",               "Army Navy Dr & S Nash St",
  "11th & S St NW")

######################

#'2014-Q2-Trips-History-Data.csv' %>% get_bike_data -> bike


term <- unique(c(bike$sterm, bike$eterm))
term <- term[!is.na(term)]


t_mat <- matrix(0, ncol = length(term), nrow = length(term))
b <- bike[bike$type == 'Registered', ]
for (i in 1:length(term)) {
  tmp <- b[b$sterm == term[i], ]
  for (j in 1:length(term)) {
    t_mat[i, j] <- nrow(tmp[tmp$eterm == term[j], ])
  }
  print(i)
}






clus <- mcl(t_mat, addLoops = F, max.iter = 1000)
#

station <- data.frame(name = as.character(term), 
                      clus_id = clus$Cluster)

#


# Clean the data.
geo %>% 
  select(name, lat, long) %>%
  # Clean up odd list structures and characters
  transform(name = unlist(name), lat = as.numeric(unlist(lat)), 
            long = as.numeric(unlist(long))) %>%
  inner_join(station, by = 'name') %>%
  na.omit() -> stations
%>% filter(!clus_id %in% c(15, 16, 13, 11, 3)) -> stations 


ggplot(stations, aes(long, lat)) + geom_point(aes(colour = factor(clus_id)))


#
