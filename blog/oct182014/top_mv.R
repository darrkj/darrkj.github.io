
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

rm(date, day, i, range)

# Need to pull in calander heat map here


library(dplyr)

mov %>% 
  group_by(date) %>% 
  summarise(sum = sum(daily)) ->
  daily



names(daily) <- c('Date', 'value')
calheat(ts(daily))


# This looks about like what I expected

# What movies were good.

# Lets remove movies that we dont have there open

mg <- data.frame(date = date + 0:150, rem = 0)

name <- unique(mov[mov$Day == 1, ]$name)

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


daily %>%
  group_by(name) %>%
  summarise(sum = sum(daily)) %>%
  arrange(desc(sum)) %>%
  slice(c(1:10)) ->
  top_mv
  


mv <- top_mv$name


#create_data_file <- function(st, file, d1 = min(bike_stream$date), 
#                             d2 = min(bike_stream$date) + 500000) {
  



  dd <- daily[daily$name %in% mv, c('name', 'daily', 'date')]
  names(dd) <- c('key', 'value', 'date')
  dd$value <- dd$value / 1000000


dd <- dd[order(dd$key, dd$date),]
  #dd <- dd[dd$date >= d1, ]
  #dd <- dd[dd$date <= d2, ]
  
  # file 'www/data2.csv'
  write.csv(dd, file = 'daily.csv', row.names = FALSE)
  
#}


stream_plot('mv.csv')

# There is teo much noise here

# New movies come out on Friday 
mov[mov$Day == 1 & mov$name %in% mv,]$weekday






mg <- data.frame(date = date + 1:147, w = rep(1:21, each = 7))

#name <- unique(mov[mov$Day == 1, ]$name)

weekly <- list()

for (i in mv) {
  tmp <- mov[mov$name == i, c('name', 'daily', 'date')]
  tmp <- merge(tmp, mg, by ='date', all.y = TRUE)
  tmp$name <- i
  tmp$daily <- ifelse(is.na(tmp$daily), 0, tmp$daily)
  weekly[[i]] <- tmp
}

weekly <- recurBind(weekly)[[1]]


weekly %>%
  group_by(name, w) %>%
  summarise(sum = sum(daily)) ->
  w2


weekly %>%
  group_by(name, w) %>%
  summarise(date = min(date)) ->
  w3
  
w4 <- merge(w2,w3)

w4 <- w4[order(w4$name, w4$date), c('name', 'sum', 'date')]
#w3 <- w3[, c('name', 'sum', 'w')]



#w3$w <- NULL
names(w4) <- c('key', 'value', 'date')
#dd$value <- (dd$value / max(dd$value)) / 10


#dd <- dd[order(dd$key, dd$date),]
#dd <- dd[dd$date >= d1, ]
#dd <- dd[dd$date <= d2, ]

# file 'www/data2.csv'
write.csv(w4, file = 'weekly.csv', row.names = FALSE)

#}


stream_plot('weekly.csv')


https://public.tableausoftware.com/views/movie/Sheet1?:embed=y&:display_count=no
<script type='text/javascript' src='https://public.tableausoftware.com/javascripts/api/viz_v1.js'></script><div class='tableauPlaceholder' style='width: 699px; height: 458px;'><noscript><a href='#'><img alt='Sheet 1 ' src='https:&#47;&#47;public.tableausoftware.com&#47;static&#47;images&#47;mo&#47;movie&#47;Sheet1&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz' width='699' height='458' style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableausoftware.com%2F' /> <param name='site_root' value='' /><param name='name' value='movie&#47;Sheet1' /><param name='tabs' value='no' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableausoftware.com&#47;static&#47;images&#47;mo&#47;movie&#47;Sheet1&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /></object></div><div style='width:699px;height:22px;padding:0px 10px 0px 0px;color:black;font:normal 8pt verdana,helvetica,arial,sans-serif;'><div style='float:right; padding-right:8px;'><a href='http://www.tableausoftware.com/public/about-tableau-products?ref=https://public.tableausoftware.com/views/movie/Sheet1' target='_blank'>Learn About Tableau</a></div></div>
# Now try a bunp chart
