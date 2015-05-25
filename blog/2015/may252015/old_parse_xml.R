get_locs <- function() {
  # Get data from web with locations
  'http://www.capitalbikeshare.com/data/stations/bikeStations.xml' %>%
    read_xml %>%
    xml_find_all('.//station') -> stations
  
  tags <- c('terminalName', 'lat', 'long')
  
  . %>% paste0('.//', .) %>% xml_find_one(j, .) %>% xml_text -> get_vals
  
  yy <- list()
  l <- 1
  for (j in stations) {
    yy[[l]] <- sapply(tags, get_vals) #%>% t %>% data.frame 
    l <- l + 1
  }
  
  yy %>% bind_rows %>% as.data.frame %>% 
    filter(terminalName != "8D OPS test") %>%
    mutate(lat = as.numeric(lat), long = as.numeric(long))
}
