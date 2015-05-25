get_locs <- function() {
  # Get data from web with locations
  g <- read_xml('http://www.capitalbikeshare.com/data/stations/bikeStations.xml')
  
  # Tags that I want from the XML
  tags <- c('name', 'terminalName', 'lastCommWithServer', 'lat', 'long',
            'installed', 'locked', 'installDate', 'removalDate', 'temporary',
            'public', 'nbBikes', 'nbEmptyDocks', 'latestUpdateTime')
  
  
  . %>% paste0('.//', .) %>% xml_find_one(j, .) %>% xml_text -> get_vals
  # . %>% sapply(get_vals) %>% t %>% data.frame -> datify
  # lapply(xml_find_all(g, './/station'), function(j) datify(tags)) %>%
  #   bind_rows %>%
  #   as.data.frame -> z
  
  yy <- list()
  l <- 1
  for (j in xml_find_all(g, './/station')) {
    xx <- sapply(tags, get_vals)
    yy[[l]] <- data.frame(t(xx))
    l <- l + 1
  }
  
  geo <- as.data.frame(bind_rows(yy))
  
  geo <- geo[-which(geo$terminalName == "8D OPS test"), ]
  geo$lat <- as.numeric(geo$lat)
  geo$long <- as.numeric(geo$long)
  geo
}
