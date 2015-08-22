library(rvest)

get_all_urls <- function() {
  'http://www.gtfs-data-exchange.com/agencies' %>% 
    html() %>% html_nodes('a') %>% 
    html_attr('href') %>% 
    grep('agency', ., value = T) %>% 
    paste0('http://www.gtfs-data-exchange.com', .) -> locations

  # Last one is check
  locations[-length(locations)]
}

# This function will get the url for the zip file.
. %>% html() %>% html_nodes('a') %>% 
  html_attr('href') %>%
  grep('.zip', ., value = T) %>% 
  grep('agency', ., value = T) %>%
  paste0('http://www.gtfs-data-exchange.com', .) -> get_zip_url

# Get a list of all of the cities
each_cities_page <- get_all_urls()


# Loop over each site to get the location of the zip file.
all_zips <- list()
for (i in each_cities_page) {
  all_zips[[i]] <- get_zip_url(i)
  print(i)
}



# Download zip file
download_gtfs <- function(url, loc) {
  download.file(url = url, destfile = paste(loc, basename(url), sep = '/'))

  name <- gsub('http://www.gtfs-data-exchange.com/agency', loc, url)
  name <- gsub('/latest.zip', '', name)
  # Unzip downloaded file
  url %>% basename %>% paste(loc, ., sep = '/') %>% unzip(., exdir = name)
  unlink(paste(loc, basename(url), sep = '/'))
}


# Create a folder to store the data.
dir.create('gtfs_data')

download_gtfs(all_zips[[1]], 'gtfs_data/')

